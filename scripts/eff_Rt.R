library(dplyr)
library(lubridate)
library(ggplot2)
library(incidence) 
library(EpiEstim)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(zoo)
library(ggthemes)
library(cowplot)
library(patchwork)
library(fuzzyjoin)
library(shellpipes)
library(macpan2)
loadEnvironments()
set.seed(2025)

start_date <- as.Date("2021-12-15") - offset0
last_date <-"2022-05-22"

calibrator <- rdsRead("calibrate.rds")

sims = (calibrator
  |> mp_trajectory_sd(conf.int = TRUE, back_transform = TRUE)
  |> dplyr::filter(time >= offset0)
  |> dplyr::filter(matrix == c("inc"))
  |> mutate(date = seq.Date(from = as.Date("2021-12-15"), by = "1 day", length.out = n()))
  |> filter(date >= as.Date("2021-12-15") & date <= as.Date("2022-05-22"))
)

alert_values <- rep(c('ALS-2', 'ALS-3', 'ALS-4', 'Mod-ALS-3', 'No-ALS'),times = c(10, 10, 35, 35, 69))

sims <- (sims |>
  mutate(alert_level = alert_values)
)

# Estimate Effective Reproductive (Rt) and the average case-weighted Rt period ALS period
# Omicron's  serial interval distribution is 3.5 and that the SD is 2.4 using an estimate 

sims_inc <- (sims |>
  dplyr::filter(matrix == "inc")
)

incidence_df <- data.frame(date = sims_inc$date, I = sims_inc$value)

#incidence_df <- data.frame(date = sims$date, I = sims$inc)

# estimate Rt using EpiEstim
n_days <- length(incidence_df$date)

# uncommenting and using these time window estimates the Rt for each window separately
#t_start <- c( 2, 11, 21, 55, 90 )
#t_end   <- c(10, 20, 54, 89, 162)

t_start <- seq(2, n_days - 6)
t_end <- seq(8, n_days)
config <- make_config(list(
			   t_start = t_start,
			   t_end = t_end,
			   mean_si = 3.5,
                           std_si = 2.4
			 )
		     )

res <- estimate_R(incid = incidence_df, method = "parametric_si", config = config)

# intervention periods
interventions <- data.frame(
  period = c("ALS-2", "ALS-3", "ALS-4","Mod-ALS-3","No-ALS"),
  start = as.Date(c("2021-12-15", "2021-12-25", "2022-01-09","2022-02-08","2022-03-15")),
  end   = as.Date(c("2021-12-24", "2022-01-08", "2022-02-07","2022-03-14","2022-05-26"))
)

# map Rt estimates to actual calendar dates
Rt_df <- res$R %>%
  mutate(date = incidence_df$date[t_end])  

# merge incidence counts
Rt_df <- Rt_df %>%
  left_join(incidence_df, by = "date") %>%
  dplyr::rename(inc = I)

# assign Rt values to intervention periods using fuzzy join
Rt_df <- Rt_df %>%
  fuzzy_left_join(
    interventions,
    by = c("date" = "start", "date" = "end"),
    match_fun = list(`>=`, `<=`)
  ) %>%
  select(date,`Mean(R)`, inc, period)

# case-weighted average Rt + min/max per period
summary_Rt <- Rt_df %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  summarise(
    Rt_weighted = sum(`Mean(R)` * inc, na.rm = TRUE) / sum(inc, na.rm = TRUE),
    Rt_min = min(`Mean(R)`, na.rm = TRUE),
    Rt_max = max(`Mean(R)`, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_Rt)

alert_colors <- c("No-ALS" = "#D3D3D3", "ALS-2" = "#66D1B5", "ALS-3" = "#87CEFA", "Mod-ALS-3" = "#F7E2E2", "ALS-4" = "#FFD580")

Rt_df$period <- factor(Rt_df$period, levels = names(alert_colors))

Eff_Reprod <- (ggplot(Rt_df, aes(x = date, y = `Mean(R)`)) +
  geom_rect(aes(xmin=ymd('2022-03-14'), xmax = ymd('2022-05-22'), ymin = -Inf, ymax = Inf), 
            fill = adjustcolor("#D3D3D3"), alpha = 0.05) +
  geom_rect(aes(xmin=ymd('2021-12-21'), xmax = ymd('2021-12-24'), ymin = -Inf, ymax = Inf), 
            fill = adjustcolor("#66D1B5"), alpha = 0.05) +
  geom_rect(aes(xmin=ymd('2021-12-24'), xmax = ymd('2022-01-08'), ymin = -Inf, ymax = Inf), 
            fill = adjustcolor("#87CEFA"), alpha = 0.05) +
  geom_rect(aes(xmin=ymd('2022-02-07'), xmax = ymd('2022-03-14'), ymin = -Inf, ymax = Inf), 
            fill = adjustcolor("#F7E2E2"), alpha = 0.05) +
  geom_rect(aes(xmin=ymd('2022-01-08'), xmax = ymd('2022-02-07'), ymin = -Inf, ymax = Inf), 
            fill = adjustcolor("#FFD580"), alpha = 0.05) +
  geom_ribbon(data = Rt_df, aes(ymin = pmax(0, `Mean(R)` - 0.25 * sd(`Mean(R)`)),ymax = `Mean(R)` + 0.25 * sd(`Mean(R)`)), fill = "gray70", alpha = 0.75)+
  geom_line(lwd = 1.5, color = "blue") +
  scale_color_manual(values = alert_colors, guide = "none") +
  scale_x_date(date_breaks = "2 week",date_labels = "%b %d") +
  theme_clean() + 
  theme(axis.text.x = element_text(size = 12, angle = 0, hjust = 1, face = "bold"),
        axis.title.x = element_text(size = 12, color = "black", face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, color = "black", face = "bold"),
        legend.position = c(0.9,0.6),
        legend.text = element_text(hjust = 0.25),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.border = element_blank(),
        plot.background = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_hline(yintercept = 1, linetype = 6, lwd = 1.3, color = "darkslategrey") +
  geom_vline(xintercept = as.Date("2021-12-24"), colour = "gold4", linetype = 2, size = 1)  +
  geom_vline(xintercept = as.Date("2022-01-08"), colour = "gold4", linetype = 2, size = 1)  +
  geom_vline(xintercept = as.Date("2022-02-07"), colour = "gold4", linetype = 2, size = 1)  +
  geom_vline(xintercept = as.Date("2022-03-14"), colour = "black", linetype = 1, size = 1)  +
  annotate("text", x = as.Date("2021-12-18"), y = 1.4, label = expression(R[t[2]] == 1.38),size = 4,angle = 90, hjust = 1, color = "black")+
  annotate("text", x = as.Date("2022-01-02"), y = 1.2, label = expression(R[t[3]] == 1.06),size = 4,angle = 90, hjust = 1,color = "black", alpha = 1)+
  annotate("text", x = as.Date("2022-02-02"), y = 1.05, label = expression(R[t[4]]== 1.02),size = 4,angle = 0, hjust = 1, color = "black",alpha = 1)+
  annotate("text", x = as.Date("2022-03-06"), y = 1.2, label = expression(R[t[3][m]] == 1.09),size = 4,angle = 0, hjust = 1, color = "black", alpha = 1)+
  annotate("text", x = as.Date("2022-05-01"), y = 1.1, label = expression(R[t[0]] == 1.00),size = 4, hjust = 1, color = "black", alpha = 1)+
  annotate("text", x = as.Date("2022-03-02"), y = 1.50, label = "Pre-Cancellation of Public \nHealth Emergency Declaration",size = 4, hjust=1, color = "black")+
  annotate("text", x = as.Date("2022-05-18"), y = 1.50, label = "Post Cancellation of Public \nHealth Emergency Declaration",size = 4, hjust=1,color = "black")+
  ggtitle(label = expression("Estimated Time-varying " * R[t] * " Across Intervention Periods in NL", subtitle = "")) +
  labs(x = "Dates (Dec 15, 2021 -- May 26, 2022)", y =  expression("Effective Reproduction Number ("* R[t]* ")"))
)

print(Eff_Reprod)

png("../figures/eff_Rt.png", width = 2800, height = 1500, res = 300, bg = "white", type = "cairo")
Eff_Reprod
dev.off()

