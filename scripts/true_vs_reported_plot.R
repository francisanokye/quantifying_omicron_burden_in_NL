library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(zoo)
library(forcats)
library(ggthemes)
library(cowplot)
library(patchwork)
library(shellpipes)
library(macpan2)
loadEnvironments()
set.seed(2025)

start_date <- as.Date("2021-12-15") - offset0
last_date <-"2022-05-22"

calibrator <- rdsRead("calibrate.rds")

# model simulation with calibrated parameters
fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE)

fitted_data <- (fitted_data
        |> mutate(date = as.Date(start_date) + as.numeric(time) -1 )
        |> dplyr::filter(between(date, as.Date(start_date), as.Date(last_date)))
        |> dplyr::filter(matrix %in% c("date","inc","conf.low","conf.high"))
)

# convert matrix values into columns
true_infections <- fitted_data |>
  select(-any_of(c("row", "col"))) |>
  pivot_wider(names_from = matrix, values_from = value) |>
  group_by(date) |>
  mutate(across(everything(), ~ first(na.omit(.)), .names = "{.col}")) |>
  ungroup() |>
  distinct(date, .keep_all = TRUE) |>
  drop_na() #|>
  #select(c(date, inc))

true_infections <- true_infections |>
        dplyr::filter(date >= as.Date("2021-12-15") & date <= as.Date("2022-05-22"))

# load reported data
reported_cases <- csvRead()
reported_cases$date <- as.Date(reported_cases$date, format = "%Y-%m-%d")

# drop all NAs
reported_cases <- reported_cases |>
        dplyr::filter(date >= as.Date("2021-12-15") & date <= as.Date("2022-05-22")) |>
	drop_na()

d1 <- reported_cases[c("date","cases")]
d1$type <- "reported"
colnames(d1) <- c("date","inc","type")

d2 <- true_infections[c("date","inc")]
d2$type <- "true_infections"
allcases <- rbind(d1,d2)

# data prep
allcases <- allcases %>%
  mutate(type = factor(type, levels = c("reported", "true_infections")))

combined_case_plot <- ggplot(allcases, aes(x = date)) +
  geom_rect(aes(xmin=ymd('2022-03-17'), xmax = ymd('2022-05-22'), ymin = 0, ymax = Inf), fill = adjustcolor("#F7E2E2", alpha = 0.03), alpha = 0.05) +
  geom_bar(data = true_infections, aes(y = inc), fill = "blue", stat = "identity", position = "stack",width = 0.25, alpha = 1) +
  geom_bar(data = filter(allcases, type == "reported"), aes(y = inc), fill = "darkgreen", stat = "identity",position = "stack", width = 0.25, alpha = 1) +
  geom_ribbon(data = true_infections, aes(x = date, ymin = conf.low, ymax = conf.high, fill = "gray"), alpha = 0.5,show.legend = FALSE)+
  geom_point(data = true_infections, aes(y = inc, color = "True Infections"),size = 1.0) +
  geom_line(data = true_infections, aes(y = inc, color = "True Infections"),linewidth = 0.5) +
  geom_point(data = filter(allcases, type == "reported"), aes(y = inc, color = "Reported Cases"),size = 1.0) +
  geom_line(data = filter(allcases, type == "reported"), aes(y = inc, color = "Reported Cases"),linewidth = 0.5) +
  scale_color_manual(name = NULL,values = c("Reported Cases" = "darkgreen", "True Infections" = "blue")) +
  labs(x = "Date (Dec 15, 2021 - May 22, 2022)",y = "Number of Cases",title = "Omicron Infections: Reported vrs Estimated Seroincidence") +
  geom_segment(aes(x = as.Date("2021-12-24"), y = 0, yend = Inf),linetype = "dashed",color = "gold4",linewidth = 1) +
  geom_segment(aes(x = as.Date("2022-01-08"), y = 0, yend = Inf),linetype = "dashed",color = "gold4",linewidth = 1) +
  geom_segment(aes(x = as.Date("2022-02-07"), y = 0, yend = Inf),linetype = "dashed",color = "gold4",linewidth = 1) +
  geom_segment(aes(x = as.Date("2022-03-14"), y = 0, yend = Inf),linetype = "solid",color = "black",linewidth = 1) +
  annotate("text", x = as.Date("2021-12-18"), y = 1200, label = "ALS-2", size = 5, angle = 90, hjust = 1) +
  annotate("text", x = as.Date("2021-12-28"), y = 1200, label = "ALS-3", size = 5, angle = 90, hjust = 1) +
  annotate("text", x = as.Date("2022-01-30"), y = 950, label = "ALS-4", size = 5, hjust = 1) +
  annotate("text", x = as.Date("2022-03-07"), y = 600, label = "Mod-ALS-3", size = 5, hjust = 1) +
  annotate("text", x = as.Date("2022-04-20"), y = 600, label = "No-ALS", size = 5, hjust = 1) +
  theme_clean() +
  theme(
    axis.text.x = element_text(),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.background = element_rect(color = NA),
    plot.background = element_blank()
  )

# cumulative cases for reported and true cases 
tot_obs <- allcases %>% filter(type == "reported") %>% summarise(total = sum(inc)) %>% pull(total)
tot_const <- allcases %>% filter(type == "true_infections") %>% summarise(total = sum(inc)) %>% pull(total)

# data for bar plot
const_final_totals <- data.frame(Group = c("Reported\nCases", "True\nInfections"),Total_Cases = c(tot_obs, tot_const))

# reorder the groups from smallet to largest
const_final_totals <- const_final_totals %>%
  mutate(Group = fct_reorder(Group, -Total_Cases))  

# inset plot
const_cum <- ggplot(const_final_totals, aes(x = Group, y = Total_Cases, fill = Group)) +
  geom_col(width = 0.6, alpha = 0.35, color = "black", show.legend = FALSE) +
  geom_text(
    aes(label = scales::comma(Total_Cases)),
    vjust = -0.5,
    size = 3
  ) +
  scale_fill_manual(
    values = c("Reported\nCases" = "darkgreen", "True\nInfections" = "blue")
  ) +
  labs(
    title = "Infections: True vs Reported",
    x = NULL,
    y = "Total Cases"
  ) +
  ylim(0, max(const_final_totals$Total_Cases) * 1.2) +  # leave space above bars
  theme_clean() +
  theme(
    plot.background = element_rect(fill = "white", color = "black"),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 9, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.95)
  )


# combine plots ---
final_combined_plot <- ggdraw() +
  draw_plot(combined_case_plot, 0, 0, 1, 1) +
  draw_plot(const_cum, 0.22, 0.66, 0.27, 0.28)

print(final_combined_plot)

png("../figures/reported_vs_true.png", width = 3000, height = 1800, res = 300, bg = "white", type = "cairo")
final_combined_plot
dev.off()
