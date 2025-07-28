library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(zoo)
library(ggthemes)
library(ggtext)
library(cowplot)
library(patchwork)
library(fuzzyjoin)
library(shellpipes)
rpcall("beta_plot.Rout beta_plot.R calibrate_inc.rds params.rda")
library(tidyverse)
library(macpan2)
loadEnvironments()
set.seed(2025)

start_date <- as.Date("2021-12-15") - offset0
last_date <-"2022-05-22"

calibrator <- rdsRead("calibrate.rds")

fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE)

fitted_data <- (fitted_data
        |> mutate(date = as.Date(start_date) + as.numeric(time) -1 )
        |> dplyr::filter(between(date, as.Date(start_date), as.Date(last_date)))
        |> dplyr::filter(matrix %in% c("date","beta_thing"))
)

beta_values <- fitted_data |>
  select(-any_of(c("row", "col"))) |>
  pivot_wider(names_from = matrix, values_from = value) |>
  group_by(date) |>
  mutate(across(everything(), ~ first(na.omit(.)), .names = "{.col}")) |>
  ungroup() |>
  distinct(date, .keep_all = TRUE) |>
  drop_na() |>
  select(c(date, beta_thing))

beta_values <- beta_values |>
        dplyr::filter(date >= as.Date("2021-12-15") & date <= as.Date("2022-05-22"))

beta_values <- beta_values %>%
  mutate(
    alert_level = case_when(
      date >= as.Date("2021-12-15") & date <= as.Date("2021-12-19") ~ "ALS-2\nK-12 Open",
      date >= as.Date("2021-12-20") & date <= as.Date("2021-12-24") ~ "ALS-2\nK-12 Closed",
      date >= as.Date("2021-12-25") & date <= as.Date("2022-01-03") ~ "ALS-3\nK-12 Closed",
      date >= as.Date("2022-01-04") & date <= as.Date("2022-01-25") ~ "ALS-4\nK-12 Closed",
      date >= as.Date("2022-01-26") & date <= as.Date("2022-02-07") ~ "ALS-4\nK-12 Open",
      date >= as.Date("2022-02-08") & date <= as.Date("2022-03-14") ~ "ALS-3^relax\nK-12 Open",
      date >= as.Date("2022-03-15") & date <= as.Date("2022-05-22") ~ "No-ALS\nK-12 Open",
      TRUE ~ NA_character_
    ),
    combined_alert = case_when(
      date >= as.Date("2021-12-20") & date <= as.Date("2022-01-25") ~ "K-12 Closed",
      date >= as.Date("2022-01-26") & date <= as.Date("2022-05-22") ~ "K-12 Open",
      TRUE ~ NA_character_
    )
  )

beta_summary <- beta_values %>%
  group_by(alert_level) %>%
  summarise(
    mean_value = mean(beta_thing, na.rm = TRUE),
    sd_value = sd(beta_thing, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(alert_level)) %>%
  mutate(type = "Specific")

beta_summary_combined <- beta_values %>%
  group_by(combined_alert) %>%
  summarise(
    mean_value = mean(beta_thing, na.rm = TRUE),
    sd_value = sd(beta_thing, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(combined_alert)) %>%
  rename(alert_level = combined_alert) %>%
  mutate(type = "Combined")

beta_summary_all <- bind_rows(beta_summary, beta_summary_combined)

kappa1 <- 1
kappa2 <- 0.91
kappa3 <- 0.3
gamma_i <- 1/7
gamma_a <- 1/10
mu <- 0.324
zeta <- 0.75
p1 <- 0.15
p2 <- 0.85
p3 <- 0

bracket_term <- (mu / gamma_i) + ((1 - mu) * zeta / gamma_a)
susceptibility_sum <- (p1 * kappa1) + (p2 * kappa2) + (p3 * kappa3)
constant_mult <- susceptibility_sum * bracket_term

calc_R0 <- function(beta) beta * constant_mult
calc_R0_sd <- function(beta, beta_sd) constant_mult * beta_sd

beta_summary_all <- beta_summary_all %>%
  mutate(
    R0_mean = calc_R0(mean_value),
    R0_sd = calc_R0_sd(mean_value, sd_value),
    k12_group = ifelse(alert_level %in% c("K-12 Closed", "K-12 Open"), "Overall", "Specific"),
    k12_status = case_when(
      grepl("Closed", alert_level) ~ "K-12 Closed",
      grepl("Open", alert_level) ~ "K-12 Open",
      TRUE ~ NA_character_
    )
  )

specific_data <- beta_summary_all %>%
  filter(k12_group == "Specific") %>%
  arrange(k12_status, desc(R0_mean)) %>%
  mutate(alert_level = factor(alert_level, levels = alert_level))

overall_data <- beta_summary_all %>%
  filter(k12_group == "Overall") %>%
  mutate(alert_level = factor(alert_level, levels = alert_level))

alert_colors <- c(
  "ALS-2\nK-12 Open" = "#66D1B566",
  "ALS-2\nK-12 Closed" = "#66D1B566",
  "ALS-3\nK-12 Closed" = "#87CEFA66",
  "ALS-4\nK-12 Closed" = "#FFD58066",
  "ALS-4\nK-12 Open" = "#FFD58066",
  "ALS-3^relax\nK-12 Open" = "#F7E2E299",
  "No-ALS\nK-12 Open" = "#D3D3D399",
  "K-12 Closed" = "red",
  "K-12 Open" = "navy"
)

# Use k12_status to determine label color
label_html <- purrr::map_chr(specific_data$alert_level, function(lvl) {
  status <- specific_data$k12_status[which(specific_data$alert_level == lvl)][1]
  col <- ifelse(status == "K-12 Closed", "red", "blue")
  pretty_label <- gsub("ALS-3\\^relax", "ALS-3<sup>relax</sup>", lvl)
  pretty_label <- gsub("\\n", "<br>", pretty_label)
  sprintf("<span style='color:%s;'>%s</span>", col, pretty_label)
})
names(label_html) <- specific_data$alert_level

plot_specific <- ggplot(specific_data, aes(x = alert_level, y = R0_mean, fill = alert_level)) +
  geom_col(color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = R0_mean - R0_sd, ymax = R0_mean + R0_sd), width = 0.15) +
  geom_text(aes(label = format(R0_mean, nsmall = 1, digits = 2)),hjust = -0.5, vjust = -0.5, size = 8)+
  scale_fill_manual(values = alert_colors) +
  scale_x_discrete(labels = label_html) +
  labs(title = expression("Estimated " * R[0] * " by Specific Alert Levels"),
       x = NULL, y = expression("Reproduction Number ("*R[0]*")")) +
  theme_minimal() +
  theme(
    axis.text.x = ggtext::element_markdown(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "none"
  )

plot_overall <- ggplot(overall_data, aes(x = alert_level, y = R0_mean, fill = alert_level)) +
  geom_col(color = "black", width = 0.5) +
  geom_errorbar(aes(ymin = R0_mean - R0_sd, ymax = R0_mean + R0_sd), width = 0.15) +
  geom_text(aes(label = format(R0_mean, nsmall = 1, digits = 2)),hjust = 0.5, vjust = -0.95, size = 8)+
  coord_cartesian(ylim = c(0, max(beta_summary_all$R0_mean + beta_summary_all$R0_sd) + 0.6))+
  scale_fill_manual(values = alert_colors) +
  labs(title = "Overall Periods (K-12 Closed vs Open)",x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 20, angle = 0, hjust = 0.5),
    axis.title.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "none",
    panel.border = element_blank(),
    plot.background = element_blank()
  )

final_plot <- cowplot::plot_grid(plot_specific, plot_overall, ncol = 1, rel_heights = c(3, 1))
print(final_plot)

#png("../figures/R0_errorplot.png", width = 5000, height = 2500, res = 300, bg = "white")
#final_plot

print(overall_data)
print(specific_data)

chrono_order <- c("ALS-2\nK-12 Closed", "ALS-3\nK-12 Closed", "ALS-4\nK-12 Closed"
	, "ALS-2\nK-12 Open", "ALS-4\nK-12 Open", "ALS-3^relax\nK-12 Open", "No-ALS\nK-12 Open"
)

spec_data <- (specific_data
	|> mutate(chrono = factor(alert_level,levels=chrono_order))
)


gg <- (ggplot(spec_data,aes(x=chrono,y=R0_mean))
	+ geom_point()
	+  geom_rect(data = NULL, aes(
    xmin = stage("ALS-2\nK-12 Closed", after_scale = xmin-0.5),
    xmax = stage("ALS-4\nK-12 Closed", after_scale = xmax+0.5),
    ymin = 1.37 - 1.96*0.0222,
    ymax = 1.37 + 1.96*0.0222
  ), fill = "red",alpha=0.05)
     +  geom_rect(data = NULL, aes(
    xmin = stage("ALS-2\nK-12 Closed", after_scale = xmin-0.5),
    xmax = stage("ALS-4\nK-12 Closed", after_scale = xmax+0.5),
    ymin = 1.37,
    ymax = 1.37
  ), color = "red")
   +  geom_rect(data = NULL, aes(
    xmin = stage("ALS-2\nK-12 Open", after_scale = xmin-0.5),
    xmax = stage("No-ALS\nK-12 Open", after_scale = xmax+0.5),
    ymin = 1.92 - 1.96*0.268,
    ymax = 1.92 + 1.96*0.268
  ), fill = "blue",alpha=0.05)
     +  geom_rect(data = NULL, aes(
    xmin = stage("ALS-2\nK-12 Open", after_scale = xmin-0.5),
    xmax = stage("No-ALS\nK-12 Open", after_scale = xmax+0.5),
    ymin = 1.92,
    ymax = 1.92
  ), color = "blue")

	+ geom_errorbar(aes(ymin=R0_mean - 1.96*R0_sd,ymax=R0_mean + 1.96*R0_sd))
  + labs(y = expression(""*R[0]*"(t)"))
  + theme_bw()

)

print(gg)


#dev.off()

