

library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(zoo)
library(grid)
library(ggthemes)
library(cowplot)
library(patchwork)
library(fuzzyjoin)
library(shellpipes)
rpcall("beta_plot.Rout beta_plot.R calibrate.rds params.rda")
library(tidyverse)
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
        n|> dplyr::filter(matrix %in% c("date","beta_thing"))
)

# convert matrix values into columns
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
      date >= as.Date("2021-12-15") & date <= as.Date("2021-12-19") ~ "ALS-2\nNo",
      date >= as.Date("2021-12-20") & date <= as.Date("2021-12-24") ~ "ALS-2\nYes",
      date >= as.Date("2021-12-25") & date <= as.Date("2022-01-03") ~ "ALS-3\nYes",
      date >= as.Date("2022-01-04") & date <= as.Date("2022-01-25") ~ "ALS-4\nYes",
      date >= as.Date("2022-01-26") & date <= as.Date("2022-02-07") ~ "ALS-4\nNo",
      date >= as.Date("2022-02-08") & date <= as.Date("2022-03-14") ~ "Mod-ALS-3\nNo",
      date >= as.Date("2022-03-15") & date <= as.Date("2022-05-22") ~ "No-ALS\nNo",
      TRUE ~ NA_character_
    ),
    combined_alert = case_when(
      date >= as.Date("2021-12-20") & date <= as.Date("2022-01-25") ~ "ALS-2-3-4\nYes",
      date >= as.Date("2022-01-26") & date <= as.Date("2022-05-22") ~ "ALS-4-Mod3-None\nNo",
      TRUE ~ NA_character_
    )
  )

# summary: initial default alert levels
# -------------------------------------

beta_summary <- beta_values %>%
  group_by(alert_level) %>%
  summarise(
    mean_value = mean(beta_thing, na.rm = TRUE),
    sd_value = sd(beta_thing, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(alert_level)) %>%
  mutate(type = "Specific")


# summary: the combined alert levels
# ----------------------------------

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

# merge both summaries
# --------------------

beta_summary_all <- bind_rows(beta_summary, beta_summary_combined)

# arrange in descending order
# ---------------------------
ordering <- beta_summary_all %>%
  group_by(alert_level) %>%
  summarise(avg = mean(mean_value)) %>%
  arrange(desc(avg)) %>%
  pull(alert_level)

beta_summary_all$alert_level <- factor(
  beta_summary_all$alert_level,
  levels = ordering
)

# extract unique levels
# --------------------
actual_levels <- levels(beta_summary_all$alert_level)

alert_colors <- c(
  "ALS-2\nNo" = "#1b9e77",
  "ALS-2\nYes" = "#d95f02",
  "ALS-3\nYes" = "#7570b3",
  "ALS-4\nYes" = "#e7298a",
  "ALS-4\nNo" = "#66a61e",
  "Mod-ALS-3\nNo" = "#e6ab02",
  "No-ALS\nNo" = "#a6761d",
  "ALS-2-3-4\nYes" = "#666666",
  "ALS-4-Mod3-None\nNo" = "#1f78b4"
)

# color interventions based on the school closure

alert_k12 <- c("Mod-ALS-3\nNo" = "blue", 
	       "ALS-4-Mod3-None\nNo" = "blue",
	       "ALS-4\nNo"= "blue",
	       "No-ALS\nNo"= "blue",          
	       "ALS-4\nYes" = "red",
	       "ALS-2-3-4\nYes" = "red",
	       "ALS-3\nYes" = "red",
	       "ALS-2\nYes" = "red",
	       "ALS-2\nNo" = "blue"
)
# Plot
beta_errorplot_all <- (
  ggplot(beta_summary_all,
         aes(x = alert_level,
             y = mean_value,
             fill = alert_level,
             color = alert_level)) +
    geom_errorbar(
      aes(ymin = mean_value - sd_value,
          ymax = mean_value + sd_value),
      width = 0.15,
      size = 0.8
    ) +
    geom_point(
      size = 3,
      position = position_dodge(width = 0.5)
    ) +
    geom_text(
      aes(label = round(mean_value, 3)),
      vjust = 0.0,
      hjust = -0.38,
      size = 2.8,
      color = "black"
    ) +
    labs(
      title = "Effect Comparison: Alert Levels and K-12 Schools Closure",
      x = "Intervention",
      y = expression("Transmission rate ("*beta*")")
    ) +
    scale_fill_manual(
      values = alert_colors
    ) +
    scale_color_manual(
      values = alert_colors
    ) +
    theme_clean() +
    theme(
      axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5, color = alert_k12),
      axis.title.x = element_text(size = 12),
      axis.text.y = element_text(size = 8),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 12, hjust = 0.5, face = "plain"),
      legend.position = "none",
      panel.border = element_blank(),
      plot.background = element_blank()
    )
)

print(beta_errorplot_all)


png("../figures/beta_plot.png", width = 2500, height = 1500, res = 300, bg = "white")
beta_errorplot_all
dev.off()










