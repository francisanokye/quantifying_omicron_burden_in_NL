library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(zoo)
library(ggthemes)
library(cowplot)
library(patchwork)
library(fuzzyjoin)
library(shellpipes)
library(tidyverse)
library(macpan2)
loadEnvironments()
set.seed(2025)

start_date <- "2021-12-15"
last_date <-"2022-05-26"

calibrator <- rdsRead("calibrate_inc.rds")

# model simulation with calibrated parameters
fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE)

fitted_data <- (fitted_data
        |> mutate(date = as.Date(start_date) + as.numeric(time) -1 )
        |> dplyr::filter(between(date, as.Date(start_date), as.Date(last_date)))
        |> dplyr::filter(matrix %in% c("date","beta"))
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
  select(c(date, beta))

beta_values$alert_level <- rep(c('ALS-2', 'ALS-3', 'ALS-4', 'Mod-ALS-3', 'No-ALS'),times = c(10, 10, 35, 35, 73))

alert_colors <- c(
  "No-ALS"      = "#F7E2E2",  
  "ALS-2"       = "#D3D3D3", 
  "ALS-3"       = "#66D1B5",  
  "Mod-ALS-3"   = "#FFD580",  
  "ALS-4"       = "#87CEFA"   
)

beta_values$alert_level <- factor(beta_values$alert_level, levels = names(alert_colors))

beta_summary <- aggregate(beta ~ alert_level, data = beta_values, 
                          FUN = function(x) c(mean_value = mean(x), sd_value = sd(x)))
beta_summary <- do.call(data.frame, beta_summary)

names(beta_summary) <- c("alert_level", "mean_value", "sd_value")

desired_order <- c("No-ALS", "ALS-2", "ALS-3", "Mod-ALS-3", "ALS-4")
beta_summary$alert_level <- factor(beta_summary$alert_level, levels = desired_order)

beta_errorplot <- ggplot(beta_summary, aes(x = alert_level, y = mean_value, color = alert_level)) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                width = 0.15, position = position_dodge(width = 0.2), show.legend = FALSE,size = 1) +
  geom_point(size = 3,position = position_dodge(width = 0.5), show.legend = FALSE) +
  geom_text(aes(label = round(mean_value, 2)), vjust = 0.5, hjust = -0.38, color = "black", size = 2.5) +
  labs(title = "Alert Levels' Mean Transmission Rate ", x = "Alert Levels", y = expression(""*beta*"")) +
  scale_color_manual(values = alert_colors, guide = "none") +
  theme_clean() +
  theme(
    axis.text.x = element_text(size = 8, hjust = 0.5, angle = 0),
    axis.title.x = element_text(size = 8, color = "black", face = "bold"),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10, color = "black", face = "bold"),
    plot.title = element_text(size = 8, face = "bold", color = "black", hjust = 0.5),
    legend.position = element_blank(),
    panel.border = element_blank(),
    plot.background = element_blank()
  )

print(beta_errorplot)

png("../figures/beta_plot.png", width = 1600, height = 1000, res = 300, bg = "white")
beta_errorplot
dev.off()






