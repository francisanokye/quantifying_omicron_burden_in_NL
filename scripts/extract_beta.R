library(shellpipes)
library(conflicted)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(broom.mixed)
library(patchwork)
library(macpan2)

set.seed(2025)

loadEnvironments()

calibrator <- rdsRead("calibrate.rds")

fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE)

fitted_data <- (fitted_data
        |> mutate(dates = as.Date(start_date) + as.numeric(time) -1 )
        |> dplyr::filter(between(dates, as.Date(start_date), as.Date(last_date)))
)

beta_values <- dplyr::filter(fitted_data, matrix %in% c("beta"))

beta_values$alert_level <- rep(c('ALS-2', 'ALS-3', 'ALS-4', 'Mod-ALS-3', 'No-ALS'), times = c(10, 15, 30, 35, 73))

alert_colors <- c("No-ALS" = "darkgray", "ALS-2" = "brown", "ALS-3" = "#009E73", "Mod-ALS-3" = "orange", "ALS-4" = "blue")

beta_values$alert_level <- factor(beta_values$alert_level, levels = names(alert_colors))

beta_values <- beta_values[c("dates","alert_level","value")]

print(beta_values)

betaplot <- (ggplot() +
  geom_rect(data = beta_values,aes(xmin=ymd('2022-03-18'), xmax = ymd('2022-05-26'), ymin = -Inf, ymax = Inf), fill = adjustcolor("#F7E2E2", alpha = 0.03), alpha = 0.05) + 
  geom_smooth(data = beta_values, aes(x = dates, y = value, color = "darkgray"), span = 0.15, alpha = 0.7, linewidth = 1.5) +
  #geom_ribbon(data = beta_values, aes(ymin = mean_beta - sd_beta, ymax = mean_beta + sd_beta), fill = "green", alpha = 0.35)+
  geom_line(data = beta_values, aes(x = dates, y = value, color = alert_level), linewidth = 1.5) +
  labs(title = "Transmission Rates Across Time",x = "Date (Dec 15, 2021 - May 26, 2022)",y = expression(""*beta*"")) +
  scale_color_manual(values = alert_colors, guide = "none") + 
  theme_clean() +
  geom_vline(data = beta_values, aes(xintercept = as.Date("2021-12-24")), colour = "gold4", linetype = 4, linewidth = 1) +
  geom_vline(data = beta_values, aes(xintercept = as.Date("2022-01-08")), colour = "gold4", linetype = 4, linewidth = 1) +
  geom_vline(data = beta_values, aes(xintercept = as.Date("2022-02-07")), colour = "gold4", linetype = 4, linewidth = 1) +
  geom_vline(data = beta_values, aes(xintercept = as.Date("2022-03-14")), colour = "gold4", linetype = 1, linewidth = 1) +
  theme(
    axis.text.x = element_text(size = 12, hjust = 1, angle = 45),
    axis.title.x = element_text(size = 12, color = "black", face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12, color = "black", face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.justification = "center",
    legend.background = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 0),
    panel.border = element_blank(),
    plot.background = element_blank()
  ))

# Fix alert_errorplot by correctly assigning alert_level colors
alert_errorplot <- (beta_values |>
                  group_by(alert_level) |>
                  summarize(mean_value = mean(value), sd_value = sd(value)) |>
                  ggplot(aes(x = alert_level, y = mean_value, color = alert_level, ymin = mean_value - sd_value, ymax = mean_value + sd_value)) +
                  geom_errorbar(width = 0.3, position = position_dodge(width = 0.25), show.legend = FALSE) +
                  geom_point(size = 4, position = position_dodge(width = 0.5), show.legend = FALSE) +
                  geom_text(aes(label = round(mean_value, 2)), vjust = -1.0, hjust = -0.20, color = "black", size = 3.5) +
                  labs(title = "Mean Transmission Rate", x = "Alert Levels", y = expression(""*beta*"")) +
                  scale_color_manual(values = alert_colors, guide = "none") + 
                  theme_clean() +
                  theme(
                        axis.text.x = element_text(size = 10, hjust = 1, angle = 45),
                        axis.title.x = element_text(size = 12, color = "black", face = "bold"),
                        axis.text.y = element_text(size = 12),
                        axis.title.y = element_text(size = 12, color = "black", face = "bold"),
                        plot.title = element_text(size = 12, face = "bold", color = "black", hjust = 0.5),
                        legend.position = element_blank(),
                        panel.border = element_blank(),
                        plot.background = element_blank()
                  ))

# Combine plots
combined_plot <- (betaplot | alert_errorplot) + 
                 plot_layout(guides = "collect", widths = c(1, 1)) & 
                 theme(legend.position = "bottom")

# Print the final combined plot
print(combined_plot)

ggsave("extract_beta.png", plot = combined_plot, width = 12, height = 6, dpi = 300)

