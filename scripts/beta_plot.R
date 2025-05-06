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

loadEnvironments()

# load true infections data 
true_infections <- csvRead()  

# convert matrix values into columns
true_infections <- true_infections |>
  select(-any_of(c("row", "col"))) |>
  pivot_wider(names_from = matrix, values_from = value) |>
  group_by(dates) |>
  mutate(across(everything(), ~ first(na.omit(.)), .names = "{.col}")) |>
  ungroup() |>
  distinct(dates, .keep_all = TRUE) |>
  drop_na() |>
  select(c(dates, cases, serop, beta, report_prob, conf.low, conf.high)) |>
  mutate(dates = as.Date(dates), cases = as.integer(cases))

true_infections$alert_level <- rep(c('ALS-2', 'ALS-3', 'ALS-4', 'Mod-ALS-3', 'No-ALS'),times = c(10, 15, 30, 35, 73))

true_infections <- true_infections %>%
  select(dates, alert_level, beta)

#alert_colors <- c("No-ALS" = "darkgray", "ALS-2" = "brown", "ALS-3" = "#009E73", "Mod-ALS-3" = "orange", "ALS-4" = "steelblue")
alert_colors <- c(
  "No-ALS"      = "#D3D3D3",  
  "ALS-2"       = "#D98880", 
  "ALS-3"       = "#66D1B5",  
  "Mod-ALS-3"   = "#FFD580",  
  "ALS-4"       = "#87CEFA"   
)

true_infections$alert_level <- factor(true_infections$alert_level, levels = names(alert_colors))

beta_summary <- aggregate(beta ~ alert_level, data = true_infections, 
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

png("../figures/beta_plot.png", width = 1600, height = 1000, res = 300, bg = "white")
beta_errorplot
dev.off()

