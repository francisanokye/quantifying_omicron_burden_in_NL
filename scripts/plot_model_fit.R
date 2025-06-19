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
set.seed(2025)
options(macpan2_log_dir = ".")
loadEnvironments()

pop = 510550

start_date <- "2021-12-15"
last_date <-"2022-05-26"

calibrator <- rdsRead("calibrate_inc.rds")

# load true incidence from seroprevalence estimates
serop_case_true <- rdsRead("fitsero.rds")
# convert matrix values into columns
serop_case_true <- serop_case_true |>
  pivot_wider(names_from = matrix, values_from = value) |>
  group_by(date) |>
  mutate(across(everything(), ~ first(na.omit(.)), .names = "{.col}")) |>
  ungroup() |>
  distinct(date, .keep_all = TRUE) |>
  drop_na() 

# model simulation with calibrated parameters
fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE)

fitted_data <- (fitted_data
	|> mutate(date = as.Date(start_date) + as.numeric(time) -1 )
	|> dplyr::filter(between(date, as.Date(start_date), as.Date(last_date)))
	|> dplyr::filter(matrix %in% c("date","sero_inc","serop"))
)

# convert matrix values into columns
true_infections <- fitted_data |>
  select(-any_of(c("row", "col"))) |>
  pivot_wider(names_from = matrix, values_from = value) |>
  group_by(date) |>
  mutate(across(everything(), ~ first(na.omit(.)), .names = "{.col}")) |>
  ungroup() |>
  distinct(date, .keep_all = TRUE) |>
  drop_na() |>
  select(c(date, serop,sero_inc)) |>
  mutate(date = as.Date(date), sero_inc = as.integer(sero_inc))

seroprevalence_plot <- (ggplot(data = true_infections, aes(x = date, y = serop))+
                          geom_rect(aes(xmin=ymd('2022-03-17'), xmax = ymd('2022-05-26'), ymin = 0, ymax = 0.45), 
                                    fill = adjustcolor("#F7E2E2", alpha = 0.03), alpha = 0.05) 
                        + geom_ribbon(data = true_infections, aes(ymin = pmax(0, serop - 0.25 * sd(serop)),
								  ymax = serop + 0.25 * sd(serop)), fill = "gray70", alpha = 0.75)
			+ geom_point(data = serop_case_true, aes(x = date, y = serop, color = "seroprevalence"), size = 2.5)
			+ geom_line(data = true_infections, aes(x = date, y = serop, color = "model fit"), span = 0.10, alpha = 1.0, linewidth = 1)
                        + scale_color_manual(labels = c("model fit","seroprevalence"), values = c("#00BFC4","black"))
                        + labs(x = "Date (Dec 15, 2021 - May 26, 2022)", y = "Seroprevalence Estimate (%)", title = "Seroprevalence Fit", color = "")
                        + theme_clean()
                        + geom_vline(xintercept = as.Date("2021-12-24"), colour = "gold4", linetype = 2, size = 1)  
                        + geom_vline(xintercept = as.Date("2022-01-08"), colour = "gold4", linetype = 2, size = 1)  
                        + geom_vline(xintercept = as.Date("2022-02-07"), colour = "gold4", linetype = 2, size = 1)  
                        + geom_vline(xintercept = as.Date("2022-03-14"), colour = "black", linetype = 1, size = 1)  
                        + annotate("text", x = as.Date("2021-12-18"), y = 0.2, label = "ALS-2",
                                   size = 4,angle = 90, hjust = 1, color = "black")
                        + annotate("text", x = as.Date("2021-12-28"), y = 0.2, label = "ALS-3",
                                   size = 4,angle = 90, hjust = 1,color = "black")
                        + annotate("text", x = as.Date("2022-02-01"), y = 0.20, label = "ALS-4",
                                   size = 4,angle = 0, hjust = 1, color = "black")
                        + annotate("text", x = as.Date("2022-03-11"), y = 0.20, label = "Mod-ALS-3",
                                   size = 4,angle = 0, hjust = 1, color = "black")
                        + annotate("text", x = as.Date("2022-04-28"), y = 0.16, label = "No-ALS",
                                   size = 4, hjust = 1, color = "black")
                        + theme(axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
                                axis.title.x = element_text(size = 14, color = "black", face = "bold"),
                                axis.text.y = element_text(size = 14),
                                axis.title.y = element_text(size = 14, color = "black", face = "bold"),
                                plot.title = element_text(size = 14, face = "bold", color = "black", hjust = 0.5),
                                strip.text = element_text(size = 14, face = "bold", color = "black"),
                                legend.position = c(0.35,0.75),
                                legend.title = element_text(size = 0),
                                legend.text = element_text(size = 12),
                                legend.background = element_rect(color = NA),
                                legend.margin = margin(0, 0, 0, 0),
                                plot.background = element_blank()) 
                        +guides(color = guide_legend(), fill = guide_legend())
)


seroinc_plot <- (ggplot(data = true_infections, aes(x = date, y = sero_inc))+
                          geom_rect(aes(xmin=ymd('2022-03-17'), xmax = ymd('2022-05-26'), ymin = 0, ymax = max(sero_inc)+150),
                                    fill = adjustcolor("#F7E2E2", alpha = 0.03), alpha = 0.05)
                        + geom_ribbon(data = true_infections,aes(ymin = pmax(0, sero_inc - 0.25 * sd(sero_inc)),
								 ymax = sero_inc + 0.25 * sd(sero_inc)), fill = "gray70", alpha = 0.75)
                        + geom_point(data = serop_case_true, aes(x = date, y = sero_inc, color = "seroincidence"), size = 2.5)
                        + geom_line(data = true_infections, aes(x = date, y = sero_inc, color = "model fit"), span = 0.10, alpha = 1.0, linewidth = 1)
                        + scale_color_manual(labels = c("model fit","seroincidence"), values = c("red","black"))
                        + labs(x = "Date (Dec 15, 2021 - May 26, 2022)", y = "Seroincidence",
                               title = "Daily Seroincidence", color = "")
			+ theme_clean()
                        + geom_vline(xintercept = as.Date("2021-12-24"), colour = "gold4", linetype = 2, size = 1)
                        + geom_vline(xintercept = as.Date("2022-01-08"), colour = "gold4", linetype = 2, size = 1)
                        + geom_vline(xintercept = as.Date("2022-02-07"), colour = "gold4", linetype = 2, size = 1)
                        + geom_vline(xintercept = as.Date("2022-03-14"), colour = "black", linetype = 1, size = 1)
                        + annotate("text", x = as.Date("2021-12-18"), y = 1200, label = "ALS-2",
                                   size = 4,angle = 90, hjust = 1, color = "black")
                        + annotate("text", x = as.Date("2021-12-28"), y = 1200, label = "ALS-3",
                                   size = 4,angle = 90, hjust = 1,color = "black")
                        + annotate("text", x = as.Date("2022-02-01"), y = 1200, label = "ALS-4",
                                   size = 4,angle = 0, hjust = 1, color = "black")
                        + annotate("text", x = as.Date("2022-03-11"), y = 500, label = "Mod-ALS-3",
                                   size = 4,angle = 0, hjust = 1, color = "black")
                        + annotate("text", x = as.Date("2022-04-28"), y = 500, label = "No-ALS",
                                   size = 4, hjust = 1, color = "black")
                        + theme(axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
                                axis.title.x = element_text(size = 14, color = "black", face = "bold"),
                                axis.text.y = element_text(size = 14),
                                axis.title.y = element_text(size = 14, color = "black", face = "bold"),
                                plot.title = element_text(size = 14, face = "bold", color = "black", hjust = 0.5),
                                strip.text = element_text(size = 14, face = "bold", color = "black"),
                                legend.position = c(0.35,0.75),
                                legend.title = element_text(size = 0),
                                legend.text = element_text(size = 12),
                                legend.background = element_rect(color = NA),
                                legend.margin = margin(0, 0, 0, 0),
                                plot.background = element_blank())
                        +guides(color = guide_legend(), fill = guide_legend())
)


combined_plot <- (seroprevalence_plot | seroinc_plot) + 
                 plot_layout(guides = "collect", widths = c(1, 1)) & 
                 theme(legend.position = "bottom")
print(combined_plot)

png("../figures/model_fit.png", width = 3400, height = 1800, res = 300, bg = "white", type = "cairo")
combined_plot
dev.off()
