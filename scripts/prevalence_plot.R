
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

start_date <- "2021-12-15"
last_date <-"2022-05-26"

calibrator <- rdsRead("calibrate_inc.rds") #readRDS("calibrate_inc.rds")

# model simulation with calibrated parameters
fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE)

fitted_data <- (fitted_data
        |> mutate(dates = as.Date(start_date) + as.numeric(time) -1 )
        |> dplyr::filter(between(dates, as.Date(start_date), as.Date(last_date))))

# plot setup 
pp <- (ggplot(data = fitted_data, aes(x = dates, y = value))
       + geom_line(data = fitted_data, aes(x = dates, y = value,color = matrix), span = 0.15, alpha = 0.85, linewidth = 1.0)
       + geom_line(data = fitted_data, aes(color = matrix), linewidth = 1.0)
       + scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d")
       + scale_fill_manual(labels = c("reported","estimated"), values = c("green","steelblue1"))
       + labs(x = "Date (Dec 15, 2021 - May 26, 2022)", y = "Value", title = "Estimated Model Prevalence", color = "")
       + theme_clean()
       + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 0.5),
	       axis.title.x = element_text(size = 10, color = "black", face = "bold"),
	       axis.text.y = element_text(size = 8),
	       axis.title.y = element_text(size = 10, color = "black", face = "bold"),
	       plot.title = element_text(size = 12, face = "bold", color = "black", hjust = 0.5),
	       strip.text = element_text(size = 10, face = "bold", color = "black"),
	       legend.position = "bottom",
	       legend.title = element_text(size = 0),
	       legend.text = element_text(size = 8),
	       legend.background = element_rect(color = NA),
	       legend.margin = margin(0, 0, 0, 0),
	       plot.background = element_blank()) 
       + guides(color = guide_legend(), fill = guide_legend())
       + facet_wrap(~matrix, scales = "free_y",
             labeller = labeller(matrix = c(
               "beta" = "Transmission",
               "cases" = "Cases",
               "inc" = "Incidence",
               "report_prob" = "Report Prob.",
               "serop" = "Seroprev."
             )))
       + theme(strip.text = element_text(size = 10, face = "bold", color = "black"),
               axis.title.y = element_text(size = 10, face = "bold", color = "black"))

)

print(pp)

## Don't do this

png("../figures/prevalence_plot.png", width = 3600, height = 2400, res = 300, bg = "white", type = "cairo")
pp
dev.off()



