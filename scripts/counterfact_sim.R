library(macpan2)
library(shellpipes)
library(conflicted)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(broom.mixed)
library(gridExtra)
library(grid)
library(gtable)

set.seed(2025)

loadEnvironments()

seroprevdata <- rdsRead("seroprevdata.rds")
spec <- rdsRead("timevar_spec.rds")
calibrator <- readRDS("eligfrac3.calibrate.rds")

# extract estimated best time-varying betas
beta_weights <- mp_tmb_coef(calibrator) |> 
	dplyr::filter(mat == "time_var_beta") |> 
	pull(estimate)

# make a copy of the model spec
newspec <- spec 

newspec$time_var_beta <- mp_rbf("beta", 6)

newspec$time_var_beta$weights <- beta_weights

print(newspec)


quit()


fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE)

fitted_data <- (fitted_data
	|> mutate(dates = as.Date(start_date) + as.numeric(time) -1 )
	|> dplyr::filter(between(dates, as.Date(start_date), as.Date(last_date)))
	|> dplyr::filter(matrix %in% c("beta","cases", "report_prob","serop"))
)
# save simulation output for adjusted infections using eligibility reporting probability 
write.csv(fitted_data, "../data/eligibility_adjusted_cases.csv", row.names = FALSE)

# subset data for "report_prob"
fitted_data_report_prob <- dplyr::filter(fitted_data, matrix == "report_prob")

# subset data without "report_prob"
fitted_data_others <- dplyr::filter(fitted_data, matrix != "report_prob")

# plot setup 
pp <- (ggplot(data = fitted_data_others, aes(x = dates, y = value))
       + geom_rect(aes(xmin=ymd('2022-03-18'), xmax = ymd('2022-05-26'), ymin = -Inf, ymax = Inf), fill = adjustcolor("#F7E2E2", alpha = 0.03), alpha = 0.05) 
       + geom_bar(data = fitted_data_others%>%dplyr::filter(matrix %in% c("cases")), aes(x = dates, y = value, fill = "estimated"),
		  stat = "identity", position = "stack", width = 0.25, alpha = 1, show.legend = FALSE) 
       + geom_rect(data = fitted_data_report_prob,aes(xmin=ymd('2022-03-17'), xmax = ymd('2022-05-26'), ymin = -Inf, ymax = Inf), fill = adjustcolor("#F7E2E2", alpha = 0.03), alpha = 0.05)
       + geom_bar(data = seroprevdata%>%dplyr::filter(matrix %in% c("cases")), aes(x = dates, y = value, fill = "reported"),
		  stat = "identity", position = "stack", width = 0.25, alpha = 1, show.legend = FALSE) 
       + geom_line(data = seroprevdata, aes(x = dates, y = value, color = "data"), linewidth = 1.0)  
       + geom_ribbon(data = fitted_data_others, aes(ymin = conf.low, ymax = conf.high), fill = "gray", alpha = 0.5)
       + geom_smooth(data = fitted_data_others, aes(x = dates, y = value,color = matrix), span = 0.15, alpha = 0.85, linewidth = 1.0)
       # Use geom_line instead for "report_prob" to ensure it remains in the plot
       + geom_line(data = fitted_data_report_prob, aes(color = matrix), linewidth = 1.0)
       + scale_fill_manual(labels = c("reported","estimated"), values = c("green","steelblue1"))
       + labs(x = "Date (Dec 15, 2021 - May 26, 2022)", y = "Value", title = "Estimated Cases Under RT-PCR Eligibility Criteria Changes", color = "") 
       + scale_color_manual(labels = c("beta", "case_fit", "data","report_prob", "serop_fit"), values = c("blue","#E31f26", "black", "red", "#00BFC4")) 
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
        plot.background = element_blank()) +
  guides(color = guide_legend(), fill = guide_legend())
       +facet_wrap(~matrix, scales = "free_y", 
             labeller = labeller(matrix = c(
               "beta" = "Transmission Rate",
               "cases" = "Case Fit",
               "data" = "Observed Data",
               "report_prob" = "Reporting Probability",
               "serop" = "Seroprevalence Fit"
             ))) 
       + theme(strip.text = element_text(size = 10, face = "bold", color = "black"),
               axis.title.y = element_text(size = 10, face = "bold", color = "black"))

)

# Add geom_vline only for the "report_prob" facet by using filtered data 
pp <- pp + geom_vline(data = fitted_data_report_prob, aes(xintercept = as.Date("2021-12-15")), colour = "gray", linetype = 2, linewidth = 1) + 
           geom_vline(data = fitted_data_report_prob, aes(xintercept = as.Date("2022-01-01")), colour = "gray", linetype = 2, linewidth = 1) + 
           geom_vline(data = fitted_data_report_prob, aes(xintercept = as.Date("2022-01-23")), colour = "gray", linetype = 2, linewidth = 1) + 
           geom_vline(data = fitted_data_report_prob, aes(xintercept = as.Date("2022-02-22")), colour = "gray", linetype = 2, linewidth = 1) + 
           geom_vline(data = fitted_data_report_prob, aes(xintercept = as.Date("2022-03-16")), colour = "gray", linetype = 1, linewidth = 1) 
 
# Add geom_vline for the rest of the facets (excluding "report_prob") 
pp <- pp + geom_vline(data = fitted_data_others, aes(xintercept = as.Date("2021-12-24")), colour = "gold4", linetype = 4, linewidth = 1) + 
           geom_vline(data = fitted_data_others, aes(xintercept = as.Date("2022-01-08")), colour = "gold4", linetype = 4, linewidth = 1) + 
           geom_vline(data = fitted_data_others, aes(xintercept = as.Date("2022-02-07")), colour = "gold4", linetype = 4, linewidth = 1) + 
           geom_vline(data = fitted_data_others, aes(xintercept = as.Date("2022-03-14")), colour = "gold4", linetype = 1, linewidth = 1) 
print(pp)

#ggsave("eligfra3_plot.png", plot = pp, width = 12, height = 6, dpi = 300)

