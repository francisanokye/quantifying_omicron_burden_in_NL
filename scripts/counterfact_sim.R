# Load necessary libraries
library(macpan2)
library(shellpipes)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(conflicted)
library(broom.mixed)

set.seed(2025)

loadEnvironments()

spec <- rdsRead("timevar_spec.rds")
seroprevdata <- rdsRead("seroprevdata.rds")
calibrator <- readRDS("eligfrac3.calibrate.rds")

#  explicitly redefine it with mp_rbf() to match the calibration
print(mp_default(spec))

btt <- dplyr::filter(mp_default(spec), matrix == "beta")
print(btt)

quit()



# extract best parameters and update spec

best_par <- mp_tmb_coef(calibrator)
print(best_par)

updated_spec <- update_default(calibrator$spec, mp_tmb_coef(calibrator))

btt <- dplyr::filter(fitted_data, matrix != "report_prob")
print(mp_default(updated_spec))


spec <- mp_update_par(spec, best_par)


outputs <- c("S", "E", "A", "R", "C", "H", "I", "D", "cases", "beta", "serop", "report_prob") 


simulation_results = mp_simulator(model = spec, time_steps = 183, outputs = outputs)

inc_sim <- mp_trajectory(simulation_results)

#print(inc_sim)

gg <- ggplot(data = inc_sim)+ 
	geom_line(aes(time, value, colour = matrix))+
	labs(x = "Date (Dec 2021  - June 2022)", y = "Incidence", title = "SEARCHI Model - Incidence Trajectory With macpan2", color = "")+
	facet_wrap(~ matrix, scales = "free")+ 
	theme_clean()+
	theme(
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom"
  ) 

print(gg)

rdsSave(inc_sim)

