library(lubridate)
library(RColorBrewer)
library(zoo)
library(ggthemes)
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

# load reported cases
# serop_case_true <- rdsRead("fitsero.rds")

# model simulation with calibrated parameters
fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE)

fitted_data <- (fitted_data
	|> mutate(dates = as.Date(start_date) + as.numeric(time) -1 )
	|> dplyr::filter(between(dates, as.Date(start_date), as.Date(last_date)))
	|> dplyr::filter(matrix %in% c("beta", "sero_inc","serop"))
)

print(fitted_data)

rdsSave(fitted_data)
