library(macpan2)
library(shellpipes)
library(conflicted)
library(tidyverse); theme_set(theme_bw())
library(dplyr)
library(ggthemes)
library(broom.mixed)
set.seed(2024)

loadEnvironments()

population = 510550

calibrator <- rdsRead("calibrate.rds")


fitted_data <- mp_trajectory_sd(calibrator)
start_date <- as.Date("2021-12-15")
fitted_data$dates <- start_date + as.numeric(fitted_data$time) - 1
fitted_data <- fitted_data[(fitted_data$dates > "2021-12-14")& (fitted_data$dates <= "2022-06-02"),]

# subset data for "report_prob"
fitted_data_report_prob <- dplyr::filter(fitted_data, matrix == "report_prob")
# subset data without "report_prob"
fitted_data_others <- dplyr::filter(fitted_data, matrix != "report_prob")

print(head(fitted_data))

newdat <- (fitted_data
	|> dplyr::filter(matrix %in% c("S","E","A","R","I","C","H","D"))
	|> select(matrix, value,dates)
)

newdat2 <- (newdat
	|> group_by(dates)
	|> summarise(matrix = "total"
		, value = sum(value)
	)
)

newdat3 <- rbind(newdat2,newdat)

gg <- (ggplot(newdat3, aes(dates,value))
	+ geom_line()
	+ facet_wrap(~matrix,scale="free")
)

print(gg)

