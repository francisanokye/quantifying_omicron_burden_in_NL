library(shellpipes)
library(zoo)
library(tidyverse)
loadEnvironments()

start_date <- "2021-12-15"
last_date <-"2022-05-26"

## Need the script for seroprevdata

seroprevalence <- csvRead()
seroprevalence <- (seroprevalence 
	|> mutate(week_end = as.Date(week_end, format = "%Y-%m-%d")) 
   # filter antinucleocaspid which indicates only infections
	|> filter((geo == "NL") & (ab_estimate == "Anti-N estimate"))
   |> rename_at("week_end", ~"date")
  # select only the date and pct_mean
   |> select(date, pct_mean)
  # create the count index
   |> mutate(day = 1:n())
)

print(seroprevalence)

serodat <- (seroprevalence
	|> transmute(NULL
		, date
		, time = as.numeric(date - as.Date(start_date))+1
		, matrix = "serop"
		, value = pct_mean
	)
	|> filter(between(date,as.Date(start_date),as.Date(last_date)))
)

print(serodat)

gg <- (ggplot(seroprevalence,aes(date,pct_mean))
	+ geom_point()
)

print(gg)

rdsSave(serodat)

