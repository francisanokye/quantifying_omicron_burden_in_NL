library(shellpipes)
library(zoo)
library(tidyverse)
loadEnvironments()

start_date <- "2021-12-15"
last_date <-"2022-05-26"

sero <- (csvRead() 
	|> mutate(dates = as.Date(date,format = "%Y-%m-%d"))
	|> select(-date)
	|> filter(between(dates,as.Date(start_date),as.Date(last_date)))
)

serodat <- (sero
        %>% select(dates, seroprevalence, cases)	
	%>% mutate(time = seq_along(dates))
        %>% pivot_longer(cols = c( seroprevalence,cases),names_to = "matrix",values_to = "value")
        %>% mutate(matrix = recode(matrix,"seroprevalence" = "serop")) 
	%>% arrange(matrix)
)

rdsSave(serodat)


