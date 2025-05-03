library(shellpipes)
library(zoo)
library(tidyverse)
loadEnvironments()

#start_date <- "2021-12-14"
#last_date <- "2022-06-01"
#trim_report <- "2022-05-01"
#trim_report <- "2022-06-01"

sero <- (csvRead() 
	|> mutate(dates = as.Date(date,format = "%Y-%m-%d"))
	|> select(-date)
	|> filter(between(dates,as.Date(start_date),as.Date(last_date)))
)

serodat <- (sero
	#%>% select(dates, adjusted_serop_cases, seroprevalence, cases)
        %>% select(dates, seroprevalence, cases)	
	%>% mutate(time = seq_along(dates))
        %>% pivot_longer(cols = c( seroprevalence,cases),names_to = "matrix",values_to = "value")
        %>% mutate(matrix = recode(matrix,"seroprevalence" = "serop"))
	#%>% pivot_longer(cols = c(adjusted_serop_cases, seroprevalence,cases),names_to = "matrix",values_to = "value") 
	#%>% mutate(matrix = recode(matrix,"adjusted_serop_cases" = "sero_cases","seroprevalence" = "serop")) 
	%>% arrange(matrix)
)

#serodat <- (serodat
#	|> mutate(value = ifelse((matrix == "cases")& (dates > as.Date(trim_report)),NA,value)
#	)
#	|> group_by(matrix)
#	|> mutate(rollavg = round(rollmean(value,7,align="left",fill=NA)), value = ifelse(matrix == "cases", rollavg, value))
#	|> filter(!is.na(value))
#)

print(serodat,n=Inf)


rdsSave(serodat)
#saveEnvironment()

