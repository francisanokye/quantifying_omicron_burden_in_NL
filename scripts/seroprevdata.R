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

print(serodat)

gg <- (ggplot(serodat,aes(dates,value))
	+ geom_point()
)

print(gg)

serodat2 <- (serodat
	|> filter(matrix == "cases")
	|> mutate(matrix = "inc")
)

print(serodat2,n=Inf)

serodat3 <- (serodat
	|> filter(matrix == "serop")
	|> mutate(value = value * 510550
		, value = diff(c(value,NA))
	)
	|> mutate(matrix = "inc")
	|> filter(!is.na(value))
)

print(serodat3)

print(ggplot(serodat3,aes(dates,value))+geom_point())

rdsSave(serodat3)


