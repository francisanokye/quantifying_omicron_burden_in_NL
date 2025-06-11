library(tidyverse)
library(shellpipes)

dat <- csvRead()

day0 <- as.Date("2021-12-17")

nl <- (dat
	|> filter(grepl("ewfoundland",prename)) 
	|> select(week_end,contains("numtotal"))
	|> mutate( numtotal_2nd_additional = ifelse(is.na(numtotal_2nd_additional),0,numtotal_2nd_additional)
		, additional_tot = numtotal_1additional + numtotal_2nd_additional
	)
	|> pivot_longer(-week_end,names_to="type",values_to="value")
)

gg <- (ggplot(nl,aes(week_end,value))
	+ geom_point()
	+ facet_wrap(~type,scale="free")
)

print(gg)

nl_additional <- (nl
	|> filter(type == "numtotal_additional")
	|> mutate(value = ifelse(week_end == as.Date("2021-12-18"),10,value))
	|> filter(!is.na(value))
	|> mutate(daydiff = diff(c(week_end,0))
		, boosterdiff = diff(c(value,0))
		, booster_daily_rate = boosterdiff/as.numeric(daydiff)
		, days = as.numeric(week_end - day0)
	)
	|> select(days,booster_daily_rate)
	|> filter(days < 159)

)

csvSave(nl_additional)

