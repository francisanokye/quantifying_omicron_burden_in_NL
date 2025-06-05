library(tidyverse)
library(shellpipes)

dat <- csvRead()

nl <- (dat
	|> filter(grepl("ewfoundland",prename)) 
	|> select(week_end,contains("numtotal"))
	|> pivot_longer(-week_end,names_to="type",values_to="value")
)

gg <- (ggplot(nl,aes(week_end,value))
	+ geom_point()
	+ facet_wrap(~type,scale="free")
)

print(gg)

