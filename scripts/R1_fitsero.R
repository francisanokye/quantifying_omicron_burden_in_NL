library(tidyverse)
library(splines)
library(shellpipes)

loadEnvironments()

serodat <- rdsRead()

newdat <- data.frame(time = (min(serodat$time)-1):max(serodat$time))
#newdat <- data.frame(time = 1:400)

dat <- (serodat
	|> transmute(NULL
		, time
		, value
		, cumInc = value*params[["N"]]
	)
)

print(dat)

mm <- lm(cumInc ~ bs(time,6)
	, data = dat
)

print(summary(mm))

newdat$predcumInc <- predict(mm,newdata=newdat)

print(dat)

gg <- (ggplot()
	+ geom_point(data=dat,aes(x=time,y=cumInc),size=2)
	+ geom_line(data=newdat,aes(x=time,y=predcumInc),color="red")
)

print(gg)

incdat <- (newdat
	|> mutate(predInc = round(diff(c(NA,predcumInc))))
)

print(plot(x=incdat$time,y=incdat$predInc))

rdsSave(incdat)


