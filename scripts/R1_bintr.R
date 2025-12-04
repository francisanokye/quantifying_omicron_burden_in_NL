library(tidyverse)
library(bbmle)
library(shellpipes)

loadEnvironments()

dat <- rdsRead()

ts <- (dat
	|> transmute(NULL
		, time
		, pcInc = round(value*params[["N"]])
		, intInc = diff(c(NA,pcInc))
	)
	|> filter(!is.na(intInc))
)

print(ts)

#m <- mle2(ts$intInc ~ dpois(expSim(i0, rsim, time)$intSim)
#	, start = list(i0=1, rsim=0)
#	, data = ts
#)

m <- mle2(ts$intInc ~ dnbinom(mu=flexSim(li0, rsim, lK=Inf,time)$intSim, size=ss)
	, start = list(li0=0,rsim=0.1,ss=1)
	, data = ts
)

print(summary(m))


newdat <- data.frame(time = 1:400)

newdat$Inc <- predict(m,newdata=newdat,type="reponse")
ts$pred <- predict(m)

gg <- (ggplot()
	+ geom_point(data=ts,aes(x=time,y=intInc))
	+ geom_point(data=ts,aes(x=time,y=pred),color="blue")
	+ geom_line(data=newdat,aes(x=time,y=Inc),color="red")
)

print(gg)

