library(tidyverse); theme_set(theme_bw())
library(shellpipes)

df <- rdsRead()

print(df)

gg <- (ggplot(df,aes(x=mat,y=estimate))
	+ geom_point()
	+ geom_pointrange(aes(ymin=estimate-2*std.error, ymax=estimate+2*std.error))
	+ facet_grid(mu~zeta)
	+ coord_flip()
)
	
print(gg)


gg2 <- (ggplot(df,aes(x=estimate,y=std.error))
	+ geom_point(aes(color=factor(mu),shape=factor(zeta)))
	+ facet_wrap(~mat,scale="free",nrow=5)
)

print(gg2)
