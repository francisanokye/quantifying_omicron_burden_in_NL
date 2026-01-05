library(tidyverse); theme_set(theme_bw())
library(shellpipes)
rpcall("R1_grid_params_plot.Rout R1_grid_params_plot.R R1_grid_params.rds")

df <- rdsRead()

print(df)

gg <- (ggplot(df,aes(x=mat,y=estimate))
	+ geom_point()
	+ geom_pointrange(aes(ymin=estimate-2*std.error, ymax=estimate+2*std.error))
	+ facet_grid(mu~zeta)
	+ coord_flip()
)
	
# print(gg)


gg2 <- (ggplot(df,aes(x=estimate,y=std.error))
	+ geom_point(aes(color=factor(mu),shape=factor(zeta)))
	+ facet_wrap(~mat,scale="free",nrow=5)
)

# print(gg2)

gg3 <- (ggplot(df, aes(x=interaction(mu,zeta),y=estimate))
	+ geom_point()
	+ geom_pointrange(aes(ymin=estimate-2*std.error, ymax=estimate+2*std.error))
	+ facet_wrap(~mat,scale="free")
	+ coord_flip()
)

print(gg3)

