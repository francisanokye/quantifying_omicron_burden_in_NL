library(tidyverse)
library(conflicted)
library(ggthemes)
library(shellpipes)
rpcall("R1_grid_params_plot.Rout R1_grid_params_plot.R R1_grid_params.rds")

df <- rdsRead()

print(df)

gg <- (ggplot(df,aes(x=mat,y=estimate))
	+ geom_point(size = 1.5)
	+ geom_pointrange(aes(ymin=estimate-2*std.error, ymax=estimate+2*std.error),size = 1.5)
	+ facet_grid(mu~zeta)
	+ coord_flip()
	+ labs(
	  title = NULL,
	  x     = NULL,
	  y     = NULL
	) 
	+ theme_clean() 
	+ theme(
	  axis.text.x       = element_text(size = 25,  hjust = 1, margin = margin(t = 8)),
	  axis.text.y       = element_text(size = 25),
	  axis.title.x      = element_text(size = 25),
	  axis.title.y      = element_text(size = 25),
	  plot.title        = element_text(size = 25, hjust = 0.5, face = "plain"),
	  strip.text        = element_text(size = 25, face = "plain", hjust = 0.5),
	  strip.background  = element_blank(),
	  legend.position   = "top",
	  legend.direction  = "horizontal",
	  legend.box        = "horizontal",
	  legend.spacing.x  = unit(1.2, "cm"),
	  legend.spacing.y  = unit(0.5, "cm"),
	  legend.background = element_blank(),
	  legend.text       = element_text(size = 25, face = "plain"),
	  panel.spacing     = unit(1.5, "cm"),
	  plot.background   = element_blank()
	)
)
	
# print(gg)


gg2 <- (ggplot(df,aes(x=estimate,y=std.error))
	+ geom_point(aes(color=factor(mu),shape=factor(zeta)), size = 6.5)
	+ facet_wrap(~mat,scale="free",nrow=5)
	+ labs(
	  title = NULL,
	  x     = NULL,
	  y     = NULL
	) 
	+ theme_clean() 
	+ theme(
	  axis.text.x       = element_text(size = 25,  hjust = 1, margin = margin(t = 8)),
	  axis.text.y       = element_text(size = 25),
	  axis.title.x      = element_text(size = 25),
	  axis.title.y      = element_text(size = 25),
	  plot.title        = element_text(size = 25, hjust = 0.5, face = "plain"),
	  strip.text        = element_text(size = 25, face = "plain", hjust = 0.5),
	  strip.background  = element_blank(),
	  legend.position   = "top",
	  legend.direction  = "horizontal",
	  legend.box        = "horizontal",
	  legend.spacing.x  = unit(1.2, "cm"),
	  legend.spacing.y  = unit(0.5, "cm"),
	  legend.background = element_blank(),
	  legend.text       = element_text(size = 25, face = "plain"),
	  panel.spacing     = unit(1.5, "cm"),
	  plot.background   = element_blank()
	)
)

# print(gg2)

gg3 <- (ggplot(df, aes(x=interaction(mu,zeta),y=estimate))
	+ geom_point(size = 2.5)
	+ geom_pointrange(aes(ymin=estimate-2*std.error, ymax=estimate+2*std.error))
	+ facet_wrap(~mat,scale="free")
	+ coord_flip()
	+ labs(
	  title = NULL,
	  x     = NULL,
	  y     = NULL
	) 
	+ theme_clean() 
	  + theme(
	    axis.text.x       = element_text(size = 25,  hjust = 1, margin = margin(t = 8)),
	    axis.text.y       = element_text(size = 25),
	    axis.title.x      = element_text(size = 25),
	    axis.title.y      = element_text(size = 25),
	    plot.title        = element_text(size = 25, hjust = 0.5, face = "plain"),
	    strip.text        = element_text(size = 25, face = "plain", hjust = 0.5),
	    strip.background  = element_blank(),
	    legend.position   = "top",
	    legend.direction  = "horizontal",
	    legend.box        = "horizontal",
	    legend.spacing.x  = unit(1.2, "cm"),
	    legend.spacing.y  = unit(0.5, "cm"),
	    legend.background = element_blank(),
	    legend.text       = element_text(size = 25, face = "plain"),
	    panel.spacing     = unit(1.5, "cm"),
	    plot.background   = element_blank()
	  )
)

png("../figures/param_plot1.png", width = 7500, height = 5000, res = 300, bg = "white", type = "cairo")
print(gg)
dev.off()

png("../figures/param_plot2.png", width = 7500, height = 5500, res = 300, bg = "white", type = "cairo")
print(gg2)
dev.off()

png("../figures/param_plot3.png", width = 7500, height = 5000, res = 300, bg = "white", type = "cairo")
print(gg3)
dev.off()
