# ==== Load libraries ====
library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(zoo)
library(ggthemes)
library(cowplot)
library(patchwork)
library(fuzzyjoin)
library(shellpipes)
rpcall("R1_grid_inc.Rout R1_grid_inc.R R1_grid_calibrate.rds R1_fitsero.rds vacdat.rda")
library(tidyverse);theme_set(theme_bw())
library(macpan2)

loadEnvironments()

# ==== initialize ====
set.seed(2025)
options(macpan2_log_dir = ".")
loadEnvironments()

calib_list   <- rdsRead("R1_grid_calibrate.rds")
fitserodata <- rdsRead("R1_fitsero.rds")

print(fitserodata)

simlist <- lapply(calib_list,function(x){
	df <- (mp_trajectory_sd(x,conf.int = TRUE, back_transform = TRUE)
  		|> filter(time >= offset0, matrix == "newR")
  		|> mutate(date = seq.Date(from = as.Date("2021-12-15"), by = "1 day", length.out = n()))
  		|> filter(date >= as.Date("2021-12-15") & date <= as.Date("2022-05-22"))
		|> mutate(NULL
			, mu = x$cal_spec$default[["mu"]]
			, zeta = x$cal_spec$default[["zeta"]]
			)
		)
	}
)

grid_df <- (bind_rows(simlist)
	|> filter(date > as.Date("2021-12-17"))
)

print(grid_df)

gg <- (ggplot(grid_df,aes(date,value))
	+ geom_line(aes(color=interaction(mu,zeta),group=interaction(mu,zeta)))
	+ geom_ribbon(aes(ymin=conf.low,ymax=conf.high,fill=interaction(mu,zeta),group=interaction(mu,zeta)),alpha=0.2)
	+ facet_grid(mu~zeta)
	+ geom_point(data=fitserodata,aes(x=date,y=value),size=0.2)
)

print(gg)



