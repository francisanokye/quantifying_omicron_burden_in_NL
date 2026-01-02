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
library(tidyverse);theme_set(theme_bw())
library(macpan2)

loadEnvironments()

# ==== initialize ====
set.seed(2025)
options(macpan2_log_dir = ".")
loadEnvironments()

calib_list   <- rdsRead()

simlist <- lapply(calib_list,function(x){
	df <- (mp_trajectory_sd(x,conf.int = TRUE, back_transform = TRUE)
  		|> filter(time >= offset0, matrix == "beta_thing")
  		|> mutate(date = seq.Date(from = as.Date("2021-12-15"), by = "1 day", length.out = n()))
  		|> filter(date >= as.Date("2021-12-15") & date <= as.Date("2022-05-22"))
		|> mutate(NULL
			, mu = x$cal_spec$default[["mu"]]
			, zeta = x$cal_spec$default[["zeta"]]
			)
		)
	}
)

grid_df <- bind_rows(simlist)

gg <- (ggplot(grid_df,aes(date,value))
	+ geom_line()
	+ geom_ribbon(aes(ymin=conf.low,ymax=conf.high),fill="gray",alpha=0.4)
	+ facet_grid(mu~zeta)
	+ geom_hline(aes(yintercept=0.5),color="red")
)

print(gg)

quit()







