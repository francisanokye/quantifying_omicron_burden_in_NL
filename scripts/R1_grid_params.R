library(macpan2)
library(dplyr)
library(shellpipes)
rpcall("R1_grid_params.Rout R1_grid_params.R R1_grid_calibrate.rds")

calib_list <- rdsRead()

## cal_spec$default[["mu"]]

dflist <- lapply(calib_list,function(x){
	df <- (mp_tmb_coef(x)
		|> filter(mat %in% c("gamma_a","gamma_i","kappa2","kappa3","sigma"))
		|> transmute(NULL
			, mat
			, estimate
			, std.error
			, mu = x$cal_spec$default[["mu"]]
			, zeta = x$cal_spec$default[["zeta"]]
		)
	)
	}
)

grid_df <- bind_rows(dflist)

print(grid_df)

rdsSave(grid_df)
