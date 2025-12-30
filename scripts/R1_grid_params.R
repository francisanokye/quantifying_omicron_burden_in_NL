library(macpan2)
library(dplyr)
library(shellpipes)

calib_list <- rdsRead()

print((calib_list[[1]]))

quit()


df <- (mp_tmb_coef(calib_list[[1]])
	|> filter(mat %in% c("gamma_a","gamma_i","kappa2","kappa3","sigma"))
	|> transmute(NULL
		, mat
		, estimate
		, std.error
	)
)

print(df)
