library(macpan2)
library(shellpipes)
rpcall("calibrate_inc.Rout calibrate_inc.R timevar_spec_inc.rds fitsero.rds params.rda")
library(conflicted)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(broom.mixed)

set.seed(2025)
options(macpan2_log_dir = ".")
loadEnvironments()

timevar_spec <- rdsRead("timevar_spec_inc.rds")

seroprevdata <- (rdsRead("fitsero.rds")
 ## hack
 |> mutate(value = ifelse(is.na(value)&(matrix=="sero_inc"),700,value))
)

outputs = c("S","E","A","I","R",
	    "S1","E1","A1","I1","R1",
            "V2","E2","A2","I2","R2",
            "V3","E3","A3","I3","R3", 
	    "serop","beta","sero_inc",
	    "double_vac","booster_shot"
	   )

calibrator = mp_tmb_calibrator(
    spec = timevar_spec |> mp_hazard()
  , data = seroprevdata |> select(-date)
  , time = mp_sim_offset(0, 30, "steps")
  , outputs = c(outputs)
  , traj = list(sero_inc = mp_neg_bin(disp = mp_fit(0.1))
                , serop = mp_normal(sd=mp_fit(0.01))
               )
  , tv = mp_rbf("beta", 5, sparse_tol = 0)
  , par = c("beta", "log_R_offset")
)

mp_optimize(calibrator)

new_spec = mp_optimized_spec(calibrator, spec_structure = "modified")

# uncomment codes below to simulate with calibrated parameters to see model fit
#sim = mp_simulator(new_spec, 162, c("beta","serop","sero_inc"))
#print(sim |> mp_trajectory() |> ggplot()
#  + geom_line(aes(time, value))
#  + geom_point(aes(time, value), data = seroprevdata, colour = "red")
#  + facet_wrap(~matrix, scales = "free")
#)

# extract fitted coeficients and print out
model_estimates = mp_tmb_coef(calibrator, conf.int = TRUE)
print(model_estimates, digits = 2)

rdsSave(calibrator)















