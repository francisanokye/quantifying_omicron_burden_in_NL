library(macpan2)
library(shellpipes)
rpcall("calibrate.Rout calibrate.R timevar_spec.rds seroprevdata.rds params.rda timevar_spec.R")
library(conflicted)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(broom.mixed)

set.seed(2025)
options(macpan2_log_dir = ".")
loadEnvironments()

timevar_spec <- rdsRead("timevar_spec.rds")

seroprevdata <- rdsRead("seroprevdata.rds")

outputs = c("S","E","A","I","R",
	    "S1","E1","A1","I1","R1",
            "V2","E2","A2","I2","R2",
            "V3","E3","A3","I3","R3", 
#	    "serop","beta","inc",
	    "beta","inc",
	    "double_vac","booster_shot"
	   )

seroprevdata <- (seroprevdata
	|> dplyr:::filter(matrix == "inc")
)

print(seroprevdata, n=Inf)

calibrator = mp_tmb_calibrator(
    spec = timevar_spec |> mp_hazard()
  , data = seroprevdata
  , time = mp_sim_offset(0, 30, "steps")
  , outputs = c(outputs)
#  , traj = list(serop = mp_normal(sd = mp_fit(0.01))) # 0.015
  , traj = list(inc = mp_normal(sd = mp_fit(0.01))) # 0.015
  , tv = mp_rbf("beta", 7)
  , par = c("beta")
)


mp_optimize(calibrator)

model_estimates = mp_tmb_coef(calibrator, conf.int = TRUE)
print(model_estimates, digits = 3)

rdsSave(calibrator)















