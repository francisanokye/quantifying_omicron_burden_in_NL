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
	    "serop","beta","inc",
#	    "beta","inc",
	    "double_vac","booster_shot"
	   )


calibrator = mp_tmb_calibrator(
    spec = timevar_spec |> mp_hazard()
  , data = seroprevdata |> select(-date)
#  , time = mp_sim_offset(0, 30, "steps")
  , time = mp_sim_offset(0, 30, "steps")
  , outputs = c(outputs)
  , traj = list(serop = mp_normal(sd = mp_fit(0.01))) # 0.015
#  , traj = list(inc = mp_neg_bin(disp = mp_fit(0.01))) # 0.015
#  , traj = list(inc = mp_poisson()) # 0.015
#  , tv = mp_rbf("beta", 5, fit_prior_sd = FALSE, prior_sd = 1)
  , tv = mp_rbf("beta", 5)

  , par = c("beta")
)

mp_optimize(calibrator)
if (interactive()) {
new_spec = mp_optimized_spec(calibrator, spec_structure = "modified")
sim = mp_simulator(new_spec, 162, c("beta", "serop"))
# sim = mp_simulator(new_spec, 162, c("beta", "inc"))
(sim |> mp_trajectory() |> ggplot()
  + geom_line(aes(time, value))
  + geom_point(aes(time, value), data = seroprevdata, colour = "red")
  + facet_wrap(~matrix, scales = "free")
)
}


model_estimates = mp_tmb_coef(calibrator, conf.int = TRUE)
print(model_estimates, digits = 2)


beta_changepoints <- c(0)
beta_change <- c(0.1)

newspecs <- (timevar_spec 
  |> mp_tmb_insert(phase = "during"
  , at = 1
#  , expressions = list(beta ~ time_var(c(beta0,beta_change,beta_last),beta_changepoints))
  , expressions = list(beta ~ time_var(c(beta0,beta_change),beta_changepoints))
#, expressions = list(beta ~ time_var(c(beta_change),beta_changepoints))

#  , default = list(beta_change = beta_change, beta0=0.22,beta_last=0.3)
, default = list(beta_change = beta_change, beta0=0.22)
#, default = list(beta_change = beta_change)
, integers = list(beta_changepoints = beta_changepoints) 
))

calibrator2 = mp_tmb_calibrator(
  spec = newspecs |> mp_hazard()
  , data = seroprevdata |> select(-dates)
  #  , time = mp_sim_offset(0, 30, "steps")
  , time = mp_sim_offset(0, 30, "steps")
  , outputs = c(outputs)
  #  , traj = list(serop = mp_normal(sd = mp_fit(0.01))) # 0.015
  , traj = list(inc = mp_neg_bin(disp = mp_fit(0.01))) # 0.015
  #  , traj = list(inc = mp_poisson()) # 0.015
  #  , tv = mp_rbf("beta", 5, fit_prior_sd = FALSE, prior_sd = 1)
  , par = c("beta_change")
)


mp_optimize(calibrator2)
if (interactive()) {
  new_spec = mp_optimized_spec(calibrator2, spec_structure = "modified")
  sim = mp_simulator(new_spec, 162, c("beta", "inc"))
  # sim = mp_simulator(new_spec, 162, c("beta", "inc"))
  (sim |> mp_trajectory() |> ggplot()
    + geom_line(aes(time, value))
    + geom_point(aes(time, value), data = seroprevdata, colour = "red")
    + facet_wrap(~matrix, scales = "free")
  )
}

print(calibrator2)

model_estimates = mp_tmb_coef(calibrator2, conf.int = TRUE)
print(model_estimates)

rdsSave(calibrator2)















