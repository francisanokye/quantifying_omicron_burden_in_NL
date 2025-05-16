library(macpan2)
library(shellpipes)
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

outputs = c("S", "E", "A", "R", "C", "H", "I", "D","inc", "beta", "serop", "report_prob")

seroprevdata <- (seroprevdata
	|> dplyr:::filter(matrix == "serop")# time <=80
)

calibrator <- mp_tmb_calibrator(
    spec = timevar_spec |> mp_hazard()
  , data = seroprevdata
  , traj = list(serop = mp_normal(sd = mp_fit(0.5)))
  , outputs = c(outputs)
  , par = "beta"
  , tv = mp_rbf("beta", 6, sparse_tol = 0.0)
  , time = mp_sim_bounds(-off, 100-off, "daily")
  # , time = mp_sim_bounds(1, 80, "daily")

  # doesn't work
  #  , time = mp_sim_offset(off, 0, "steps")
)

mp_optimize(calibrator)

rdsSave(calibrator)










