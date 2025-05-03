library(macpan2)
library(shellpipes)
library(conflicted)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(broom.mixed)

set.seed(2025)

loadEnvironments()

timevar_spec <- rdsRead("timevar_spec.rds")

seroprevdata <- rdsRead("seroprevdata.rds")

outputs = c("S", "E", "A", "R", "C", "H", "I", "D","cases", "beta", "serop", "report_prob")

calibrator <- mp_tmb_calibrator(
    spec = timevar_spec |> mp_hazard()
  , data = seroprevdata
  , traj = list(
   # cases = mp_neg_bin(disp = mp_nofit(1.5)),
    serop = mp_normal(sd = mp_fit(0.5))
  )
  , outputs = c(outputs)
  , par = "beta"
  , tv = mp_rbf("beta", 6, sparse_tol = 0.0)
  , time = mp_sim_bounds(-off, 100-off, "daily")
)

mp_optimize(calibrator)

rdsSave(calibrator)










