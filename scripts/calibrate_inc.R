library(macpan2)
library(shellpipes)
rpcall("calibrate_inc.Rout calibrate_inc.R timevar_spec_inc.rds fitsero.rds params.rda")
library(conflicted)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(broom.mixed)

if (packageVersion("macpan2") < "2.5.0") {
  stop(
      "Please install a new version of macpan2\n"
    , "https://canmod.github.io/macpan2/#installation"
  )
}

set.seed(2025)
options(macpan2_log_dir = ".")
loadEnvironments()

beta_timevar_length = 200
spline_beta = TRUE

timevar_spec <- rdsRead("timevar_spec_inc.rds")

seroprevdata <- (rdsRead("fitsero.rds")
 ## hack
 |> mutate(value = ifelse(is.na(value)&(matrix=="sero_inc"),700,value))
)

## The start-date offset is embedded in the seroprev dataset.
## When generating this dataset, we begin the time column
## (which aligns observed data with simulation steps) at
## offset + 1 rather than 1.
#offset = min(seroprevdata$time) - 1
time_steps = max(seroprevdata$time)

if (spline_beta) {
  basis_cols = 4
  basis_rows = time_steps
  X = splines::ns(1:basis_rows
    , basis_cols
    , intercept = TRUE
    , Boundary.knots = c(offset0, basis_rows)
  )
  timevar_spec = mp_tmb_insert_glm_timevar(timevar_spec
    , parameter_name = "beta"
    , design_matrix = X
    , timevar_coef = rep(0, basis_cols)
    , link_function = mp_log
  )
}

# shift = function(x, off) c(0, x[-1] + off)
# timevar_spec = (timevar_spec
#   |> mp_tmb_update(
#     integers = within(timevar_spec$integers, {
#         double_vac_changepoints <- shift(double_vac_changepoints, offset)
#         booster_vac_changepoints <- shift(booster_vac_changepoints, offset)
#     })
#   )
# )


outputs = c("S","E","A","I","R",
	    "S1","E1","A1","I1","R1",
            "V2","E2","A2","I2","R2",
            "V3","E3","A3","I3","R3", 
	    "serop","beta","sero_inc",
	    "double_vac","booster_shot"
	   )

calibrator = mp_tmb_calibrator(
    spec = timevar_spec |> mp_euler()
  , data = seroprevdata |> select(-date)
  , time = mp_sim_bounds(1, time_steps, "steps")
  , traj = list(
        sero_inc = mp_poisson()
      , serop = mp_poisson()
  )
  , par = list(
        log_beta = mp_uniform()
      , time_var_beta = mp_uniform()
  )
)
mp_optimize(calibrator)

new_spec = mp_optimized_spec(calibrator, spec_structure = "modified")

# uncomment codes below to simulate with calibrated parameters to see model fit
sim = mp_simulator(new_spec, time_steps, c("beta","serop","sero_inc"))
print(sim |> mp_trajectory() |> ggplot()
 + geom_line(aes(time, value), linewidth = 3)
 + geom_point(aes(time, value), data = seroprevdata, colour = "red")
 + facet_wrap(~matrix, scales = "free")
)

# extract fitted coeficients and print out
model_estimates = mp_tmb_coef(calibrator, conf.int = TRUE)
print(model_estimates, digits = 2)

rdsSave(calibrator)
