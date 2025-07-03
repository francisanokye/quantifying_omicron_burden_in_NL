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

spline_beta = TRUE

timevar_spec <- rdsRead("timevar_spec_inc.rds")

seroprevdata <- (rdsRead("fitsero.rds")
 ## hack
 |> mutate(value = ifelse(is.na(value) & (matrix == "sero_inc"), 700, value))
)

time_steps = max(seroprevdata$time)

if (spline_beta) {
  basis_cols = 6
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
  , time = mp_sim_bounds(1, time_steps, "steps")
  , traj = list(
        sero_inc = mp_poisson()
      , serop = mp_poisson()
  )
  , par = list(
        log_beta = mp_uniform()
      , time_var_beta = mp_normal(0, 0.5)
      , log_R_initial = mp_uniform()
  )
  , outputs = c("beta","log_serop","log_sero_inc")
)
mp_optimize(calibrator)
sims = calibrator |> mp_trajectory_sd(conf.int = TRUE)
print(ggplot()
 + geom_point(aes(time, value), data = seroprevdata)
 #+ geom_line(aes(time, value), linewidth = 1, colour = "red", data = sims)
 + geom_ribbon(aes(x = time, ymin = conf.low, ymax = conf.high), alpha = 0.4, colour = "red", fill = "red", data = sims)
 + facet_wrap(~matrix, scales = "free", ncol = 1)
 + scale_y_continuous(name = "")
 + scale_x_continuous(limits = c(0, time_steps))
 + theme_bw()
)


print(calibrator
  |> mp_optimized_spec("modified")
  |> mp_simulator(time_steps, outputs = mp_state_vars(timevar_spec))
  |> mp_trajectory()
  |> ggplot()
  + aes(time, value)
  + geom_line()
  + facet_wrap(~matrix, scales = "free")
  #+ scale_x_continuous(limits = c(offset0, time_steps))
  + theme_bw()
)

remade_vax_data = list(
  
double_vac = data.frame(
    time = timevar_spec$integers$double_vac_changepoints
  , value = timevar_spec$default$double_vac_values
),
booster_shot = data.frame(
    time = timevar_spec$integers$booster_vac_changepoints
  , value = timevar_spec$default$booster_vac_values
)
) |> bind_rows(.id = "matrix")

print(calibrator
  |> mp_optimized_spec("modified")
  |> mp_simulator(time_steps, outputs = mp_flow_vars(timevar_spec))
  |> mp_trajectory()
  |> ggplot()
  + aes(time, value)
  + geom_line()
  + geom_point(data = remade_vax_data)
  + facet_wrap(~matrix, scales = "free")
  #+ scale_x_continuous(limits = c(offset0, time_steps))
  + theme_bw()
)

# extract fitted coeficients and print out
model_estimates = mp_tmb_coef(calibrator, conf.int = TRUE)
print(model_estimates, digits = 2)

rdsSave(calibrator)
