library(macpan2)
library(shellpipes)
rpcall("calibrate.Rout calibrate.R timevar_spec.rds seroprevdata.rds params.rda")
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

timevar_spec <- rdsRead("timevar_spec.rds")

seroprevdata <- rdsRead("seroprevdata.rds")
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

calibrator = mp_tmb_calibrator(
    spec = timevar_spec |> mp_hazard()
  , data = seroprevdata |> select(-date)
  , time = mp_sim_bounds(1, time_steps, "steps")
  , traj = list(serop = mp_poisson())
  , par = list(
        log_beta = mp_normal(log(0.25), 0.01)
      , time_var_beta = mp_normal(0, 0.5)
      #, log_R_initial = mp_uniform()
  )
  , outputs = c("log_beta_thing", "log_inc", "logit_serop")
)
mp_optimize(calibrator)
priors = list(
      log_beta = mp_normal(log(0.25), 0.01)
    , time_var_beta = mp_normal(0, 0.5)
    #, log_R_initial = mp_uniform()
    , log_gamma_i = mp_uniform()
    , log_gamma_a = mp_uniform()
    #, logit_kappa2 = mp_uniform()
    #, logit_kappa3 = mp_uniform()
    #, log_sigma = mp_uniform()
)
model_traj = mp_optimized_spec(calibrator, "modified") |> mp_simulator(time_steps, outputs = c("serop", "inc")) |> mp_trajectory()
fake_cal = mp_tmb_calibrator(
    spec = timevar_spec |> mp_hazard()
  , data = model_traj
  , time = mp_sim_bounds(1, time_steps, "steps")
  , traj = list(serop = mp_poisson())
  , par = priors
  #, outputs = c("log_beta_thing", "log_inc", "logit_serop")
  , outputs = c("log_beta_thing", "log_serop", "log_inc")
)
mp_optimize(fake_cal)

fake_sims = fake_cal |> mp_trajectory_sd(conf.int = TRUE, back_transform = TRUE)
real_sims = calibrator |> mp_trajectory_sd(conf.int = TRUE, back_transform = TRUE)
plot_data = bind_rows(list(fake = fake_sims, real = real_sims), .id = "type")
print(ggplot()
 + geom_point(aes(time, value), data = plot_data)
 + geom_line(aes(time, value, colour = type), linewidth = 1, data = plot_data)
 + geom_ribbon(aes(x = time, ymin = conf.low, ymax = conf.high, fill = type), alpha = 0.4, data = plot_data)
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
