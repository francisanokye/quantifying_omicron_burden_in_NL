library(macpan2)
library(shellpipes)
rpcall("calibrate.Rout calibrate.R timevar_spec.rds seroprevdata.rds fitsero.rds params.rda")
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
fitserodata <- rdsRead("fitsero.rds")
time_steps = max(seroprevdata$time)
upper_plot_time = 300 #time_steps

## SW: i'm winging this ...
fitserodata = (seroprevdata 
  |> mutate(value = timevar_spec$default$N * (value - dplyr::lag(value)) / 7)
  |> mutate(matrix = "inc")
)

if (spline_beta) {
  basis_cols = 5
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
get_prior = function(trans) function(rng) {
  mp_normal(
      (trans(rng[1]) + trans(rng[2])) / 2
    , log((trans(rng[2]) - trans(rng[1])) / (2 * 1.96))
  )
}
priors = list(
      log_beta = mp_normal(log(0.25), log(1))
    , time_var_beta = mp_normal(0, log(1))
    , log_gamma_a = get_prior(log)(prior_range$gamma_a)
    , log_gamma_i = get_prior(log)(prior_range$gamma_i)
    , logit_kappa2 = get_prior(qlogis)(prior_range$kappa2)
    , logit_kappa3 = get_prior(qlogis)(prior_range$kappa3)
    , log_sigma = get_prior(log)(prior_range$sigma)
)
calibrator = mp_tmb_calibrator(
    spec = timevar_spec |> mp_rk4() # mp_hazard()
  , data = (seroprevdata 
      |> select(-date) 
      |> dplyr::filter(matrix == "serop") 
      |> mutate(matrix = "logit_serop", value = qlogis(value))
  )
  , time = mp_sim_bounds(1, time_steps)
  , traj = list(logit_serop = mp_normal(sd = mp_fit(1)))
  , par = priors
  , outputs = c("log_beta_thing", "log_inc", "logit_serop")
)
mp_optimize(calibrator)

sims = (calibrator
  |> mp_trajectory_sd(conf.int = TRUE, back_transform = TRUE)
  |> dplyr::filter(time >= offset0)
)

lim_dat = \(x) dplyr::filter(x, time <= upper_plot_time)
print(ggplot()
 + geom_point(aes(time, value), data = lim_dat(seroprevdata))
 + geom_point(aes(time, value), data = lim_dat(fitserodata))
 + geom_line(aes(time, value), linewidth = 1, data = lim_dat(sims))
 + geom_ribbon(aes(x = time, ymin = conf.low, ymax = conf.high), alpha = 0.4, data = lim_dat(sims))
 + facet_wrap(~matrix, scales = "free", ncol = 1)
 + scale_y_continuous(name = "")
 + scale_x_continuous(limits = c(offset0, upper_plot_time))
 + theme_bw()
)

print(calibrator
  |> mp_optimized_spec("modified")
  |> mp_simulator(time_steps, outputs = mp_state_vars(timevar_spec))
  |> mp_trajectory()
  |> lim_dat()
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
  |> mp_simulator(time_steps, outputs = c("double_vac","booster_shot"))#mp_flow_vars(timevar_spec))
  |> mp_trajectory()
  |> lim_dat()
  |> ggplot()
  + aes(time, value)
  + geom_line()
  + geom_point(data = remade_vax_data)
  + facet_wrap(~matrix, scales = "free")
  #+ scale_x_continuous(limits = c(offset0, time_steps))
  + theme_bw()
)

# extract fitted coeficients and print out
model_estimates = mp_tmb_coef(calibrator, conf.int = TRUE) |> dplyr::select(-term, -col, -type) |> mutate(mat = abbreviate(mat, minlength = 15))
print(model_estimates, digits = 2)

rdsSave(calibrator)
