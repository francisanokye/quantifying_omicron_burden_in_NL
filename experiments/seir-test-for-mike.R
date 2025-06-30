library(macpan2); library(dplyr); library(tidyr); library(ggplot2); library(splines)
seir = (mp_tmb_library("starter_models", "seir", package = "macpan2")
  |> mp_tmb_insert("before", at = 1L, expressions = list(beta ~ beta0), default = list(beta0 = 0.2))
)
time_steps = 100
basis_cols = 4
X = splines::ns(1:time_steps
  , basis_cols
  , intercept = FALSE
)
seir_tv = mp_tmb_insert_glm_timevar(seir
  , parameter_name = "beta"
  , design_matrix = X
  , timevar_coef = c(1.3, -3.3, 1.2, 1.1)
  , link_function = mp_log
  , timevar_coef_name = "beta_values"
)
seir_tv_sim = (seir_tv
  |> mp_simulator(time_steps, c("I", "beta"))
  |> mp_trajectory()
)
calibrator = mp_tmb_calibrator(seir_tv
  , data = seir_tv_sim
  , traj = list(I = mp_poisson())
  , par = list(
      beta_values = mp_normal(0, 5)
    , log_beta0 = mp_normal(log(0.1), 1)
  )
  , default = list(
      beta_values = rep(0, basis_cols)
  )
  , outputs = c("log_I", "log_beta")
)

mp_optimize(calibrator)
seir_tv_fit = mp_trajectory_sd(calibrator, conf.int = TRUE)
plot = (ggplot()
  + geom_line(aes(time, value), data = seir_tv_sim)
  + geom_line(aes(time, value), data = seir_tv_fit, colour = "red")
  + geom_ribbon(aes(time, ymin = conf.low, ymax = conf.high)
    , data = seir_tv_fit
    , colour = "red"
    , alpha = 0.4
    , fill = "red"
  )
  + facet_wrap(~matrix, scales = 'free')
  + theme_bw()
)
print(plot)

model_estimates = mp_tmb_coef(calibrator, conf.int = TRUE)
print(model_estimates, digits = 2) |> select(-term, -type, -row, -col)
