library(macpan2); library(dplyr); library(tidyr); library(ggplot2); library(splines)
seir = (mp_tmb_library("starter_models", "seir", package = "macpan2")
  |> mp_tmb_insert("before", at = 1L
    , expressions = list(beta ~ beta0)
    , default = list(beta0 = 0.2)
  )
)
time_steps = 50
end_steps = 0
spline_times = 1:(time_steps - end_steps)
basis_cols = 4
X = splines::ns(spline_times
  , basis_cols
  , intercept = FALSE
)[c(spline_times, rep(max(spline_times), end_steps)),]
seir_tv = mp_tmb_insert_glm_timevar(seir
  , parameter_name = "beta"
  , design_matrix = X
  
  ## these spline weights generate infection and I 
  ## data streams that can be used to recover these
  ## spline weights -- i.e., it indentifiable.
  ## if you only fit to infection _or_ I you are
  ## back in trouble. it might be interesting that 
  ## these coefficients lead to two infection waves.
  , timevar_coef = c(
        -2.38488006910888
      , 0.694651718879189
      , 4.07386373401663
      , -1.33038785414563
  )
  
  , link_function = mp_logit
  , timevar_coef_name = "beta_values"
)
seir_tv_sim = (seir_tv
  |> mp_simulator(time_steps, c("infection", "I", "beta"))
  |> mp_trajectory()
)
calibrator = mp_tmb_calibrator(seir_tv
  , data = seir_tv_sim
  , traj = list(
      I = mp_poisson()
    , infection = mp_poisson()
  )
  , par = list(
      beta_values = mp_uniform()
    , logit_beta0 = mp_uniform()
    , log_gamma = mp_uniform()
    , log_alpha = mp_uniform()
  )
  , default = list(
      beta_values = rep(0, basis_cols)
  )
  , outputs = c(
        "log_infection"
      , "logit_beta"
      , mp_state_vars(seir_tv, trans = "log")
  )
)

mp_optimize(calibrator)
seir_tv_fit = mp_trajectory_sd(calibrator, conf.int = TRUE)
plot = (ggplot()
  + geom_line(aes(time, value), data = seir_tv_fit, colour = "red")
  + geom_ribbon(aes(time, ymin = conf.low, ymax = conf.high)
    , data = seir_tv_fit
    , colour = "red"
    , alpha = 0.4
    , fill = "red"
  )
  + geom_line(aes(time, value), data = seir_tv_sim)
  + facet_wrap(~matrix, scales = 'free')
  + theme_bw()
)
print(plot)

model_estimates = mp_tmb_coef(calibrator, conf.int = TRUE)
print(model_estimates, digits = 2) |> select(-term, -type, -row, -col)
