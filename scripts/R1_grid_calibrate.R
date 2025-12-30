library(macpan2)
library(shellpipes)
library(conflicted)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(broom.mixed)

# check macpan2 version
if (packageVersion("macpan2") < "2.5.0") {
  stop(
    "please install a new version of macpan2\n"
    , "https://canmod.github.io/macpan2/#installation"
  )
}

# set seed and options
set.seed(2025)
options(macpan2_log_dir = ".")
loadEnvironments()

spline_beta = TRUE

# load input data
temp_spec <- rdsRead("muzeta.newspecs.rds")
seroprevdata <- rdsRead("R1_seroprevdata.rds")
fitserodata <- rdsRead("R1_fitsero.rds")
time_steps = max(seroprevdata$time)
upper_plot_time = 300

### 

params_grid <- expand.grid(mu = seq(0.45, 0.85, length=2)
	, zeta = seq(0.4,1,length = 2)
)

print(params_grid)


calibrator_list <- lapply(1:nrow(params_grid), function(x){ 
	timevar_spec <- mp_tmb_update(temp_spec, default = params_grid[x,])

if (spline_beta) {
 	basis_cols = 11
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

# prior constructor
get_prior = function(trans) function(rng) {
	mp_normal(
		(trans(rng[1]) + trans(rng[2])) / 2
		, log((trans(rng[2]) - trans(rng[1])) / (2 * 1.96))
	)
}

# define priors
priors = list(log_beta = mp_normal(log(0.25), log(1))
	, time_var_beta = mp_normal(0, log(1))
	, log_gamma_a = get_prior(log)(prior_range$gamma_a)
	, log_gamma_i = get_prior(log)(prior_range$gamma_i)
	, logit_kappa2 = get_prior(qlogis)(prior_range$kappa2)
	, logit_kappa3 = get_prior(qlogis)(prior_range$kappa3)
	, log_sigma = get_prior(log)(prior_range$sigma)
)

# fit model using seroprevalence data
calibrator = mp_tmb_calibrator(
	spec = timevar_spec |> mp_rk4()
	, data = (fitserodata##  seroprevdata                   ## change here?!? 
		|> select(-date) 
		|> dplyr::filter(matrix == "newR") 
		|> mutate(matrix = "log_newR", value = log(value))
	)
	, time = mp_sim_bounds(1, time_steps)
	, traj = list(log_newR = mp_normal(sd = mp_fit(0.1)))
	, par = priors
	, outputs = c("log_beta_thing", "log_inc", "log_newR","logit_serop_total"
		,"gamma_a","gamma_i","kappa2","kappa3","sigma"
	)
)

# optimize fit
mp_optimize(calibrator)
return(calibrator)
}
)

# save fitted calibrator
rdsSave(calibrator_list)
