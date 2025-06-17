library(macpan2)
library(dplyr)
library(zoo)
library(shellpipes)
rpcall("timevar_spec.Rout timevar_spec.R flows.rda ../data/daily_2vac_rate.csv vacdat.Rout.csv params.rda")
loadEnvironments()

dat <- csvRead()

double_daily_vac = dat |> filter(type == "numtotal_fully")
booster_daily_vac = dat |> filter(type == "numtotal_additional")


reporting_delay <- TRUE

initialize_state = list(
    V2 ~ (N2 - A2 - E2 - I2 - R2)
  , V3 ~ 0
  , S1 ~ (N - A1 - E1 - I1 - R1 - V2 - A2 - E2 - I2 - R2 - V3 - A3 - E3 - I3 - R3)
)

constant_computations = list(N ~ sum(S1, A1, E1, I1, R1, V2, A2, E2, I2, R2, V3, A3, E3, I3, R3))

spec <- mp_tmb_model_spec(
	before = c(initialize_state, constant_computations) 
	, during = flows
	, default = params
)

# we create a piecewise time-varying vaccination rate based on true data
double_vac_changepoints = double_daily_vac$days - 1 
double_vac_values = double_daily_vac$daily_rate

booster_vac_changepoints = booster_daily_vac$days - 1
booster_vac_values = booster_daily_vac$daily_rate

expr2 = list(vac2 ~ time_var(double_vac_values, double_vac_changepoints))
expr3 = list(vac3 ~ time_var(booster_vac_values, booster_vac_changepoints))

# update model specification to reflect the vaccination rates from data
timevar_spec <- spec |> mp_tmb_insert(
      phase = "during"
    , at = 1
    , expressions = c(expr2, expr3)
    , default = list(double_vac_values = double_vac_values, booster_vac_values = booster_vac_values)
    , integers = list(double_vac_changepoints = double_vac_changepoints, booster_vac_changepoints = booster_vac_changepoints)
)

## inserting betas?




if(reporting_delay){
	timevar_spec = mp_tmb_insert_reports(timevar_spec
					     , incidence_name = "inc_total"
					     , report_prob = 1.0
					     , mean_delay = 14
					     , cv_delay = 0.95
					     , reports_name = "inc"
					     , report_prob_name = "report_prob"
	)
}

print(timevar_spec)
timevar_spec$integers$double_vac_changepoints

rdsSave(timevar_spec)

