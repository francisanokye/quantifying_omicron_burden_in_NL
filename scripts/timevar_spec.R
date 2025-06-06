library(macpan2)
library(dplyr)
library(zoo)
library(shellpipes)
rpcall("timevar_spec.Rout timevar_spec.R flows.rda ../data/daily_2vac_rate.csv vacdat.Rout.csv params.rda")
loadEnvironments()

double_daily_vac= csvRead("daily_2vac_rate")

booster_daily_vac = csvRead("vacdat")

print(booster_daily_vac)

reporting_delay <- TRUE

spec <- mp_tmb_model_spec(
  before = list(N ~ N1 + N2 + N3
		, A1 ~ A10, E1 ~ E10, I1 ~ I10, R1 ~ R10
		, A2 ~ A20, E2 ~ E20, I2 ~ I20, R2 ~ R20
		, A3 ~ A30, E3 ~ E30, I3 ~ I30, R3 ~ R30
		, S1 ~ N1 - (E10 + A10 + I10 + R10)  
		, V2 ~ N2 - (E20 + A20 + I20 + R20) 
		, V3 ~ N3 - (E30 + A30 + I30 + R30)
  )
  , during = flows
  , default = c(params)
)
# we create a piecewise time-varying vaccination rate based on true data
double_vac_changepoints = double_daily_vac$days - 1 
double_vac_values = double_daily_vac$double_daily_rate

booster_vac_changepoints = booster_daily_vac$days - 1
booster_vac_values = booster_daily_vac$booster_daily_rate

expr2 = list(v2 ~ time_var(double_vac_values, double_vac_changepoints))
expr3 = list(v3 ~ time_var(booster_vac_values, booster_vac_changepoints))

# update model specification to reflect the vaccination rates from data
timevar_spec <- spec |> mp_tmb_insert(
      phase = "during"
    , at = 1
    , expressions = c(expr2, expr3)
    , default = list(double_vac_values = double_vac_values, booster_vac_values = booster_vac_values)
    , integers = list(double_vac_changepoints = double_vac_changepoints, booster_vac_changepoints = booster_vac_changepoints)
)

if(reporting_delay){
	timevar_spec = mp_tmb_insert_reports(timevar_spec
					     , incidence_name = "inc"
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

