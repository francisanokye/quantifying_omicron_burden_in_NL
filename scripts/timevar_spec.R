library(macpan2)
library(dplyr)
library(zoo)
library(shellpipes)
loadEnvironments()

effprop <- 0.9

reporting_delay <- TRUE

spec <- mp_tmb_model_spec(
  before = list(N1 ~ N1
		, N2 ~ N2
		, N3 ~ N3
		, A1 ~ A10
	        , E1 ~ E10
		, I1 ~ I10
		, R1 ~ R10
		, A2 ~ A20
		, E2 ~ E20
		, I2 ~ I20
		, R2 ~ R20
		, A3 ~ A30
		, E3 ~ E30
		, I3 ~ I30
		, R3 ~ R30
		, S1 ~ N1 - E1 - A1 - I1 - R1
		, V2 ~ N2 - E2 - A2 - I2 - R2
		, V3 ~ N3 - E3 - A3 - I3 - R3
  )
  , during = flows
  , default = c(params)
)

if(reporting_delay){
	timevar_spec = mp_tmb_insert_reports(spec
					     , incidence_name = "inc"
					     , report_prob = 1.0
					     , mean_delay = 14
					     , cv_delay = 0.95
					     , reports_name = "inc"
					     , report_prob_name = "report_prob"
	)
}

rdsSave(timevar_spec)

