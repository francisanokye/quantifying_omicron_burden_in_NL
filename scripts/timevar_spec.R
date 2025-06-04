library(macpan2)
library(dplyr)
library(zoo)
library(shellpipes)
loadEnvironments()

reporting_delay <- TRUE

spec <- mp_tmb_model_spec(
  before = list(N ~ N1 + N2 + N3
		, A1 ~ A10, E1 ~ E10, I1 ~ I10, R1 ~ R10
		, V2 ~ V20, A2 ~ A20, E2 ~ E20, I2 ~ I20, R2 ~ R20
		, V3 ~ V30, A3 ~ A30, E3 ~ E30, I3 ~ I30, R3 ~ R30
		, S1 ~ N1 - (E10 + A10 + I10 + R10)  
		, V2 ~ N2 - (E20 + A20 + I20 + R20) 
		, V3 ~ N3 - (E30 + A30 + I30 + R30)
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

