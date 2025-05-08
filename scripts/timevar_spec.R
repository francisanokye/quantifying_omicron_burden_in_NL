library(macpan2)
library(dplyr)
library(zoo)
library(shellpipes)
loadEnvironments()

effprop <- 0.9

reporting_delay <- TRUE

## offset hack step
reporting_probs = (csvRead()
	%>% mutate(Date = ifelse(Date > as.Date(start_date),Date + off,Date)
		, Date = as.Date(Date)
	)
)

report_prob_ts <- reporting_probs$prob
report_prob_cp = as.integer(reporting_probs$Date - as.Date(start_date))

nspec <- rdsRead()

timevar_spec <- mp_tmb_insert(nspec
	, expression = list(report_prob ~ time_var(report_prob_ts, report_prob_cp))
	, phase = "during", at = 1L
	, default = list(report_prob_ts = report_prob_ts)
	, integers = list(report_prob_cp = report_prob_cp)
)

if(reporting_delay){

timevar_spec = mp_tmb_insert_reports(timevar_spec
  , incidence_name = "exposure"
  , report_prob = 0.5
  , mean_delay = 11
  , cv_delay = 0.95
  , reports_name = "cases"
  , report_prob_name = "report_prob"
)

}
rdsSave(timevar_spec)
