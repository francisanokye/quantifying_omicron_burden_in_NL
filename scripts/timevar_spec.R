library(macpan2)
library(dplyr)
library(zoo)
library(shellpipes)
loadEnvironments()

effprop <- 0.9

reporting_delay <- TRUE

## offset hack step
reporting_probs = (csvRead()
	%>% mutate(Date = ifelse(Date > as.Date(start_date),Date + off,Date), Date = as.Date(Date))
)

report_prob_ts <- reporting_probs$prob
report_prob_cp = as.integer(reporting_probs$Date - as.Date(start_date))

nspec <- rdsRead()

if(reporting_delay){

timevar_spec = mp_tmb_insert_reports(nspec
  , incidence_name = "inc"
  , report_prob = 1.0
  , mean_delay = 14
  , cv_delay = 0.95
  , reports_name = "inc"
  , report_prob_name = "report_prob"
)

}
rdsSave(timevar_spec)
