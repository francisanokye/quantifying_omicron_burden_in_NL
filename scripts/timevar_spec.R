library(macpan2)
library(dplyr)
library(zoo)
library(shellpipes)
loadEnvironments()

effprop <- 0.9

reporting_delay <- TRUE

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

