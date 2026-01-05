library(shellpipes)
rpcall("muzeta.newspecs.Rout newspecs.R R1_timevar_spec.rds muzeta.rda")
library(macpan2)

loadEnvironments()

newspecs <- mp_tmb_update(rdsRead(), default = newparams)

print(newspecs)

rdsSave(newspecs)



