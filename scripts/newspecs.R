library(shellpipes)
library(macpan2)

loadEnvironments()

newspecs <- mp_tmb_update(rdsRead(), default = newparams)

print(newspecs)

rdsSave(newspecs)



