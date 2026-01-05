library(shellpipes)
rpcall("muzeta.Rout muzeta.R params.rda")

loadEnvironments()

print(params)

newparams <- list(mu = 0.5
	, zeta = 0.5
)

saveEnvironment()
