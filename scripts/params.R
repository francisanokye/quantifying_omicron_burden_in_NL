library(shellpipes)
set.seed(2024)

#params <- c(beta = 1.4419624, kappa1 = 1, kappa2 = 0.91, kappa3 = 0.3,
params <- c(beta = 0.1, kappa1 = 1, kappa2 = 0.91, kappa3 = 0.3,
            sigma  =  1/3, gamma_a  =  1/10, gamma_i  =  1/7, mu = 0.324,
            zeta  =  0.75, vac2 = 0.09, vac3 = 0.0189,
            N2 = 434968, N = 510550, 
            A1 = 5, E1 = 0, I1 = 1, R1 = 50000,
            A2 = 4800, E2 = 0, I2 = 1, R2 = 50000,
            A3 = 10, E3 = 0, I3 = 1, R3 = 2000 
           )

saveEnvironment()

