library(shellpipes)
set.seed(2024)

#params <- c(beta = 1.4419624, kappa1 = 1, kappa2 = 0.91, kappa3 = 0.3,
params <- c(beta = 0.1, kappa1 = 1, kappa2 = 0.91, kappa3 = 0.3,
            sigma  =  1/3, gamma_a  =  1/10, gamma_i  =  1/7, mu = 0.324,
            zeta  =  0.75, 
            vac2 = 0.09, vac3 = 0.0189,
            vac2_const = 0.09, vac3_const = 0.0189,
            N2 = 434968, N = 510550, 
            A1 = 1, E1 = 1, I1 = 1, R1 = 1,
            A2 = 1, E2 = 1, I2 = 1, R2 = 1,
            A3 = 1, E3 = 1, I3 = 1, R3 = 1,
            R_initial = 1
           )

offset0 <- 150

saveEnvironment()

