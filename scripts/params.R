library(shellpipes)
set.seed(2024)

params <- c(beta = 1.4419624, kappa1 = 1, kappa2 = 0.91, kappa3 = 0.3,
            sigma  =  1/3, gamma_a  =  1/10, gamma_i  =  1/7, mu = 0.324,
            zeta  =  0.75, v2 = 0.09, v3 = 0.0189,
            N1 = 111300, N2 = 271612, N3 = 127638, 
            A10 = 5, E10 = 0, I10 = 1, R10 = 0,
            V20 = 271612, A20 = 5000, E20 = 0, I20 = 10, R20 = 0,
            V30 = 0, A30 = 10, E30 = 0, I30 = 1, R30 = 2000 
           )
saveEnvironment()

