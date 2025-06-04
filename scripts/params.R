
library(shellpipes)
set.seed(2024)

params <- c(beta = 1.4419624, kappa1 = 1, kappa2 = 0.91, kappa3 = 0.3,
            sigma  =  1/3, gamma_a  =  1/10, gamma_i  =  1/7, mu = 0.324,
            zeta  =  0.75, v2_max = 287467, v3_max = 350000, v2 = 0.09, v3 = 0.0189,
            N1 = 111300, N2 = 271612, N3 = 127638, 
            A10 = 4500, E10 = 0, I10 = 1, R10 = 0,
            V20 = 271612, A20 = 0, E20 = 0, I20 = 1, R20 = 1500,
            V30 = 1, A30 = 0, E30 = 0, I30 = 1, R30 = 0, 
	    L2 = 444209, K2 = 479352,r2 = 0.07, mid2 = 58
           )
saveEnvironment()

