library(shellpipes)
set.seed(2024)

### Why do you need so many boxes?

#beta_baseline = 0.28 # baseline transmission rate
#beta_deviation = 1   # multiplicative deviation from baseline transmission rate 
## MLi: Comment on what the parameters represent

beta <- 0.035
report_prob = 0.99

## infectivity reduction due to vaccine 
kappa1 = 1 
kappa2 = 0.91 
kappa3 = 0.3

## Recovery rate for asymptomatic individuals
gamma = 1/10 

## Mean days from exposure to symptom onset
sigma = 1/3

## Proportion of asymptomatic infections
mu = 0.324  

## Reduction in viral transmission due to asymptomatic case contact
zeta = 0.75

## Rate at which people took second and booster doses
v2 = 1/21 
v3 = 1/154 

## Proportion of symptomatic individuals hospitalized across cohorts
xi1 <- xi2 <- xi3 <- 0.009 
            
## Mean days from ICU to recovery
eta1 <- eta2 <- eta3 <-  1/5.5 

## Median time to hospitalization for symptomatic individuals across cohorts
phi1 <- phi2 <- phi3 <- 1/5 

## Proportion of hospitalized individuals admitted to ICU across cohorts
theta1 <- theta2 <- theta3 <- 0.005 #0.025
               
## Recovery rate of hospitalized infections
omega1 <- omega2 <- omega3 <- 1/7 

## Proportion of ICU patients progressing to death
lambda1 = 0.25 
lambda2 = 0.156 
lambda3 = 0.150

N = 510550
E10 = 0 
A10 = 1 
R10 = 0 
C10 = 0 
H10 = 0 
I10 = 0 
D10 = 0  

E20 = 0 
A20 = 1 
R20 = 0
C20 = 0 
H20 = 0 
I20 = 0 
D20 = 0 

E30 = 0 
A30 = 1 
R30 = 0 
C30 = 0 
H30 = 0 
I30 = 0 
D30 = 0

S0prop = 0.15
V2prop = 0.3
V3prop = 0.56

S10 = S0prop * N  - (E10 - A10 - R10 - C10 - H10 - I10 -D10)
V20 = V2prop * N  - (E20 - A20 - R20 - C20 - H20 - I20 -D20)
V30 = V3prop * N  - (E30 - A30 - R30 - C30 - H30 - I30 -D30) 

## offset
off <- 20

# params = list(beta_baseline = beta_baseline, beta_deviation = beta_deviation
params = list(beta= beta
	, report_prob = report_prob
	, kappa1 = kappa1
	, kappa2 = kappa2
	, kappa3 = kappa3
	, gamma = gamma
	, sigma = sigma
	, mu = mu
	, zeta = zeta
	## cohort-specific parameters
	, v2 = v2
	, v3 = v3
	, xi1 = xi1
	, xi2 = xi2
	, xi3 = xi3
	, eta1 = eta1
	, eta2 = eta2
	, eta3 = eta3
	, phi1 = phi1
	, phi2 = phi2
	, phi3 = phi3
	, theta1 = theta1
	, theta2 = theta2
	, theta3 = theta3
	, omega1 = omega1
	, omega2 = omega2
	, omega3 = omega3
	, lambda1 = lambda1
	, lambda2 = lambda2
	, lambda3 = lambda3
	, off = off
	, S0prop = S0prop
	, V2prop = V2prop
	, V3prop = V3prop
)

states = list(N = N
	, S1 = S10
	, E1 = E10
	, A1 = A10
	, R1 = R10
	, C1 = C10
	, H1 = H10
	, I1 = I10
	, D1 = D10

	, V2 = V20
	, E2 = E20
        , A2 = A20
        , R2 = R20
        , C2 = C20
        , H2 = H20
        , I2 = I20
	, D2 = D20

	, V3 = V30
        , E3 = E30
        , A3 = A30
        , R3 = R30
        , C3 = C30
        , H3 = H30
        , I3 = I30
        , D3 = D30
	## MLi: Probably don't need this here, MLi will simply later
	, S = S10 + V20 + V30
        , E = E10 + E20 + E30
        , A = A10 + A20 + A30
        , R = R10 + R20 + R30
        , C = C10 + C20 + C30
        , H = H10 + H20 + H30
        , I = I10 + I20 + I30
        , D = D10 + D20 + D30

	)

saveEnvironment()
