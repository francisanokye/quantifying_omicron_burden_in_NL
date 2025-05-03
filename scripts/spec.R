library(macpan2)
library(shellpipes)
loadEnvironments()

spec <- mp_tmb_model_spec(
  before = list(
      N ~ N
    , E1 ~ exp(log_E10)
    , A1 ~ exp(log_A10)
    , R1 ~ exp(log_R10)
    , C1 ~ exp(log_C10)
    , H1 ~ exp(log_H10)
    , I1 ~ exp(log_I10)
    , D1 ~ exp(log_D10)

    , E2 ~ exp(log_E20)
    , A2 ~ exp(log_A20)
    , R2 ~ exp(log_R20)
    , C2 ~ exp(log_C20)
    , H2 ~ exp(log_H20)
    , I2 ~ exp(log_I20)
    , D2 ~ exp(log_D20)

    , E3 ~ exp(log_E30)
    , A3 ~ exp(log_A30)
    , R3 ~ exp(log_R30)
    , C3 ~ exp(log_C30)
    , H3 ~ exp(log_H30)
    , I3 ~ exp(log_I30)
    , D3 ~ exp(log_D30)

    , S1  ~ S0prop * N  - (E1 - A1 - R1 - C1 - H1 - I1 - D1)
    , V2  ~ V2prop * N  - (E2 - A2 - R2 - C2 - H2 - I2 - D2)
    , V3  ~ V3prop * N  - (E3 - A3 - R3 - C3 - H3 - I3 - D3))
  , during = flows
  , default = c(params)
)

newspec <- mp_tmb_update(spec
			 , default = list(kappa1 = kappa1, kappa2 = kappa2, kappa3 = kappa3, gamma = gamma, sigma = sigma, mu = mu,
              zeta = zeta, v2 = v2, v3 = v3, xi1 = xi1, xi2 = xi2, xi3 = xi3, eta1 = eta1, eta2 = eta2, eta3 = eta3,
              phi1 = phi1, phi2 = phi2, phi3 = phi3, theta1 = theta1, theta2 = theta2, theta3 = theta3,
              omega1 = omega1, omega2 = omega2, omega3 = omega3, lambda1 = lambda1, lambda2 = lambda2, lambda3 = lambda3
				  , S0prop = S0prop
				  , V2prop = V2prop
				  , V3prop = V3prop
	      , N = N
	      , log_E10 = log(E10)
	      , log_A10 = log(A10)
	      , log_R10 = log(R10)
	      , log_C10 = log(C10)
	      , log_H10 = log(H10)
	      , log_I10 = log(I10)
	      , log_D10 = log(D10)

	      , log_E20 = log(E20)
              , log_A20 = log(A20)
              , log_R20 = log(R20)
              , log_C20 = log(C20)
              , log_H20 = log(H20)
              , log_I20 = log(I20)
              , log_D20 = log(D20)

	      , log_E30 = log(E30)
              , log_A30 = log(A30)
              , log_R30 = log(R30)
              , log_C30 = log(C30)
              , log_H30 = log(H30)
              , log_I30 = log(I30)
              , log_D30 = log(D30))
			)

## accumulate infections
nspec <- mp_tmb_insert(newspec
  , expression = list(serop ~ (R/N))
  , at = Inf
  , phase = "during"
)

rdsSave(nspec)
