library(macpan2)
library(shellpipes)
# This decribes the model flow of how individuals transiton across comaprtments and cohorts
flows = list(
  foi ~  beta * (zeta * (A1 + A2 + A3) + (I1 + I2 + I3)) / N
  ## do we still do it this way?
  , mp_per_capita_flow("S1", "E1", "kappa1 * foi", "incS")
  , mp_per_capita_flow("E1", "I1", "sigma * mu", "infect_symp1")
  , mp_per_capita_flow("E1", "A1", "sigma * (1-mu)", "infect_asymp1")
  , mp_per_capita_flow("A1", "R1", "gamma", "asymp_recov1")
  , mp_per_capita_flow("I1", "H1", "phi1 * xi1", "symp_hosp1")
  , mp_per_capita_flow("I1", "R1", " phi1 * (1-xi1)", "symp_recov1")
  , mp_per_capita_flow("H1", "R1", "omega1 * (1-theta1)", "hosp_recov1")
  , mp_per_capita_flow("H1", "C1", "omega1 * theta1", "hosp_icu1")
  , mp_per_capita_flow("C1", "R1", "eta1 * (1-lambda1)", "icu_recov1")
  , mp_per_capita_flow("C1", "D1", "eta1 * lambda1", "icu_dead1")
  , mp_per_capita_flow("S1", "V2", "v2*(1-S1/v2_max)", "s1_v2")
  
  , mp_per_capita_flow("V2", "E2", "kappa2 * foi","incv2")
  , mp_per_capita_flow("E2", "I2", "sigma * mu", "infect_symp2")
  , mp_per_capita_flow("E2", "A2", "sigma * (1-mu)", "infect_asymp2")
  , mp_per_capita_flow("A2", "R2", "gamma", "asymp_recov2")
  , mp_per_capita_flow("I2", "H2", "phi2 * xi2", "symp_hosp2")
  , mp_per_capita_flow("I2", "R2", "phi2 * (1-xi2)", "symp_recov2")
  , mp_per_capita_flow("H2", "R2", "omega2 * (1-theta2)", "hosp_recov2")
  , mp_per_capita_flow("H2", "C2", "omega2 * theta2", "hosp_icu2")
  , mp_per_capita_flow("C2", "R2", "eta2 * (1-lambda2)", "icu_recov2")
  , mp_per_capita_flow("C2", "D2", "eta2 * lambda2", "icu_dead2")
  , mp_per_capita_flow("V2", "V3", "v3*(1-V2/v3_max)", "v2_v3")
  
  , mp_per_capita_flow("V3", "E3", "kappa3 * foi","incv3")
  , mp_per_capita_flow("E3", "I3", "sigma * mu", "infect_symp3")
  , mp_per_capita_flow("E3", "A2", "sigma * (1-mu)", "infect_asymp3")
  , mp_per_capita_flow("A3", "R3", "gamma", "asymp_recov3")
  , mp_per_capita_flow("I3", "H3", "phi3 * xi3", "symp_hosp3")
  , mp_per_capita_flow("I3", "R3", "phi3 * (1-xi3)", "symp_recov3")
  , mp_per_capita_flow("H3", "R3", "omega3 * (1-theta3)", "hosp_recov3")
  , mp_per_capita_flow("H3", "C3", "omega3 * theta3", "hosp_icu3")
  , mp_per_capita_flow("C3", "R3", "eta3 * (1-lambda3)", "icu_recov3")
  , mp_per_capita_flow("C3", "D3", "eta3 * lambda3", "icu_dead3")
  
  # compartment aggregates are made to avoid over-counting
  , S ~ S1 + V2 + V3
  , E ~ E1 + E2 + E3
  , A ~ A1 + A2 + A3
  , R ~ R1 + R2 + R3
  , C ~ C1 + C2 + C3
  , H ~ H1 + H2 + H3
  , I ~ I1 + I2 + I3
  , D ~ D1 + D2 + D3
 
  , inc ~ incS + incv2 + incv3
)

saveVars(flows)
