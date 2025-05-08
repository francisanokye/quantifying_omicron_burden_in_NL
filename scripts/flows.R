library(macpan2)
library(shellpipes)
# This decribes the model flow of how individuals transiton across comaprtments and cohorts
flows = list(
#    beta ~ beta_baseline * beta_deviation
  foi ~  beta * (zeta * (A1 + A2 + A3) + (I1 + I2 + I3)) / N
  , mp_per_capita_flow("S1", "E1", exposure1 ~ kappa1 * foi)
  , mp_per_capita_flow("E1", "I1", infect_symp1 ~ sigma * mu)
  , mp_per_capita_flow("E1", "A1", infect_asymp1 ~ sigma * (1-mu))
  , mp_per_capita_flow("A1", "R1", asymp_recov1 ~ gamma)
  , mp_per_capita_flow("I1", "H1", symp_hosp1 ~ phi1 * xi1)
  , mp_per_capita_flow("I1", "R1", symp_recov1 ~ phi1 * (1-xi1))
  , mp_per_capita_flow("H1", "R1", hosp_recov1 ~ omega1 * (1-theta1))
  , mp_per_capita_flow("H1", "C1", hosp_icu1 ~ omega1 * theta1)
  , mp_per_capita_flow("C1", "R1", icu_recov1 ~ eta1 * (1-lambda1))
  , mp_per_capita_flow("C1", "D1", icu_dead1 ~ eta1 * lambda1)
  , mp_per_capita_flow("S1", "V2", s1_v2 ~ v2)
  
  , mp_per_capita_flow("V2", "E2", exposure2 ~ kappa2 * foi)
  , mp_per_capita_flow("E2", "I2", infect_symp2 ~ sigma * mu)
  , mp_per_capita_flow("E2", "A2", infect_asymp2 ~ sigma * (1-mu))
  , mp_per_capita_flow("A2", "R2", asymp_recov2 ~ gamma)
  , mp_per_capita_flow("I2", "H2", symp_hosp2 ~ phi2 * xi2)
  , mp_per_capita_flow("I2", "R2", symp_recov2 ~ phi2 * (1-xi2))
  , mp_per_capita_flow("H2", "R2", hosp_recov2 ~ omega2 * (1-theta2))
  , mp_per_capita_flow("H2", "C2", hosp_icu2 ~ omega2 * theta2)
  , mp_per_capita_flow("C2", "R2", icu_recov2 ~ eta2 * (1-lambda2))
  , mp_per_capita_flow("C2", "D2", icu_dead2 ~ eta2 * lambda2)
  , mp_per_capita_flow("V2", "V3", v2_v3 ~ v3)
  
  , mp_per_capita_flow("V3", "E3", exposure3 ~ kappa3 * foi)
  , mp_per_capita_flow("E3", "I3", infect_symp3 ~ sigma * mu)
  , mp_per_capita_flow("E3", "A2", infect_asymp3 ~ sigma * (1-mu))
  , mp_per_capita_flow("A3", "R3", asymp_recov3 ~ gamma)
  , mp_per_capita_flow("I3", "H3", symp_hosp3 ~ phi3 * xi3)
  , mp_per_capita_flow("I3", "R3", symp_recov3 ~ phi3 * (1-xi3))
  , mp_per_capita_flow("H3", "R3", hosp_recov3 ~ omega3 * (1-theta3))
  , mp_per_capita_flow("H3", "C3", hosp_icu3 ~ omega3 * theta3)
  , mp_per_capita_flow("C3", "R3", icu_recov3 ~ eta3 * (1-lambda3))
  , mp_per_capita_flow("C3", "D3", icu_dead3 ~ eta3 * lambda3)
  
  # compartment aggregates are made to avoid over-counting
  , S ~ S1 + V2 + V3
  , E ~ E1 + E2 + E3
  , A ~ A1 + A2 + A3
  , R ~ R1 + R2 + R3
  , C ~ C1 + C2 + C3
  , H ~ H1 + H2 + H3
  , I ~ I1 + I2 + I3
  , D ~ D1 + D2 + D3
 
  , exposure ~ exposure1 + exposure2 + exposure3
)

saveVars(flows)
