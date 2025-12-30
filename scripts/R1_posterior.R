library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(zoo)
library(ggthemes)
library(cowplot)
library(patchwork)
library(fuzzyjoin)
library(shellpipes)
library(tidyverse)
library(macpan2)

# ==== initialize ====
set.seed(2025)
options(macpan2_log_dir = ".")
loadEnvironments()

calibrator   <- rdsRead("calibrate.rds")

print(mp_tmb_coef(calibrator)
	|> filter(mat %in% c("gamma_a","gamma_i","kappa2","kappa3","sigma"))
)



quit()

sims = (calibrator
   |> mp_trajectory_ensemble(n=50)
#   |> dplyr::filter(time >= offset0)
	|> filter(matrix %in% c("gamma_a","gamma_i","kappa2","kappa3","sigma"))
)

print(sims)






