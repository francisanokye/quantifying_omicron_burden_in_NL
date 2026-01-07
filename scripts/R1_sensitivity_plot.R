suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(macpan2)
  library(patchwork)
  library(shellpipes)
rpcall("R1_sensitivity_plot.Rout R1_sensitivity_plot.R R1_grid_params.rds R1_grid_calibrate.rds params.rda")
  library(ggthemes)
  library(grid)
  library(conflicted)
})

set.seed(2025)
options(macpan2_log_dir = ".")
loadEnvironments()

# inputs 
calib_list <- readRDS("R1_grid_calibrate.rds")

anchor_start <- as.Date("2021-12-15")
anchor_end   <- as.Date("2022-05-22")

# fixed parameters 
kappa1 <- 1; kappa2 <- 0.91; kappa3 <- 0.3
gamma_i <- 1/7; gamma_a <- 1/10
p1 <- 0.15; p2 <- 0.85; p3 <- 0

# build trajectories for each draw ----
grid_ts <- purrr::imap_dfr(calib_list, function(x, i){
  
  mu_x   <- x$cal_spec$default[["mu"]]
  zeta_x <- x$cal_spec$default[["zeta"]]
  
  bracket_term   <- (mu_x/gamma_i) + ((1 - mu_x) * zeta_x / gamma_a)
  susceptibility <- p1*kappa1 + p2*kappa2 + p3*kappa3
  mult_const     <- bracket_term * susceptibility
  
  mp_trajectory_sd(x, conf.int = TRUE, back_transform = TRUE) %>%
    dplyr::filter(time >= offset0, matrix == "beta_thing") %>%
    dplyr::mutate(date = seq.Date(from = anchor_start, by = "1 day", length.out = dplyr::n())) %>%
    dplyr::filter(date >= anchor_start, date <= anchor_end) %>%
    dplyr::transmute(
      draw = i,
      mu = mu_x,
      zeta = zeta_x,
      date,
      Re_t = value * mult_const
    )
})

# phase definitions 
grid_ts <- grid_ts %>%
  mutate(
    phase = case_when(
      date <= as.Date("2021-12-31") ~ "Early",
      date <  as.Date("2022-01-04") ~ "ALS-3\nK-12 Closed",
      date <  as.Date("2022-01-25") ~ "ALS-4\nK-12 Closed",
      date <  as.Date("2022-02-08") ~ "ALS-4\nK-12 Open",
      date <  as.Date("2022-03-15") ~ "ALS-3\nK-12 Open",
      TRUE                          ~ "No-ALS\nK-12 Open"
    )
  )

# mean Re(t) per draw x phase
phase_means <- grid_ts %>%
  group_by(draw, mu, zeta, phase) %>%
  summarise(mean_Re = mean(Re_t, na.rm = TRUE), .groups = "drop")

# differences vs Early (reference) 
diffs <- phase_means %>%
  group_by(draw) %>%
  mutate(early_mean = mean_Re[phase == "Early"][1]) %>%
  ungroup() %>%
  dplyr::filter(phase != "Early") %>%
  mutate(diff_vs_early = mean_Re - early_mean)

# =========================
# Panel A: Closed vs Open
# =========================
diffs_A <- diffs %>%
  mutate(k12 = if_else(str_detect(phase, "Closed"), "K-12 Closed", "K-12 Open")) %>%
  group_by(draw, mu, zeta, k12) %>%
  summarise(diff_vs_early = mean(diff_vs_early, na.rm = TRUE), .groups = "drop") %>%
  mutate(k12 = factor(k12, levels = c("K-12 Closed", "K-12 Open")))

pA <- ggplot(diffs_A, aes(x = k12, y = diff_vs_early)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8) +
  geom_boxplot(outlier.size = 0.5, linewidth = 0.4, fatten = 0.5) +
  labs(x = NULL, y = expression(Delta~mean~R[e](t)~"vs Early"), title = expression(
    "Effect of school status on "~Delta~mean~R[e](t)~" across sensitivity draws"
  )) +
  theme_clean() + 
  theme(axis.text.x = element_text(size = 8), 
        axis.title.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8), 
        axis.title.y = element_text(size = 8), 
        plot.title = element_text(size = 10, hjust = 0.5,face = "plain"), 
        legend.text = element_text(size = 8), 
        legend.background = element_rect(color = NA), 
        legend.position = "bottom", 
        plot.background = element_blank())+
  labs(tag = "A") +
  theme(
    plot.tag = element_text(size = 8, face = "bold", colour = "black"),
    plot.tag.position = c(0.01, 0.98)   
  )

# =========================
# Panel B: ALS levels 
# =========================
# Here we compare ALS-4 Closed vs ALS-3 Closed 
diffs_B <- diffs %>%
  dplyr::mutate(als = case_when(
    str_detect(phase, "ALS-4")  ~ "ALS-4",
    str_detect(phase, "ALS-3")  ~ "ALS-3",
    str_detect(phase, "No-ALS") ~ "No-ALS",
    TRUE                        ~ NA_character_
  )) %>%
  dplyr::filter(!is.na(als)) %>%
  dplyr::mutate(als = factor(als, levels = c("ALS-4", "ALS-3", "No-ALS")))

pB <- ggplot(diffs_B, aes(x = als, y = diff_vs_early)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8) +
  geom_boxplot(outlier.size = 0.5, linewidth = 0.4, fatten = 0.5) +
  labs(x = NULL, y = expression(Delta~mean~R[e](t)~"vs Early"), title = expression(
    "Effect of ALS level on "~Delta~mean~R[e](t)~" relative to Early period"
  ))+
  theme_clean() + 
  theme(
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_text(size = 10, hjust = 0.5, face = "plain"),
    legend.text = element_text(size = 10),
    legend.background = element_rect(color = NA),
    legend.position = "bottom",
    plot.background = element_blank()
  ) +
  labs(tag = "B") +
  theme(
    plot.tag = element_text(size = 8, face = "bold", colour = "black"),
    plot.tag.position = c(0.01, 0.98)
  )

pC <- pA / pB


png("../figures/R1_pA.png", width = 1500, height = 750, res = 300, bg = "white", type = "cairo")
pA
dev.off()

png("../figures/R1_pB.png", width = 1500, height = 750, res = 300, bg = "white", type = "cairo")
pB
dev.off()

png("../figures/R1_pC.png", width = 1500, height = 1000, res = 300, bg = "white", type = "cairo")
pC
dev.off()


