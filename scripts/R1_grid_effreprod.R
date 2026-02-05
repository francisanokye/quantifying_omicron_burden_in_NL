# # # ==== Load libraries ====
library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(zoo)
library(ggthemes)
library(tidyverse)
library(macpan2)
library(shellpipes)
rpcall("R1_grid_effreprod.Rout R1_grid_effreprod.R R1_grid_calibrate.rds vacdat.rda")

set.seed(2025)
options(macpan2_log_dir = ".")
loadEnvironments()

# ==== Inputs ====
calib_list <- readRDS("R1_grid_calibrate.rds")
stopifnot(is.list(calib_list), length(calib_list) > 0)

anchor_start <- as.Date("2021-12-15")
anchor_end   <- as.Date("2022-05-22")

pars_keep <- c("gamma_a","gamma_i","sigma","kappa2","kappa3")

# fixed structure constants (as in manuscript)
kappa1 <- 1; p1 <- 0.15; p2 <- 0.85; p3 <- 0

# extract calibrated parameters per draw 
get_draw_params <- function(fit) {
  cf <- mp_tmb_coef(fit) %>%
    filter(mat %in% pars_keep) %>%
    transmute(param = mat, estimate = as.numeric(estimate)) %>%
    distinct(param, .keep_all = TRUE)
  
  # sanity check
  missing <- setdiff(pars_keep, cf$param)
  if (length(missing) > 0) stop("Missing fitted parameters: ", paste(missing, collapse = ", "))
  
  cf_wide <- cf %>%
    tidyr::pivot_wider(names_from = param, values_from = estimate)
  
  cf_wide
}

# build trajectories for each draw 
grid_df <- purrr::imap_dfr(calib_list, function(fit, draw_id){
  
  # design parameters stored in spec defaults
  mu_x   <- fit$cal_spec$default[["mu"]]
  zeta_x <- fit$cal_spec$default[["zeta"]]
  
  # calibrated parameters extracted from fit
  th <- get_draw_params(fit)
  
  gamma_a_hat <- th$gamma_a
  gamma_i_hat <- th$gamma_i
  sigma_hat   <- th$sigma
  kappa2_hat  <- th$kappa2
  kappa3_hat  <- th$kappa3
  
  # multiplier for Rc(t)
  bracket_term   <- (mu_x / gamma_i_hat) + ((1 - mu_x) * zeta_x / gamma_a_hat)
  susceptibility <- p1 * kappa1 + p2 * kappa2_hat + p3 * kappa3_hat
  mult_const     <- bracket_term * susceptibility
  
  # label used for grouping/faceting
  muzeta_lab <- paste0("mu=", signif(mu_x, 3), " | zeta=", signif(zeta_x, 3))
  
  mp_trajectory_sd(fit, conf.int = TRUE, back_transform = TRUE) %>%
    filter(time >= offset0, matrix == "beta_thing") %>%
    mutate(date = anchor_start + (time - offset0)) %>%
    filter(date >= anchor_start, date <= anchor_end) %>%
    transmute(
      draw   = draw_id,
      mu     = mu_x,
      zeta   = zeta_x,
      muzeta = muzeta_lab,
      
      # store calibrated params PER DRAW (requested)
      gamma_a = gamma_a_hat,
      gamma_i = gamma_i_hat,
      sigma   = sigma_hat,
      kappa2  = kappa2_hat,
      kappa3  = kappa3_hat,
      
      date,
      Rc_t    = value     * mult_const,
      Rc_low  = conf.low  * mult_const,
      Rc_high = conf.high * mult_const
    )
})

grid_df <- grid_df %>%
  mutate(muzeta = factor(muzeta, levels = unique(muzeta)))

p_combined <- ggplot(grid_df, aes(x = date, y = Rc_t, group = muzeta, color = muzeta, fill = muzeta)) +
  geom_ribbon(aes(ymin = Rc_low, ymax = Rc_high), alpha = 0.01, color = NA) +
  geom_line(show.legend = FALSE) +
  geom_hline(yintercept = 1.0, linewidth = 0.9, linetype = 2) +
  labs(title = "Time-varying control reproduction number across sensitivity draws", x = NULL,y = expression(R[c](t))) +
  theme_clean() +
  theme(
    axis.text.x       = element_text(size = 10, margin = margin(t = 5)),
    axis.text.y       = element_text(size = 10),
    axis.title.y      = element_text(size = 10),
    plot.title        = element_text(size = 15, hjust = 0.5, face = "plain"),
    plot.background   = element_blank(),
    legend.text = element_blank(),
    legend.position = "none"
  )

png("../figures/R1_sen_effreprod_noCI.png",width = 2500, height = 1000, res = 300, bg = "white", type = "cairo")
print(p_combined)
dev.off()

# Facets (with credible bands) 
p_facets <- ggplot(grid_df, aes(x = date, y = Rc_t, fill = muzeta)) +
  geom_ribbon(aes(ymin = Rc_low, ymax = Rc_high), alpha = 0.1, color = NA) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 1.0, linewidth = 0.9, linetype = 2) +
  facet_wrap(~ muzeta, ncol = 5, scales = "free_y") +
  labs(title = "Time-varying control reproduction number across sensitivity draws", x = NULL, y = expression(R[c](t))) +
  theme_clean() +
  theme(
    axis.text.x     = element_text(size = 20, margin = margin(t = 4)),
    axis.text.y     = element_text(size = 20),
    axis.title.y    = element_text(size = 20),
    strip.text      = element_text(size = 20, face = "plain", hjust = 0.5),
    legend.position = "none",
    panel.spacing   = unit(0.55, "cm"),
    plot.title        = element_text(size = 30, hjust = 0.5, face = "plain"),
    plot.background = element_blank()
  )

png("../figures/R1_sen_effreprod_facets.png",width = 7500, height = 5000, res = 300, bg = "white", type = "cairo")
print(p_facets)
dev.off()

# per-draw parameter table 
draw_params_tbl <- grid_df %>%
  distinct(draw, mu, zeta, gamma_a, gamma_i, sigma, kappa2, kappa3) %>%
  arrange(draw)

print(draw_params_tbl)
# write.csv(draw_params_tbl, "../output/R1_draw_calibrated_params.csv", row.names = FALSE)
