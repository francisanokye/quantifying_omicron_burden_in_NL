# # ==== Load libraries ====
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
# 
# 
loadEnvironments()
# 
# # ==== initialize ====
set.seed(2025)
options(macpan2_log_dir = ".")
loadEnvironments()
# 
# # inputs 
# calib_list <- readRDS("R1_grid_calibrate.rds")
# 
# start_date <- as.Date("2021-12-15")
# end_date   <- as.Date("2022-05-22")
# 
# # fixed parameters 
# kappa1 <- 1; kappa2 <- 0.91; kappa3 <- 0.3
# gamma_i <- 1/7; gamma_a <- 1/10
# p1 <- 0.15; p2 <- 0.85; p3 <- 0
# 
# # build trajectories for each draw ----
# grid_ts <- purrr::imap_dfr(calib_list, function(x, i){
#   
#   mu_x   <- x$cal_spec$default[["mu"]]
#   zeta_x <- x$cal_spec$default[["zeta"]]
#   
#   bracket_term   <- (mu_x/gamma_i) + ((1 - mu_x) * zeta_x / gamma_a)
#   susceptibility <- p1*kappa1 + p2*kappa2 + p3*kappa3
#   mult_const     <- bracket_term * susceptibility
#   
#   mp_trajectory_sd(x, conf.int = TRUE, back_transform = TRUE) %>%
#     dplyr::filter(time >= offset0, matrix == "beta_thing") %>%
#     dplyr::mutate(date = seq.Date(from = start_date, by = "1 day", length.out = dplyr::n())) %>%
#     dplyr::filter(date >= start_date, date <= end_date) %>%
#     dplyr::transmute(
#       draw = i,
#       mu = mu_x,
#       zeta = zeta_x,
#       date,
#       Re_t      = value     * mult_const,
#       Re_low    = conf.low  * mult_const,
#       Re_high   = conf.high * mult_const
#     )
# })
# 
# grid_df <- bind_rows(grid_ts)
# 
# gg <- (ggplot(grid_df,aes(date,Re_t,group=interaction(mu,zeta)))
#        + geom_line(aes(color=interaction(mu,zeta)))
#        ##+ geom_ribbon(aes(ymin = Re_low, ymax = Re_high, fill = interaction(mu,zeta)), alpha=0.2)
#        #	+ facet_grid(mu~zeta)
#        + geom_hline(aes(yintercept = 1.0),color="red", linewidth = 1.5)
#        + labs(
#          title = "Time-varying effective reproduction number across sensitivity draws",
#          x     = NULL,
#          y     = expression(R[e](t))
#        ) 
#        + theme_clean() 
#        + theme(
#          axis.text.x       = element_text(size = 25,  hjust = 1, margin = margin(t = 8)),
#          axis.text.y       = element_text(size = 25),
#          axis.title.x      = element_text(size = 25),
#          axis.title.y      = element_text(size = 25),
#          plot.title        = element_text(size = 25, hjust = 0.5, face = "plain"),
#          strip.text        = element_text(size = 25, face = "plain", hjust = 0.5),
#          strip.background  = element_blank(),
#          legend.position   = c(0.5, 0.4),
#          legend.direction  = "horizontal",
#          legend.box        = "horizontal",
#          legend.spacing.x  = unit(1.2, "cm"),
#          legend.spacing.y  = unit(0.5, "cm"),
#          legend.background = element_blank(),
#          legend.text       = element_text(size = 25, face = "plain"),
#          panel.spacing     = unit(1.5, "cm"),
#          plot.background   = element_blank()
#        )
# )
# 
# print(gg)
# 
# png("../figures/R1_sen_effreprod_noCI.png", width = 7500, height = 5000, res = 300, bg = "white", type = "cairo")
# print(gg)
# dev.off()

# ==== Load libraries ====


# inputs
calib_list <- readRDS("R1_grid_calibrate.rds")

start_date <- as.Date("2021-12-15")
end_date   <- as.Date("2022-05-22")

# fixed (as in your manuscript structure)
kappa1 <- 1
p1 <- 0.15; p2 <- 0.85; p3 <- 0

# ---- helper: safely pull a fitted coefficient from mp_tmb_coef() ----
get_fit <- function(fit_obj, par_name) {
  out <- mp_tmb_coef(fit_obj) %>%
    dplyr::filter(mat == par_name) %>%
    dplyr::slice(1) %>%
    dplyr::pull(estimate)
  if (length(out) == 0 || is.na(out)) stop("Missing fitted parameter: ", par_name)
  as.numeric(out)
}

# build trajectories for each draw ----
grid_df <- purrr::imap_dfr(calib_list, function(x, i){
  
  # mu, zeta vary by design (stored in cal_spec defaults)
  mu_x   <- x$cal_spec$default[["mu"]]
  zeta_x <- x$cal_spec$default[["zeta"]]
  
  # fitted parameters per draw (from your model fit)
  gamma_i_hat <- get_fit(x, "gamma_i")
  gamma_a_hat <- get_fit(x, "gamma_a")
  kappa2_hat  <- get_fit(x, "kappa2")
  kappa3_hat  <- get_fit(x, "kappa3")
  
  # multiplier for Re(t)
  bracket_term   <- (mu_x / gamma_i_hat) + ((1 - mu_x) * zeta_x / gamma_a_hat)
  susceptibility <- p1*kappa1 + p2*kappa2_hat + p3*kappa3_hat
  mult_const     <- bracket_term * susceptibility
  
  mp_trajectory_sd(x, conf.int = TRUE, back_transform = TRUE) %>%
    dplyr::filter(time >= offset0, matrix == "beta_thing") %>%
    dplyr::mutate(date = seq.Date(from = start_date, by = "1 day", length.out = dplyr::n())) %>%
    dplyr::filter(date >= start_date, date <= end_date) %>%
    dplyr::transmute(
      draw  = i,
      mu    = mu_x,
      zeta  = zeta_x,
      muzeta = interaction(paste0("mu=", mu_x), paste0("zeta=", zeta_x), sep = " | "),
      date,
      Re_t    = value     * mult_const,
      Re_low  = conf.low  * mult_const,
      Re_high = conf.high * mult_const
    )
})

# ===== Plot =====
gg <- ggplot(grid_df, aes(date, Re_t, group = interaction(mu, zeta))) +
  geom_line(aes(color = interaction(mu, zeta)), linewidth = 0.9) +
  # If you want CIs, turn this on (it can get busy with many draws):
  # geom_ribbon(aes(ymin = Re_low, ymax = Re_high, fill = interaction(mu, zeta)),
  #             alpha = 0.12, colour = NA) +
  geom_hline(yintercept = 1.0, color = "red", linewidth = 1.2) +
  labs(
    title = "Time-varying effective reproduction number across sensitivity draws",
    x     = NULL,
    y     = expression(R[e](t))
  ) +
  theme_clean() +
  theme(
    axis.text.x       = element_text(size = 30, hjust = 1, margin = margin(t = 8)),
    axis.text.y       = element_text(size = 30),
    axis.title.y      = element_text(size = 30),
    plot.title        = element_text(size = 35, hjust = 0.5, face = "plain"),
    legend.position   = c(0.5, 0.4),
    legend.direction  = "horizontal",
    legend.box        = "horizontal",
    legend.spacing.x  = unit(1.2, "cm"),
    legend.background = element_blank(),
    legend.text       = element_text(size = 30, face = "plain"),
    plot.background   = element_blank()
  )

png("../figures/R1_sen_effreprod_noCI.png",width = 7500, height = 5000, res = 300, bg = "white", type = "cairo")
print(gg)
dev.off()



# ===== Faceted plot =====
gg1 <- ggplot(grid_df, aes(date, Re_t)) +
  geom_line(linewidth = 0.9) +
  geom_ribbon(aes(ymin = Re_low, ymax = Re_high), alpha = 0.15, colour = NA) +
  geom_hline(yintercept = 1.0, color = "red", linewidth = 1.2) +
  facet_wrap(~muzeta, ncol = 5) +
  labs(
    title = "Time-varying effective reproduction number across sensitivity draws",
    x     = NULL,
    y     = expression(R[e](t))
  ) +
  theme_clean() +
  theme(
    axis.text.x       = element_text(size = 30, hjust = 1, margin = margin(t = 4)),
    axis.text.y       = element_text(size = 30),
    axis.title.y      = element_text(size = 30),
    plot.title        = element_text(size = 35, hjust = 0.5, face = "plain"),
    strip.text        = element_text(size = 30, face = "plain"),
    legend.position   = "none",
    panel.spacing     = unit(0.6, "cm"),
    plot.background   = element_blank()
  )

png("../figures/R1_sen_effreprod_facets.png",width = 7500, height = 5000, res = 300, bg = "white", type = "cairo")
print(gg1)
dev.off()
