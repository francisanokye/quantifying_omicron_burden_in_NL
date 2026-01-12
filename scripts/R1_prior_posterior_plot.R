suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(macpan2)
  library(ggthemes)
  library(grid)
  library(shellpipes)
rpcall("R1_prior_posterior_plot.Rout R1_prior_posterior_plot.R R1_grid_calibrate.rds params.rda")
})

set.seed(2025)

# =============================
# Inputs / configuration
# =============================
pars_keep <- c("gamma_a","gamma_i","sigma","kappa2","kappa3")  # sigma after rates
n_prior        <- 20000
n_post_per_fit <- 800
z <- qnorm(0.975)

# Safe unicode labels (no parsing)
lab_unicode <- c(
  gamma_a = "γₐ",
  gamma_i = "γᵢ",
  kappa2  = "κ₂",
  kappa3  = "κ₃",
  sigma   = "σ"
)

# Strip titles for density facets: unicode + units only where relevant
strip_titles <- c(
  gamma_a = paste0(lab_unicode[["gamma_a"]], " (per day)"),
  gamma_i = paste0(lab_unicode[["gamma_i"]], " (per day)"),
  sigma   = paste0(lab_unicode[["sigma"]],   " (per day)"),
  kappa2  = lab_unicode[["kappa2"]],
  kappa3  = lab_unicode[["kappa3"]]
)

# ~95% prior intervals used in your model
prior_range <- list(
  kappa2  = c(0.85, 0.95),
  kappa3  = c(0.2,  0.4),
  gamma_a = c(1/11, 1/8),
  gamma_i = c(1/8,  1/6),
  sigma   = c(1/4,  1/2)
)

# Publication-friendly colors (Okabe–Ito)
cols <- c(Prior = "blue", Posterior = "orange")

# =============================
# Prior constructors (match your model)
# =============================
prior_norm_from_range <- function(rng, trans) {
  m  <- (trans(rng[1]) + trans(rng[2])) / 2
  sd <- (trans(rng[2]) - trans(rng[1])) / (2 * 1.96)
  if (!is.finite(sd) || sd <= 0) stop("Bad prior SD from range: ", paste(rng, collapse = ","))
  list(mean = m, sd = sd)
}

r_prior_param <- function(param, n) {
  if (param %in% c("gamma_a","gamma_i","sigma")) {
    pr <- prior_norm_from_range(prior_range[[param]], log)
    exp(rnorm(n, mean = pr$mean, sd = pr$sd))
  } else if (param %in% c("kappa2","kappa3")) {
    pr <- prior_norm_from_range(prior_range[[param]], qlogis)
    plogis(rnorm(n, mean = pr$mean, sd = pr$sd))
  } else {
    stop("No prior defined for param: ", param)
  }
}

# =============================
# Extract MLE + SE from grid fits (TMB)
# =============================
calib_list <- readRDS("R1_grid_calibrate.rds")
stopifnot(is.list(calib_list), length(calib_list) > 0)

coef_tbl <- imap_dfr(calib_list, function(fit, draw_id) {
  mp_tmb_coef(fit) %>%
    dplyr::filter(mat %in% pars_keep) %>%
    transmute(
      draw = draw_id,
      param = mat,
      estimate = as.numeric(estimate),
      se = as.numeric(std.error)
    )
}) %>%
  filter(is.finite(estimate), is.finite(se), se > 0) %>%
  mutate(param = factor(param, levels = pars_keep)) %>%
  mutate(
    conf.low  = estimate - z * se,
    conf.high = estimate + z * se,
    # enforce valid support
    conf.low = case_when(
      param %in% c("gamma_a", "gamma_i", "sigma") ~ pmax(conf.low, 1e-12),
      param %in% c("kappa2", "kappa3")            ~ pmax(conf.low, 0),
      TRUE                                        ~ conf.low
    ),
    conf.high = case_when(
      param %in% c("kappa2", "kappa3") ~ pmin(conf.high, 1),
      TRUE                             ~ conf.high
    )
  )

# =============================
# Build prior + posterior draws
# =============================
prior_draws <- tibble(
  param = rep(pars_keep, each = n_prior),
  value = unlist(lapply(pars_keep, r_prior_param, n = n_prior)),
  which = "Prior"
)

post_draws <- coef_tbl %>%
  group_by(param, draw) %>%
  summarise(
    value = list(rnorm(n_post_per_fit, mean = estimate[1], sd = se[1])),
    .groups = "drop"
  ) %>%
  unnest(value) %>%
  mutate(which = "Posterior") %>%
  mutate(
    value = if_else(param %in% c("gamma_a","gamma_i","sigma") & value <= 0, NA_real_, value)
  ) %>%
  filter(is.finite(value))

dens_df <- bind_rows(prior_draws, post_draws) %>%
  mutate(
    param = factor(param, levels = pars_keep),
    which = factor(which, levels = c("Prior","Posterior"))
  )

# =============================
# Prior vs posterior marginal densities
# =============================
p_density <- ggplot(dens_df, aes(x = value, colour = which, fill = which)) +
  geom_density(linewidth = 0.9, alpha = 0.18, adjust = 1.1, na.rm = TRUE) +
  facet_wrap(
    ~param, scales = "free", ncol = 5,
    labeller = as_labeller(strip_titles)
  ) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  labs(
    title = "Prior vs posterior marginal distributions",
    x = "Parameter value",
    y = "Density",
    colour = NULL,
    fill = NULL
  ) +
  theme_clean(base_size = 16) +
  theme(
    plot.title        = element_text(size = 30, hjust = 0.5, face = "plain"),
    strip.text        = element_text(size = 30, face = "plain"),
    axis.text.x       = element_text(size = 30),
    axis.text.y       = element_text(size = 30),
    axis.title.x      = element_text(size = 30),
    axis.title.y      = element_text(size = 30),
    legend.position   = "top",
    legend.direction  = "horizontal",
    legend.box        = "horizontal",
    legend.text       = element_text(size = 30),
    legend.background = element_blank(),
    panel.spacing     = unit(1.2, "cm"),
    panel.grid.major.y = element_line(linewidth = 0.25),
    plot.background   = element_blank()
  )

png("../figures/R1_prior_vs_posterior_params.png",
    width = 9000, height = 2800, res = 300, bg = "white", type = "cairo")
print(p_density)
dev.off()

# =============================
# Posterior summaries (MLE ± 1.96 SE), with unicode y-axis
# =============================
posterior_plt <- ggplot(coef_tbl, aes(x = estimate, y = param)) +
  geom_vline(xintercept = 0, linetype = 3, linewidth = 0.6) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.25, linewidth = 0.7) +
  geom_point(size = 2.8) +
  scale_y_discrete(labels = lab_unicode) +
  labs(
    title = "Posterior summaries of fitted parameters",
    x = "Estimate (95% credible interval)",
    y = NULL
  ) +
  theme_clean()+
  theme(
    axis.text.x       = element_text(size = 18, angle = 0, hjust = 0.5),
    axis.text.y       = element_text(size = 18),
    axis.title.x      = element_text(size = 18),
    axis.title.y      = element_text(size = 18 ),
    plot.title        = element_text(size = 20, hjust = 0.5, face = "plain"),
    plot.background   = element_blank()
  )
  
png("../figures/R1_marginal_plot.png", width = 3500, height = 1500, res = 300, bg = "white", type = "cairo")
print(posterior_plt)
dev.off()

# =============================
# Summary table (prior check + posterior pooled quantiles + coef summaries)
# =============================
prior_check <- prior_draws %>%
  mutate(param = factor(param, levels = pars_keep)) %>%
  group_by(param) %>%
  summarise(
    prior_q025 = quantile(value, 0.025, na.rm = TRUE),
    prior_q50  = quantile(value, 0.50,  na.rm = TRUE),
    prior_q975 = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

post_check <- post_draws %>%
  mutate(param = factor(param, levels = pars_keep)) %>%
  group_by(param) %>%
  summarise(
    post_q025 = quantile(value, 0.025, na.rm = TRUE),
    post_q50  = quantile(value, 0.50,  na.rm = TRUE),
    post_q975 = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

coef_summary <- coef_tbl %>%
  group_by(param) %>%
  summarise(
    mean_est = mean(estimate),
    sd_est   = sd(estimate),
    mean_se  = mean(se),
    .groups = "drop"
  )

summary_tbl <- prior_check %>%
  left_join(post_check, by = "param") %>%
  left_join(coef_summary, by = "param") %>%
  arrange(param)

print(summary_tbl)

# dir.create("../output", showWarnings = FALSE, recursive = TRUE)
# write.csv(summary_tbl, "../output/R1_prior_posterior_summary.csv", row.names = FALSE)

