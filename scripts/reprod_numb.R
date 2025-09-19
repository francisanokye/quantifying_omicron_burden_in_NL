# ==== load libraries ====
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(macpan2)
library(ggthemes)
library(shellpipes)

# ==== initialize ====
options(macpan2_log_dir = ".")
loadEnvironments()
set.seed(2025)

anchor_start <- as.Date("2021-12-15")
anchor_end   <- as.Date("2022-05-22")
calibrator   <- rdsRead("calibrate.rds")

# ==== extract fitted beta(t) within window ====
fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE) %>%
  filter(matrix == "beta_thing") %>%
  mutate(date = anchor_start + (time - offset0)) %>%
  filter(date >= anchor_start, date <= anchor_end) %>%
  select(date, beta_thing = value)

# ==== label phases (artifacts inclusive to dec 31) ====
fitted_data <- fitted_data %>%
  mutate(
    alert_level = case_when(
      date <= as.Date("2021-12-31") ~ "Early",
      date <  as.Date("2022-01-04") ~ "ALS-3\nK-12 Closed",
      date <  as.Date("2022-01-25") ~ "ALS-4\nK-12 Closed",
      date <  as.Date("2022-02-08") ~ "ALS-4\nK-12 Open",
      date <  as.Date("2022-03-15") ~ "ALS-3\nK-12 Open",
      TRUE                          ~ "No-ALS\nK-12 Open"
    )
  )

# ==== beta summaries by phase ====
beta_summary <- fitted_data %>%
  group_by(alert_level) %>%
  summarise(
    mean_value = mean(beta_thing, na.rm = TRUE),
    sd_value   = sd(beta_thing,   na.rm = TRUE),
    .groups    = "drop"
  )

# ==== constants and r0 scaling (replace with params if desired) ====
kappa1 <- 1; kappa2 <- 0.91; kappa3 <- 0.3
gamma_i <- 1/7; gamma_a <- 1/10; mu <- 0.324; zeta <- 0.75
p1 <- 0.15; p2 <- 0.85; p3 <- 0
bracket_term  <- (mu/gamma_i) + ((1 - mu) * zeta / gamma_a)
susceptibility <- p1*kappa1 + p2*kappa2 + p3*kappa3
mult_const    <- bracket_term * susceptibility

# ==== r0 per phase + grouping ====
spec_data <- beta_summary %>%
  mutate(
    R0_mean = mean_value * mult_const,
    R0_sd   = sd_value   * mult_const,
    k12_status = case_when(
      alert_level == "Early"        ~ "Early",
      str_detect(alert_level, "Closed") ~ "K-12 Closed",
      TRUE                              ~ "K-12 Open"
    ),
    chrono = factor(alert_level, levels = c(
      "Early","ALS-3\nK-12 Closed","ALS-4\nK-12 Closed",
      "ALS-4\nK-12 Open","ALS-3\nK-12 Open","No-ALS\nK-12 Open"
    ))
  ) %>%
  arrange(k12_status, desc(R0_mean))

# ==== per-group mean and 95% ci (single row per group) ====
summary_stats <- spec_data %>%
  group_by(k12_status) %>%
  summarise(
    mean  = mean(R0_mean, na.rm = TRUE),
    se    = sd(R0_mean,   na.rm = TRUE) / sqrt(n()),
    lower = mean - 1.96 * se,
    upper = mean + 1.96 * se,
    line_col = case_when(
      k12_status == "K-12 Closed" ~ "red",
      k12_status == "K-12 Open"   ~ "blue",
      TRUE                        ~ "gray"   # artifacts
    ),
    band_col = case_when(
      k12_status == "K-12 Closed" ~ "red",
      k12_status == "K-12 Open"   ~ "blue",
      TRUE                        ~ "gray"   # artifacts
    ),
    .groups = "drop"
  ) %>%
  distinct(k12_status, .keep_all = TRUE)

# ==== x-span per group over its categories (deduped) ====
group_bands <- spec_data %>%
  mutate(index = as.numeric(chrono)) %>%
  group_by(k12_status) %>%
  summarise(xmin = min(index) - 0.5, xmax = max(index) + 0.5, .groups = "drop") %>%
  left_join(summary_stats, by = "k12_status") %>%
  distinct(k12_status, .keep_all = TRUE)

# ==== ci-aware helpers (drop ci where na/non-finite) ====
group_bands_ci <- group_bands %>%
  filter(is.finite(lower), is.finite(upper))

spec_data_ci <- spec_data %>%
  filter(is.finite(R0_sd))  # no error bars if sd is na

# ==== dynamic label positions (one label per group; omit ci text if na) ====
y_top <- max(spec_data$R0_mean, na.rm = TRUE) * 1.15
mean_labels <- summary_stats %>%
  arrange(desc(mean)) %>%
  mutate(
    ci_ok = is.finite(lower) & is.finite(upper),
    label = ifelse(ci_ok,
                   sprintf("Mean (%s): %.2f [%.2f–%.2f]", k12_status, mean, lower, upper),
                   sprintf("Mean (%s): %.2f", k12_status, mean)),
    x = 0.8,
    y = y_top - 0.05 * y_top * (row_number() - 1),
    hjust = 0, vjust = 1
  ) %>%
  distinct(k12_status, .keep_all = TRUE)

# ==== plot ====
gg <- ggplot(spec_data, aes(x = chrono, y = R0_mean)) +
  # background 95% ci band per group (draw only when ci is available)
  geom_rect(
    data = group_bands_ci,
    aes(xmin = xmin, xmax = xmax, ymin = lower, ymax = upper, fill = band_col),
    inherit.aes = FALSE, alpha = 0.08
  ) +
  # per-phase points and (ci) error bars where available
  geom_point(size = 5) +
  geom_errorbar(
    data = spec_data_ci,
    aes(ymin = R0_mean - 1.96*R0_sd, ymax = R0_mean + 1.96*R0_sd),
    width = 0.2, linewidth = 1
  ) +
  geom_text(aes(label = sprintf("%.2f", R0_mean), y = R0_mean + 0.03*y_top),
            size = 9, hjust = 1.1, vjust = 1.0) +
  # group mean lines (always drawn)
  geom_segment(
    data = group_bands,
    aes(x = xmin, xend = xmax, y = mean, yend = mean, color = line_col),
    inherit.aes = FALSE, linewidth = 1.2
  ) +
  # geom_text(
  #  data = mean_labels,
  #  aes(x = x, y = y, label = label, color = line_col, hjust = hjust, vjust = vjust),
  #  inherit.aes = FALSE, size = 7
  # ) +
  scale_color_identity() +
  scale_fill_identity() +
  labs(
    title = "Estimated basic reproduction number for alert level and school closure combinations",
    y = expression(R[0]*"(t)"), x = NULL
  ) +
  theme_clean(base_size = 16) +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title  = element_text(size = 25, hjust = 0.5, face = "plain"),
    legend.position = "none",
    plot.background = element_blank()
  )

# png("../figures/reprod_numb.png", width = 5000, height = 2500, res = 300, bg = "white", type = "cairo")
# print(gg)
# dev.off()
