
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(ggthemes)
  library(macpan2)
  library(shellpipes)
  library(grid)   # unit()
  library(scales) # rescale()
})

# ==== Initialize ====
options(macpan2_log_dir = ".")
loadEnvironments()
set.seed(2025)

# ==== Inputs / window ====
anchor_start <- as.Date("2021-12-15")
anchor_end   <- as.Date("2022-05-22")
calibrator   <- rdsRead("calibrate.rds")

# fixed parameters for Rc(t) scaling (use manuscript values) 
kappa1 <- 1; kappa2 <- 0.91; kappa3 <- 0.3
gamma_i <- 1/7; gamma_a <- 1/10; mu <- 0.678; zeta <- 0.75
p1 <- 0.15; p2 <- 0.85; p3 <- 0

bracket_term   <- (mu/gamma_i) + ((1 - mu) * zeta / gamma_a)
susceptibility <- p1*kappa1 + p2*kappa2 + p3*kappa3
mult_const     <- bracket_term * susceptibility

# extract beta(t) mean + CI, convert to Rc(t) mean + CI (daily) 
# Keep beta_mean/beta_low/beta_high so we can aggregate
fitted_daily <- mp_trajectory_sd(calibrator, conf.int = TRUE) %>%
  dplyr::filter(matrix == "beta_thing") %>%
  dplyr::mutate(date = anchor_start + (time - offset0)) %>%
  dplyr::filter(date >= anchor_start, date <= anchor_end) %>%
  dplyr::transmute(
    date,
    beta_mean = as.numeric(value),
    beta_low  = as.numeric(conf.low),
    beta_high = as.numeric(conf.high)
  )

# ==== Phase labels ====
fitted_daily <- fitted_daily %>%
  mutate(
    alert_level = case_when(
      date <= as.Date("2021-12-31") ~ "Early",
      date <  as.Date("2022-01-04") ~ "ALS-3\nK-12 Closed",
      date <  as.Date("2022-01-25") ~ "ALS-4\nK-12 Closed",
      date <  as.Date("2022-02-08") ~ "ALS-4\nK-12 Open",
      date <  as.Date("2022-03-15") ~ "ALS-3\nK-12 Open",
      TRUE                          ~ "No-ALS\nK-12 Open"
    ),
    chrono = factor(alert_level, levels = c(
      "Early","ALS-3\nK-12 Closed","ALS-4\nK-12 Closed",
      "ALS-4\nK-12 Open","ALS-3\nK-12 Open","No-ALS\nK-12 Open"
    )),
    k12_status = case_when(
      alert_level == "Early"            ~ "Early",
      str_detect(alert_level, "Closed") ~ "K-12 Closed",
      TRUE                              ~ "K-12 Open"
    )
  )

# ==== Phase summaries  ====
# 1) average beta within phase
# 2) multiply by mult_const to get Rc
spec_data <- fitted_daily %>%
  group_by(chrono, alert_level, k12_status) %>%
  summarise(
    n_days   = dplyr::n(),
    beta_m   = mean(beta_mean, na.rm = TRUE),
    beta_lo  = mean(beta_low,  na.rm = TRUE),
    beta_hi  = mean(beta_high, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    Rc_mean = beta_m  * mult_const,
    Rc_low  = beta_lo * mult_const,
    Rc_high = beta_hi * mult_const
  ) %>%
  arrange(k12_status, chrono)

# ==== cap width depends on days ====
spec_data <- spec_data %>%
  mutate(
    x     = as.numeric(chrono),
    cap_w = scales::rescale(n_days, to = c(0.15, 0.95))
  )

# ==== Group mean lines + group CI bands ====
group_stats <- spec_data %>%
  group_by(k12_status) %>%
  summarise(
    mean  = mean(Rc_mean, na.rm = TRUE),
    se    = sd(Rc_mean, na.rm = TRUE) / sqrt(dplyr::n()),
    lower = mean - 1.96 * se,
    upper = mean + 1.96 * se,
    .groups = "drop"
  )

group_bands <- spec_data %>%
  group_by(k12_status) %>%
  summarise(xmin = min(x) - 0.5, xmax = max(x) + 0.5, .groups = "drop") %>%
  left_join(group_stats, by = "k12_status")

# ==== Colors ====
col_map <- c(
  "Early"       = "grey40",
  "K-12 Closed" = "red",
  "K-12 Open"   = "blue"
)

# ==== Plot ====
y_top <- max(spec_data$Rc_high, na.rm = TRUE)

# do NOT draw the mean line for Early
group_bands_no_early <- group_bands %>% filter(k12_status != "Early")

gg <- ggplot(spec_data, aes(x = x, y = Rc_mean)) +
  # group CI bands
  geom_rect(
    data = group_bands,
    aes(xmin = xmin, xmax = xmax, ymin = lower, ymax = upper, fill = k12_status),
    inherit.aes = FALSE,
    alpha = 0.06
  ) +
  geom_point(aes(color = k12_status), size = 5) +
  geom_segment(
    aes(xend = x, y = Rc_low, yend = Rc_high, color = k12_status),
    linewidth = 1.2
  ) +
  geom_segment(
    aes(x = x - cap_w/2, xend = x + cap_w/2, y = Rc_low,  yend = Rc_low,  color = k12_status),
    linewidth = 1.2
  ) +
  geom_segment(
    aes(x = x - cap_w/2, xend = x + cap_w/2, y = Rc_high, yend = Rc_high, color = k12_status),
    linewidth = 1.2
  ) +
  geom_text(
    aes(label = sprintf("%.2f", Rc_mean), color = k12_status),
    y = spec_data$Rc_mean + 0.03 * y_top,
    hjust = 1.25, vjust = 1.25,
    size = 10, show.legend = FALSE
  ) +
  geom_segment(
    data = group_bands_no_early,
    aes(x = xmin, xend = xmax, y = mean, yend = mean, color = k12_status),
    inherit.aes = FALSE,
    linewidth = 1.2
  ) +
  scale_color_manual(values = col_map, guide = "none") +
  scale_fill_manual(values = col_map, guide = "none") +
  scale_x_continuous(
    breaks = sort(unique(spec_data$x)),
    labels = levels(spec_data$chrono)
  ) +
  labs(
    x = NULL,
    y = expression(R[c](t)),
    title = "Control reproduction number by alert level and school status"
  ) +
  theme_clean() +
  theme(
    plot.title   = element_text(size = 32, hjust = 0.5, face = "plain"),
    axis.text.x  = element_text(size = 28),
    axis.text.y  = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    panel.grid.major.y = element_line(color = "grey85"),
    plot.background = element_blank()
  ) +
  labs(tag = "A") +
  theme(
    plot.tag = element_text(size = 35, face = "bold"),
    plot.tag.position = c(0.01, 0.98)
  )

png("../figures/R1_reprod_numb.png", width = 5000, height = 2000, res = 300, bg = "white", type = "cairo")
print(gg)
dev.off()

