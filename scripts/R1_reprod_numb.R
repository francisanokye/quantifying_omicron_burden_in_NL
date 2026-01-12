
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(ggthemes)
  library(macpan2)
  library(shellpipes)
  library(grid)   # unit()
})

# ==== Initialize ====
options(macpan2_log_dir = ".")
loadEnvironments()
set.seed(2025)

# ==== Inputs / window ====
anchor_start <- as.Date("2021-12-15")
anchor_end   <- as.Date("2022-05-22")
calibrator   <- rdsRead("calibrate.rds")

# ==== Fixed constants for Rc(t) scaling (use manuscript values) ====
kappa1 <- 1; kappa2 <- 0.91; kappa3 <- 0.3
gamma_i <- 1/7; gamma_a <- 1/10; mu <- 0.678; zeta <- 0.75
p1 <- 0.15; p2 <- 0.85; p3 <- 0

bracket_term   <- (mu/gamma_i) + ((1 - mu) * zeta / gamma_a)
susceptibility <- p1*kappa1 + p2*kappa2 + p3*kappa3
mult_const     <- bracket_term * susceptibility

# ==== Extract beta(t) mean + CI, convert to Rc(t) mean + CI (daily) ====
fitted_daily <- mp_trajectory_sd(calibrator, conf.int = TRUE) %>%
  dplyr::filter(matrix == "beta_thing") %>%
  dplyr::mutate(date = anchor_start + (time - offset0)) %>%
  dplyr::filter(date >= anchor_start, date <= anchor_end) %>%
  dplyr::transmute(
    date,
    beta_mean = as.numeric(value),
    beta_low  = as.numeric(conf.low),
    beta_high = as.numeric(conf.high),
    Rc_mean   = as.numeric(value)     * mult_const,
    Rc_low    = as.numeric(conf.low)  * mult_const,
    Rc_high   = as.numeric(conf.high) * mult_const
  )

# ==== Phase labels (same cutpoints as your figure logic) ====
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

# ==== Phase summaries: mean Rc + matching mean of fitted CI bounds ====
spec_data <- fitted_daily %>%
  group_by(chrono, alert_level, k12_status) %>%
  summarise(
    Rc_mean = mean(Rc_mean, na.rm = TRUE),
    Rc_low  = mean(Rc_low,  na.rm = TRUE),
    Rc_high = mean(Rc_high, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(k12_status, chrono)

# group mean lines + group CI bands 
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
  mutate(idx = as.numeric(chrono)) %>%
  group_by(k12_status) %>%
  summarise(xmin = min(idx) - 0.5, xmax = max(idx) + 0.5, .groups = "drop") %>%
  left_join(group_stats, by = "k12_status")

# colors 
col_map <- c(
  "Early"       = "grey40",
  "K-12 Closed" = "red",  # vermillion
  "K-12 Open"   = "blue"   # blue
)

# plot 
y_top <- max(spec_data$Rc_high, na.rm = TRUE)

mean_labels <- group_stats %>%
  mutate(label = sprintf("%s: %.2f [%.2f–%.2f]", k12_status, mean, lower, upper)) %>%
  left_join(group_bands %>% select(k12_status, xmin, xmax),by = "k12_status") %>%
  mutate(x = 1.5,y = y_top - 0.05 * y_top * (row_number() - 1),hjust = 0,vjust = 1)

gg <- ggplot(spec_data, aes(x = chrono, y = Rc_mean)) +
  # group CI bands 
  geom_rect(
    data = group_bands,
    aes(xmin = xmin, xmax = xmax, ymin = lower, ymax = upper, fill = k12_status),
    inherit.aes = FALSE,
    alpha = 0.06
  ) +
  # phase points
  geom_point(aes(color = k12_status), size = 5) +
  # phase error bars from fitted daily CI (aggregated within each phase)
  geom_errorbar(
    aes(ymin = Rc_low, ymax = Rc_high, color = k12_status),
    width = 0.65, linewidth = 1.2
  ) +
  # numeric labels (mean)
  geom_text(
    aes(label = sprintf("%.2f", Rc_mean), color = k12_status),
    y = spec_data$Rc_mean + 0.03 * y_top, hjust = 1.25, vjust = 1.5,
    size = 10, show.legend = FALSE
  ) +
  # group mean lines
  geom_segment(
    data = group_bands,
    aes(x = xmin, xend = xmax, y = mean, yend = mean, color = k12_status),
    inherit.aes = FALSE,
    linewidth = 1.2
  ) +
  # geom_text(
  #   data = mean_labels,
  #   aes(x = x, y = y, label = label, color = k12_status),
  #   inherit.aes = FALSE,
  #   size = 6.5,
  #   fontface = "plain",
  #   show.legend = FALSE
  # )+
  scale_color_manual(values = col_map, guide = "none") +
  scale_fill_manual(values = col_map, guide = "none") +
  labs(
    x = NULL,
    y = expression(R[c](t)),
    title = "Control reproduction number by alert level and school status"
  ) +
  theme_clean() +
  theme(
    plot.title   = element_text(size = 32, hjust = 0.5, face = "plain"),
    axis.text.x  = element_text(size = 30),
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

png("../figures/R1_reprod_numb.png", width = 5000, height = 2500, res = 300, bg = "white", type = "cairo")
print(gg)
dev.off()
