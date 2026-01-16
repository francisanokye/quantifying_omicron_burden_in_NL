
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggthemes)
  library(macpan2)
  library(shellpipes)
  library(grid)
  library(patchwork)
  library(scales)
})

# ==== Initialize ====
rpcall("beta_plot.Rout beta_plot.R calibrate_inc.rds params.rda")
options(macpan2_log_dir = ".")
loadEnvironments()
set.seed(2025)

anchor_start <- as.Date("2021-12-15")
anchor_end   <- as.Date("2022-05-22")
calibrator   <- rdsRead("calibrate.rds")

# ==== Extract beta(t) over study window (FIXED date mapping) ====
fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE) %>%
  mutate(date = anchor_start + (time - offset0)) %>%   # <- FIX: remove -1 shift
  filter(matrix == "beta_thing",
         date >= anchor_start,
         date <= anchor_end) %>%
  transmute(date, beta_thing = as.numeric(value))

# ==== Map dates to ALS phases ====
fitted_data <- fitted_data %>%
  mutate(
    alert_level = case_when(
      date >= as.Date("2021-12-15") & date <= as.Date("2021-12-31") ~ "Early",
      date >= as.Date("2022-01-01") & date <= as.Date("2022-01-03") ~ "ALS-3",
      date >= as.Date("2022-01-04") & date <= as.Date("2022-02-07") ~ "ALS-4",
      date >= as.Date("2022-02-08") & date <= as.Date("2022-03-14") ~ "ALS-3",
      date >= as.Date("2022-03-15") & date <= as.Date("2022-05-22") ~ "No-ALS",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(alert_level))

# ==== Summaries per phase (mean/sd + phase length in days) ====
beta_summary <- fitted_data %>%
  group_by(alert_level) %>%
  summarise(
    phase_start = min(date),
    phase_end   = max(date),
    n_days      = as.integer(phase_end - phase_start) + 1L,
    mean_value  = mean(beta_thing, na.rm = TRUE),
    sd_value    = sd(beta_thing,   na.rm = TRUE),
    .groups     = "drop"
  )

# ==== Constants for Rc(t) scaling ====
kappa1 <- 1; kappa2 <- 0.91; kappa3 <- 0.3
gamma_i <- 1/7; gamma_a <- 1/10; mu <- 0.678; zeta <- 0.75
p1 <- 0.15; p2 <- 0.85; p3 <- 0

bracket_term   <- mu/gamma_i + (1 - mu)*zeta/gamma_a
susceptibility <- p1*kappa1 + p2*kappa2 + p3*kappa3
mult_const     <- bracket_term * susceptibility

# Rc(t) by alert level + 95% intervals + ordering
r0_data <- beta_summary %>%
  mutate(
    Rc_mean = mean_value * mult_const,
    Rc_sd   = sd_value   * mult_const,
    ymin    = Rc_mean - 1.96 * Rc_sd,
    ymax    = Rc_mean + 1.96 * Rc_sd,
    chrono  = factor(alert_level, levels = c("Early", "ALS-4", "ALS-3", "No-ALS"))
  )

# variable cap width based on # days
r0_data <- r0_data %>%
  mutate(
    x = as.numeric(chrono),
    cap_w = scales::rescale(n_days, to = c(0.15, 0.95))
  )

# Labels
phase_labels <- r0_data %>%
  mutate(label = sprintf("%.2f", Rc_mean))

# Colors
als_colors <- c(
  "Early"  = "grey40",
  "ALS-2"  = "#66D1B5",
  "ALS-3"  = "#87CEFA",
  "ALS-4"  = "#FFD580",
  "No-ALS" = "pink"
)

print(r0_data)

# Main plot
gg_main <- ggplot(r0_data, aes(x = x, y = Rc_mean, color = chrono)) +
  geom_point(size = 7) +
  geom_segment(aes(xend = x, y = ymin, yend = ymax), linewidth = 1.5) +
  geom_segment(aes(x = x - cap_w/2, xend = x + cap_w/2, y = ymin, yend = ymin), linewidth = 1.5) +
  geom_segment(aes(x = x - cap_w/2, xend = x + cap_w/2, y = ymax, yend = ymax), linewidth = 1.5) +
  geom_text(
    data = phase_labels,
    aes(x = x, y = Rc_mean, label = label),
    inherit.aes = FALSE,
    size = 10,
    color = "black",
    hjust = 1.25, vjust = 0.50
  ) +
  scale_color_manual(values = als_colors, guide = "none") +
  scale_x_continuous(
    breaks = sort(unique(r0_data$x)),
    labels = levels(r0_data$chrono)
  ) +
  labs(
    title = "Estimated control reproduction number for alert levels",
    y = expression(R[c]*"(t)"),
    x = NULL
  ) +
  theme_clean() +
  theme(
    axis.text.x  = element_text(size = 30),
    axis.text.y  = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    plot.background = element_blank(),
    plot.title   = element_text(size = 32, hjust = 0.5, face = "plain")
  ) +
  labs(tag = "B") +
  theme(
    plot.tag = element_text(size = 35, face = "bold", colour = "black"),
    plot.tag.position = c(0.01, 0.98)
  )

# Arrow strip
lvl     <- levels(r0_data$chrono)
x_start <- if ("ALS-4" %in% lvl) match("ALS-4", lvl) else min(which(lvl != "Early"))
x_end   <- length(lvl)

gg_arrow <- ggplot() +
  annotate(
    "segment",
    x = x_start, xend = x_end, y = 0.5, yend = 0.5,
    arrow = arrow(length = unit(0.25, "inches"), ends = "last", type = "closed"),
    linewidth = 1.5, color = "black"
  ) +
  annotate(
    "text",
    x = (x_start + x_end) / 2, y = -0.25,
    label = "decreasing strictness",
    size = 14, color = "black"
  ) +
  xlim(1, x_end) +
  ylim(-1, 1) +
  theme_void()

# Stack and save
final_plot <- gg_main / gg_arrow + plot_layout(heights = c(12, 2))

png("../figures/R1_als_R0.png", width = 5000, height = 2000, res = 300, bg = "white", type = "cairo")
print(final_plot)
dev.off()
