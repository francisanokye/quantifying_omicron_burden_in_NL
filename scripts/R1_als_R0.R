
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggthemes)
  library(macpan2)
  library(shellpipes)
  library(grid)       # unit()
  library(patchwork)  # stacking
})

# ==== Initialize ====
rpcall("beta_plot.Rout beta_plot.R calibrate_inc.rds params.rda")
options(macpan2_log_dir = ".")
loadEnvironments()
set.seed(2025)

start_date <- as.Date("2021-12-15") - offset0
calibrator <- rdsRead("calibrate.rds")

# ==== Extract beta(t) over study window ====
fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE) %>%
  mutate(date = as.Date(start_date) + time - 1) %>%
  filter(matrix == "beta_thing",
         date >= as.Date("2021-12-15"),
         date <= as.Date("2022-05-22")) %>%
  transmute(date, beta_thing = value)

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

# ==== Summaries per phase ====
beta_summary <- fitted_data %>%
  group_by(alert_level) %>%
  summarise(
    mean_value = mean(beta_thing, na.rm = TRUE),
    sd_value   = sd(beta_thing,   na.rm = TRUE),
    .groups    = "drop"
  )

# ==== Constants for Rc(t) scaling ====
kappa1 <- 1; kappa2 <- 0.91; kappa3 <- 0.3
gamma_i <- 1/7; gamma_a <- 1/10; mu <- 0.678; zeta <- 0.75
p1 <- 0.15; p2 <- 0.85; p3 <- 0

bracket_term   <- mu/gamma_i + (1 - mu)*zeta/gamma_a
susceptibility <- p1*kappa1 + p2*kappa2 + p3*kappa3
mult_const     <- bracket_term * susceptibility

# ==== Rc(t) by phase + 95% intervals (as in your plot) ====
r0_data <- beta_summary %>%
  mutate(
    Rc_mean = mean_value * mult_const,
    Rc_sd   = sd_value   * mult_const
  ) %>%
  mutate(
    chrono = factor(alert_level, levels = c("Early", "ALS-4", "ALS-3", "No-ALS")),
    ymin = Rc_mean - 1.96 * Rc_sd,
    ymax = Rc_mean + 1.96 * Rc_sd
  )

# ==== Label positions that correspond to each phase interval ====
y_min_ci <- min(r0_data$ymin, na.rm = TRUE)
y_max_ci <- max(r0_data$ymax, na.rm = TRUE)
y_span   <- y_max_ci - y_min_ci

phase_labels <- r0_data %>%
  mutate(
    label = sprintf("%.2f", Rc_mean)
    # label = sprintf("%.2f [%.2f–%.2f]", Rc_mean, ymin, ymax),
  )

als_colors <- c(
  "Early"  = "grey40",
  "ALS-2"  = "#66D1B5",
  "ALS-3"  = "#87CEFA",
  "ALS-4"  = "#FFD580",
  "No-ALS" = "pink"
)

# ==== Main plot (points + matching CI labels) ====
gg_main <- ggplot(r0_data, aes(x = chrono, y = Rc_mean, color = chrono)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.70, linewidth = 1.5) +
  geom_text(
    data = phase_labels,
    aes(x = chrono, y = Rc_mean, label = label),
    inherit.aes = FALSE,
    size = 10,
    color = "black",
    hjust = 1.25, vjust = 0.50
  ) +
  scale_color_manual(values = als_colors, guide = "none") +
  labs(title = "Estimated control reproduction number for alert levels",
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

# ==== Arrow strip (decreasing strictness) ====
lvl     <- levels(r0_data$chrono)
x_start <- if ("ALS-4" %in% lvl) match("ALS-4", lvl) else min(which(lvl != "Early"))
x_end   <- length(lvl)

gg_arrow <- ggplot() + 
  annotate("segment",x = x_start, xend = x_end, y = 0.5, yend = 0.5,
    arrow = arrow(length = unit(0.25, "inches"), ends = "last", type = "closed"),linewidth = 1.5, color = "black"
    ) +
  annotate("text",x = (x_start + x_end) / 2, y = -0.25,label = "decreasing strictness", size = 14, color = "black") +
  xlim(1, x_end) +
  ylim(-1, 1) +
  theme_void()

# ==== Stack + save ====
final_plot <- gg_main / gg_arrow + plot_layout(heights = c(12, 2))

png("../figures/R1_als_R0.png", width = 5000, height = 2500, res = 300, bg = "white", type = "cairo")
print(final_plot)
dev.off()
