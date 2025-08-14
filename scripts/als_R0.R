# ==== Load Libraries ====
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(macpan2)
library(shellpipes)
library(grid)   # for unit()

# ==== initialize ====
rpcall("beta_plot.Rout beta_plot.R calibrate_inc.rds params.rda")
loadEnvironments()
set.seed(2025)

start_date <- as.Date("2021-12-15") - offset0
calibrator <- rdsRead("calibrate.rds")

# ==== extract Beta(t) ====
fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE) %>%
  mutate(date = as.Date(start_date) + time - 1) %>%
  filter(matrix == "beta_thing",
         date >= as.Date("2021-12-25"),
         date <= as.Date("2022-05-22")) %>%
  select(date, beta_thing = value)

# ==== define ALS Phases ====
fitted_data <- fitted_data %>%
  mutate(
    alert_level = case_when(
      date >= as.Date("2021-12-25") & date <= as.Date("2022-01-03") ~ "ALS-3",
      date >= as.Date("2022-01-04") & date <= as.Date("2022-02-07") ~ "ALS-4",
      date >= as.Date("2022-02-08") & date <= as.Date("2022-03-14") ~ "ALS-3",
      date >= as.Date("2022-03-15") & date <= as.Date("2022-05-22") ~ "No-ALS",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(alert_level))

# ==== summary Statistics ====
beta_summary <- fitted_data %>%
  group_by(alert_level) %>%
  summarise(
    mean_value = mean(beta_thing),
    sd_value   = sd(beta_thing),
    .groups    = "drop"
  )

# ==== Constants for R₀ ====
kappa1 <- 1; kappa2 <- 0.91; kappa3 <- 0.3
gamma_i <- 1/7; gamma_a <- 1/10; mu <- 0.324; zeta <- 0.75
p1 <- 0.15; p2 <- 0.85; p3 <- 0
bracket_term   <- mu / gamma_i + (1 - mu) * zeta / gamma_a
susceptibility <- p1 * kappa1 + p2 * kappa2 + p3 * kappa3
mult_const     <- bracket_term * susceptibility

# ==== compute R₀(t) by phase ====
r0_data <- beta_summary %>%
  mutate(
    R0_mean = mean_value * mult_const,
    R0_sd   = sd_value   * mult_const
  )

# ==== order for display: decreasing strictness (ALS-4 -> ALS-3 -> No-ALS) ====
strictness_levels <- c("ALS-4", "ALS-3", "No-ALS")
r0_data <- r0_data %>%
  mutate(chrono = factor(alert_level, levels = strictness_levels))

# ==== color map (by phase) ====
als_colors <- c(
  "ALS-4"  = "#FFD580",  # light orange
  "ALS-3"  = "#87CEFA",  # light blue
  "No-ALS" = "#D3D3D3"   # light gray
)

# ==== Y placement for arrow/label (outside plotting region) ====
y_min_ci <- min(r0_data$R0_mean - 1.96 * r0_data$R0_sd, na.rm = TRUE)
y_max_ci <- max(r0_data$R0_mean + 1.96 * r0_data$R0_sd, na.rm = TRUE)
y_span   <- y_max_ci - y_min_ci
y_arrow  <- y_min_ci - 0.08 * y_span
y_label  <- y_min_ci - 0.14 * y_span

# ==== plot ====
gg <- ggplot(r0_data, aes(x = chrono, y = R0_mean, color = chrono)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = R0_mean - 1.96 * R0_sd,
                    ymax = R0_mean + 1.96 * R0_sd),
                width = 0.2, linewidth = 0.9,alpha = 1.0) +
  geom_text(aes(label = sprintf("%.2f", R0_mean),
                y = R0_mean + 0.06 * y_span),
            size = 7, color = "black", hjust = 1.3, vjust = 2.8) +
  scale_color_manual(values = als_colors, guide = "none") +
  labs(
    title = "Estimated Basic Reproduction Number by ALS Phase",
    y = expression(R[0]*"(t)"), x = NULL
  ) +
  theme_clean(base_size = 16) +
  theme(
    axis.text       = element_text(size = 20),
    axis.title.y    = element_text(size = 20),
    plot.title      = element_text(size = 22, face = "plain", hjust = 0.5),
    plot.background = element_blank(),
    plot.margin     = margin(t = 10, r = 10, b = 40, l = 10)  # space for arrow/label
  ) +
  coord_cartesian(clip = "off") +
  annotate(
    "segment",
    x = 1, xend = length(strictness_levels),
    y = y_arrow, yend = y_arrow,
    arrow = arrow(length = unit(0.25, "inches"), ends = "last", type = "closed"),
    linewidth = 0.8, color = "maroon"
  ) +
  annotate(
    "text",
    x = mean(c(1, length(strictness_levels))),
    y = y_label,
    label = "decreasing strictness",
    size = 8
  )

png("../figures/als_R0.png", width = 5000, height = 2500, res = 300, bg = "white", type = "cairo")
print(gg)
dev.off()
