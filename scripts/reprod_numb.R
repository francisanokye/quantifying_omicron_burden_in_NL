# ==== Load Libraries ====
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(tidyverse)
library(macpan2)
library(ggthemes)
library(shellpipes)

# ==== Initialize ====
rpcall("beta_plot.Rout beta_plot.R calibrate_inc.rds params.rda")
loadEnvironments()
set.seed(2025)

start_date <- as.Date("2021-12-15") - offset0
last_date  <- "2022-05-22"
calibrator <- rdsRead("calibrate.rds")

# ==== Extract Fitted Beta(t) ====
fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE) %>%
  mutate(date = as.Date(start_date) + time - 1) %>%
  filter(matrix == "beta_thing", date >= as.Date("2022-01-01"), date <= as.Date("2022-05-22")) %>%
  select(date, beta_thing = value)

# ==== Label ALS Periods ====
fitted_data <- fitted_data %>%
  mutate(
    alert_level = case_when(
      date < as.Date("2022-01-04") ~ "ALS-3\nK-12 Closed",
      date < as.Date("2022-01-25") ~ "ALS-4\nK-12 Closed",
      date < as.Date("2022-02-08") ~ "ALS-4\nK-12 Open",
      date < as.Date("2022-03-15") ~ "ALS-3\nK-12 Open",
      TRUE ~ "No-ALS\nK-12 Open"
    ),
    combined_alert = if_else(
      date <= as.Date("2022-01-25"), "K-12 Closed", "K-12 Open"
    )
  )

# ==== Compute Phase Summary Statistics ====
beta_summary <- fitted_data %>%
  group_by(alert_level) %>%
  summarise(mean_value = mean(beta_thing), sd_value = sd(beta_thing), .groups = "drop") %>%
  mutate(type = "Specific")

combined_summary <- fitted_data %>%
  group_by(combined_alert) %>%
  summarise(mean_value = mean(beta_thing), sd_value = sd(beta_thing), .groups = "drop") %>%
  rename(alert_level = combined_alert) %>%
  mutate(type = "Combined")

beta_summary_all <- bind_rows(beta_summary, combined_summary)

# ==== Constants for R0 ====
kappa1 <- 1; kappa2 <- 0.91; kappa3 <- 0.3
gamma_i <- 1/7; gamma_a <- 1/10; mu <- 0.324; zeta <- 0.75
p1 <- 0.15; p2 <- 0.85; p3 <- 0
bracket_term <- (mu/gamma_i) + ((1 - mu)*zeta/gamma_a)
susceptibility <- p1*kappa1 + p2*kappa2 + p3*kappa3
mult_const <- bracket_term * susceptibility

# ==== Calculate R0 and CI ====
beta_summary_all <- beta_summary_all %>%
  mutate(
    R0_mean = mean_value * mult_const,
    R0_sd   = sd_value * mult_const,
    k12_group = if_else(alert_level %in% c("K-12 Closed", "K-12 Open"), "Overall", "Specific"),
    k12_status = if_else(str_detect(alert_level, "Closed"), "K-12 Closed", "K-12 Open")
  )

# ==== Reorder for Plotting ====
spec_data <- beta_summary_all %>%
  filter(k12_group == "Specific") %>%
  arrange(k12_status, desc(R0_mean)) %>%
  mutate(chrono = factor(alert_level, levels = c(
    "ALS-3\nK-12 Closed", "ALS-4\nK-12 Closed", "ALS-4\nK-12 Open",
    "ALS-3\nK-12 Open", "No-ALS\nK-12 Open"
  )))

# ==== horizontal Group Mean Stats ====
summary_stats <- spec_data %>%
  group_by(k12_status) %>%
  summarise(
    mean = mean(R0_mean),
    se = sd(R0_mean)/sqrt(n()),
    lower = mean - 1.96 * se,
    upper = mean + 1.96 * se,
    color = case_when(k12_status == "K-12 Closed" ~ "red", TRUE ~ "blue"),
    .groups = "drop"
  )

# ==== background CI Bands ====
group_bands <- spec_data %>%
  mutate(index = as.numeric(chrono)) %>%
  group_by(k12_status) %>%
  summarise(
    xmin = min(index) - 0.5,
    xmax = max(index) + 0.5,
    .groups = "drop"
  ) %>%
  left_join(summary_stats, by = "k12_status")

# ==== annotation Labels ====
mean_labels <- summary_stats %>%
  group_by(k12_status) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(mean)) %>%
  mutate(
    label = sprintf("Mean (%s): %.2f [%.2f–%.2f]", k12_status, mean, lower, upper),
    x = 0.8,                       # Left edge
    y = 2.5 - 0.07 * row_number(),  # Stacked close together
    hjust = 0,
    vjust = 1
  )

# ==== generate Plot ====
gg <- ggplot(spec_data, aes(x = chrono, y = R0_mean)) +
  geom_rect(
    data = group_bands,
    aes(xmin = xmin, xmax = xmax, ymin = lower, ymax = upper, fill = color),
    inherit.aes = FALSE, alpha = 0.06
  ) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = R0_mean - 1.96*R0_sd, ymax = R0_mean + 1.96*R0_sd),
                width = 0.2, linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", R0_mean), y = R0_mean + 0.11),
            size = 8, hjust = 1.2) +
  geom_segment(
    data = summary_stats,
    aes(x = 0.5, xend = length(levels(spec_data$chrono)) + 0.5,
        y = mean, yend = mean, color = color),
    inherit.aes = FALSE, linewidth = 1.2
  ) +
  geom_text(
    data = mean_labels,
    aes(x = x, y = y, label = label, color = color, hjust = hjust, vjust = vjust),
    inherit.aes = FALSE,
    size = 6
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  labs(
    title = "Estimated Basic Reproduction Number by Phase (K–12 Closed vs Open)",
    y = expression(R[0]*"(t)"), x = NULL
  ) +
  theme_clean(base_size = 16) +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 22, face = "plain", hjust = 0.5),
    legend.position = "none",
    plot.background = element_blank()
  )

print(gg)
