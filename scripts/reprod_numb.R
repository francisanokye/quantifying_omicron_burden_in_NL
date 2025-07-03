library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(zoo)
library(ggthemes)
library(ggtext)
library(cowplot)
library(patchwork)
library(fuzzyjoin)
library(shellpipes)
rpcall("beta_plot.Rout beta_plot.R calibrate_inc.rds params.rda")
library(tidyverse)
library(macpan2)
loadEnvironments()
set.seed(2025)

start_date <- as.Date("2021-12-15") - offset0
last_date <-"2022-05-22"

calibrator <- rdsRead("calibrate.rds")

# model simulation with calibrated parameters
fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE)

fitted_data <- (fitted_data
        |> mutate(date = as.Date(start_date) + as.numeric(time) -1 )
        |> dplyr::filter(between(date, as.Date(start_date), as.Date(last_date)))
        |> dplyr::filter(matrix %in% c("date","beta_thing"))
)

# convert matrix values into columns
beta_values <- fitted_data |>
  select(-any_of(c("row", "col"))) |>
  pivot_wider(names_from = matrix, values_from = value) |>
  group_by(date) |>
  mutate(across(everything(), ~ first(na.omit(.)), .names = "{.col}")) |>
  ungroup() |>
  distinct(date, .keep_all = TRUE) |>
  drop_na() |>
  select(c(date, beta_thing))

beta_values <- beta_values |>
        dplyr::filter(date >= as.Date("2021-12-15") & date <= as.Date("2022-05-22"))

beta_values <- beta_values %>%
  mutate(
    alert_level = case_when(
      date >= as.Date("2021-12-15") & date <= as.Date("2021-12-19") ~ "ALS-2\nK-12 Open",
      date >= as.Date("2021-12-20") & date <= as.Date("2021-12-24") ~ "ALS-2\nK-12 Closed",
      date >= as.Date("2021-12-25") & date <= as.Date("2022-01-03") ~ "ALS-3\nK-12 Closed",
      date >= as.Date("2022-01-04") & date <= as.Date("2022-01-25") ~ "ALS-4\nK-12 Closed",
      date >= as.Date("2022-01-26") & date <= as.Date("2022-02-07") ~ "ALS-4\nK-12 Open",
      date >= as.Date("2022-02-08") & date <= as.Date("2022-03-14") ~ "Mod-ALS-3\nK-12 Open",
      date >= as.Date("2022-03-15") & date <= as.Date("2022-05-22") ~ "No-ALS\nK-12 Open",
      TRUE ~ NA_character_
    ),
    combined_alert = case_when(
      date >= as.Date("2021-12-20") & date <= as.Date("2022-01-25") ~ "ALS-2-3-4\nK-12 Closed",
      date >= as.Date("2022-01-26") & date <= as.Date("2022-05-22") ~ "ALS-4-Mod3-None\nK-12 Open",
      TRUE ~ NA_character_
    )
  )
# ------------------------------------------------------------
# Summarize specific alert levels
# ------------------------------------------------------------

beta_summary <- beta_values %>%
  group_by(alert_level) %>%
  summarise(
    mean_value = mean(beta_thing, na.rm = TRUE),
    sd_value = sd(beta_thing, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(alert_level)) %>%
  mutate(type = "Specific")

# ------------------------------------------------------------
# Summarize combined alert levels
# ------------------------------------------------------------

beta_summary_combined <- beta_values %>%
  group_by(combined_alert) %>%
  summarise(
    mean_value = mean(beta_thing, na.rm = TRUE),
    sd_value = sd(beta_thing, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(combined_alert)) %>%
  rename(alert_level = combined_alert) %>%
  mutate(type = "Combined")

# ------------------------------------------------------------
# Merge both summaries
# ------------------------------------------------------------

beta_summary_all <- bind_rows(beta_summary, beta_summary_combined)

# Fixed parameter values
kappa1 <- 1
kappa2 <- 0.91
kappa3 <- 0.3
gamma_i <- 1/7
gamma_a <- 1/10
mu <- 0.324
zeta <- 0.75
p1 <- 0.15
p2 <- 0.85
p3 <- 0

# Pre-calculate the bracket term
bracket_term <- (mu / gamma_i) + ((1 - mu) * zeta / gamma_a)

# Susceptibility-weighted proportion
susceptibility_sum <- (p1 * kappa1) + (p2 * kappa2) + (p3 * kappa3)

# Multiplicative constant
constant_mult <- susceptibility_sum * bracket_term

# ------------------------------------------------------------
# Calculate R0 for mean and SD
# ------------------------------------------------------------

# Function to compute R0 from β
calc_R0 <- function(beta) {
  beta * constant_mult
}

calc_R0_sd <- function(beta, beta_sd) {
  # approximate SD via linear propagation
  constant_mult * beta_sd
}

# Compute R0 and propagate SD
beta_summary_all <- beta_summary_all %>%
  mutate(
    R0_mean = calc_R0(mean_value),
    R0_sd = calc_R0_sd(mean_value, sd_value)
  )

ordering <- beta_summary_all %>%
  arrange(desc(R0_mean)) %>%
  pull(alert_level)

beta_summary_all$alert_level <- factor(
  beta_summary_all$alert_level,
  levels = ordering
)

actual_levels <- levels(beta_summary_all$alert_level)
palette_size <- length(actual_levels)

if (palette_size > 0) {
  palette_colors <- RColorBrewer::brewer.pal(
    min(palette_size, 8),
    "Set2"
  )
  alert_colors <- setNames(
    palette_colors[seq_len(palette_size)],
    actual_levels
  )
} else {
  alert_colors <- NULL
}

# extract unique levels
# --------------------
actual_levels <- levels(beta_summary_all$alert_level)

alert_colors <- c(
  "ALS-2\nK-12 Open" = "#1b9e77",
  "ALS-2\nK-12 Closed" = "#d95f02",
  "ALS-3\nK-12 Closed" = "#7570b3",
  "ALS-4\nK-12 Closed" = "#e7298a",
  "ALS-4\nK-12 Open" = "#66a61e",
  "Mod-ALS-3\nK-12 Open" = "#e6ab02",
  "No-ALS\nK-12 Open" = "#a6761d",
  "ALS-2-3-4\nK-12 Closed" = "#666666",
  "ALS-4-Mod3-None\nK-12 Open" = "#1f78b4"
)

# color interventions based on the school closure

alert_k12 <- c("Mod-ALS-3\nK-12 Open" = "blue", 
	       "ALS-4-Mod3-None\nK-12 Open" = "blue",
	       "ALS-4\nK-12 Open"= "blue",
	       "No-ALS\nK-12 Open"= "blue",          
	       "ALS-4\nK-12 Closed" = "red",
	       "ALS-2-3-4\nK-12 Closed" = "red",
	       "ALS-3\nK-12 Closed" = "red",
	       "ALS-2\nK-12 Closed" = "red",
	       "ALS-2\nK-12 Open" = "blue"
)


# ------------------------------------------------------------
# Plot R0 instead of beta
# ------------------------------------------------------------

r0_errorplot_all <- ggplot(beta_summary_all,
                           aes(x = alert_level,
                               y = R0_mean,
                               fill = alert_level,
			       color = alert_level)) +
  geom_errorbar(aes(ymin = R0_mean - R0_sd,
                    ymax = R0_mean + R0_sd),
                width = 0.15,
                linewidth = 0.8) +
  geom_point(size = 3,
             position = position_dodge(width = 0.5),
             show.legend = FALSE) +
  geom_text(aes(label = round(R0_mean, 3)),
            vjust = 0.0,
            hjust = -0.38,
            size = 2.8,
            color = "black") +
  labs(title = expression("Estimated " * R[0] * " by Alert Levels and K-12 School Closures"),
       x = "Intervention",
       y = expression("Reproduction Number ("*R[0]*")")) +
  scale_fill_manual(
      values = alert_colors
    ) +
    scale_color_manual(
      values = alert_colors
    ) +
  theme_clean() +
  theme(
    axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5, color = alert_k12),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5),
    legend.position = "none",
    panel.border = element_blank(),
    plot.background = element_blank()
  )
  
# create colored labels
label_html <- purrr::map_chr(names(alert_k12), function(lvl) {
  col <- alert_k12[[lvl]]
  sprintf("<span style='color:%s;'>%s</span>", col, gsub("\\n","<br>",lvl))
})
names(label_html) <- names(alert_k12)

# add to plot
r0_errorplot_all <- r0_errorplot_all +
  scale_x_discrete(labels = label_html) +
  theme(axis.text.x = ggtext::element_markdown(size = 8, angle = 0, hjust = 0.5))

print(r0_errorplot_all)


#png("../figures/R0_errorplot.png", width = 2500, height = 1500, res = 300, bg = "white")
#r0_errorplot_all
#dev.off()








