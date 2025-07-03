

library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(zoo)
library(ggthemes)
library(cowplot)
library(patchwork)
library(fuzzyjoin)
library(shellpipes)
library(tidyverse)
library(macpan2)

set.seed(2025)
options(macpan2_log_dir = ".")
loadEnvironments()

# load macpan2 outputs
calibrator <- rdsRead("calibrate.rds")

# load seroprevalence data
seroprevdata <- rdsRead("fitsero.rds") |>
  mutate(matrix = as.character(matrix)) |>
  mutate(value = ifelse(is.na(value) & time == 0, 700, value)) |>
  group_by(matrix) |>
  mutate(date = seq(as.Date("2022-05-22"), by = "-1 day", length.out = n()) |> rev()) |>
  ungroup()

# load simulation results
sims <- calibrator |>
  mp_trajectory_sd(conf.int = TRUE) |>
  select(-any_of(c("row", "col"))) |>
  group_by(matrix) |>
  mutate(matrix = as.character(matrix)) |>
  mutate(date = seq(as.Date("2022-05-22"), by = "-1 day", length.out = n()) |> rev()) |>
  ungroup()

# extract only beta
sims <- sims |>
  filter(matrix == "beta_thing") |>
  filter(date >= as.Date("2021-12-15") & date <= as.Date("2022-05-22"))

# ------------------------------------------------------------
# Prepare for Plotting
# ------------------------------------------------------------

# ALS shading regions
als_shading <- tibble(
  xmin = as.Date(c("2021-12-15", "2021-12-24", "2022-01-08", "2022-02-07", "2022-03-14")),
  xmax = as.Date(c("2021-12-24", "2022-01-08", "2022-02-07", "2022-03-14", "2022-05-22")),
  phase = c("ALS-2", "ALS-3", "ALS-4", "Mod-ALS-3", "No-ALS"),
  fill_lab = c("ALS-2", "ALS-3", "ALS-4", "Mod-ALS-3", "No-ALS")
)

# vertical ALS phase lines
als_data <- tibble(
  date = as.Date(c("2021-12-15", "2021-12-24", "2022-01-08", "2022-02-07", "2022-03-14")),
  phase = c("ALS-2", "ALS-3", "ALS-4", "Mod-ALS-3", "No-ALS")
)

# ALS colors
fill_colors <- c(
  "95% CI" = "red",
  "ALS-2" = adjustcolor("#66D1B5", alpha.f = 0.4),
  "ALS-3" = adjustcolor("#87CEFA", alpha.f = 0.4),
  "ALS-4" = adjustcolor("#FFD580", alpha.f = 0.4),
  "Mod-ALS-3" = adjustcolor("#F7E2E2", alpha.f = 0.6),
  "No-ALS" = adjustcolor("#D3D3D3", alpha.f = 0.6)
)

# beta annotations for the plot
beta_annot <- tibble(
  x = as.Date(c("2021-12-18", "2022-01-02", "2022-02-02", "2022-03-11", "2022-05-01")),
  y = c(0.15, 0.15, 0.10, 0.10, 0.10),
  label = c("ALS-2", "ALS-3", "ALS-4", "Mod-ALS-3", "No-ALS"),
  matrix = "beta"
)

# bracket segments data
bracket_df <- tibble(
  xmin = as.Date(c("2021-12-20", "2022-01-25")),
  xmax = as.Date(c("2022-01-25", "2022-05-22")),
  sch_label = c("K-12 School\nClosed", "K-12 Schools Reopen")
)

# compute bracket midpoints for labels
bracket_df <- bracket_df |>
  mutate(x_label = as.Date((as.numeric(xmin) + as.numeric(xmax)) / 2, origin = "1970-01-01"))

# vertical dotted lines for K-12 start/end
k12_lines <- tibble(
  date = as.Date(c("2021-12-20", "2022-01-25"))
)

# ------------------------------------------------------------
# Plot 1: Main Model Fit Plot
# ------------------------------------------------------------

p1 <- ggplot() +
  geom_rect(
    data = als_shading,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill_lab),
    inherit.aes = FALSE,
    alpha = 0.3
  ) +
  geom_ribbon(
    data = sims,
    aes(x = date, ymin = conf.low, ymax = conf.high, fill = "95% CI"),
    alpha = 0.3
  ) +
  geom_line(
    data = sims,
    aes(x = date, y = value, color = "Transmission rate"),
    linewidth = 1.5
  ) +
  geom_text(
    data = filter(beta_annot, x < as.Date("2022-01-10")),
    aes(x = x, y = y, label = label),
    parse = TRUE,
    size = 5,
    angle = 90,
    hjust = 1,
    color = "black",
    face = "bold"
  ) +
  geom_text(
    data = filter(beta_annot, x >= as.Date("2022-01-10")),
    aes(x = x, y = y, label = label),
    parse = TRUE,
    size = 5,
    angle = 0,
    hjust = 1,
    color = "black",
    face = "bold"
  ) +
  geom_vline(
    data = als_data,
    aes(xintercept = date, linetype = phase),
    color = "gold4",
    linewidth = 0.8,
    show.legend = FALSE
  ) + 
  geom_vline(
    data = k12_lines,
    aes(xintercept = date),
    linetype = "dotted",
    color = "red",
    linewidth = 1
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Transmission rate" = "blue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = fill_colors
  ) +
  scale_linetype_manual(
    name = "ALS Phases",
    values = c("ALS-3" = "dashed", "ALS-4" = "dashed", "Mod-ALS-3" = "dashed", "No-ALS" = "solid")
  ) +
  scale_x_date(
  limits = c(as.Date("2021-12-15"), as.Date("2022-05-22")),
  date_breaks = "1 month",
  date_labels = "%b %d",
  expand = c(0, 0)
  ) +
  labs(
    y = "Transmission Rate",
    x = NULL,
    title = expression("Time-varying Transmission Rate ("*beta*")")
  ) +
  theme_clean() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12, color = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    plot.background = element_blank()
  )

# ------------------------------------------------------------
# plot 2: bracket timeline 
# ------------------------------------------------------------

p2 <- ggplot() +
  geom_segment(
    data = bracket_df,
    aes(x = xmin, xend = xmax, y = 1, yend = 1),
    colour = c("red","navy"),
    size = 1.2,
    arrow = arrow(angle = 90, ends = "both", length = unit(0.5, "cm"))
  ) +
  geom_text(
    data = bracket_df,
    aes(x = x_label, y = 0.0, label = sch_label),
    colour = c("red","navy"),
    size = 5
  ) +
  scale_x_date(
    limits = c(as.Date("2021-12-15"), as.Date("2022-05-22")),
    expand = c(0, 0),
    date_breaks = "2 week",
    date_labels = "%b %d"
  ) +
  ylim(-1, 1) +
  theme_void() +
  theme(
    axis.text.x = element_text(size = 12),
    plot.margin = margin(0, 0, 0, 0)
  )

# ------------------------------------------------------------
# combine p1 and p2 vertically
# ------------------------------------------------------------

p1_leg <- p1 +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  )

legend_grob <- cowplot::get_legend(p1_leg)

p1_noleg <- p1 + theme(legend.position = "none")

bettas <- p1_noleg / p2 / wrap_elements(legend_grob) +
  plot_layout(heights = c(3, 0.7, 0.25))

print(bettas)

#png("../figures/transmission_rate.png", width = 3000, height = 2000, res = 300, bg = "white", type = "cairo")
#bettas
#dev.off()
