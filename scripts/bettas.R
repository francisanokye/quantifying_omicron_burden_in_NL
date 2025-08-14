# ==== Load libraries ====
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

# ==== initialize ====
set.seed(2025)
options(macpan2_log_dir = ".")
loadEnvironments()

calibrator   <- rdsRead("calibrate.rds")
seroprevdata <- rdsRead("seroprevdata.rds")
time_steps   <- max(seroprevdata$time)
upper_plot_time <- 300

# ==== prepare seroprevalence data ====
seroprevdata <- seroprevdata %>%
  complete(date = seq.Date(from = as.Date("2021-12-15"), to = max(date), by = "1 day")) %>%
  select(date, value) %>%
  filter(date >= as.Date("2021-12-15") & date <= as.Date("2022-05-22"))

# ==== extract beta(t) with uncertainty ====
sims <- calibrator %>%
  mp_trajectory_sd(conf.int = TRUE, back_transform = TRUE) %>%
  filter(time >= offset0, matrix == "beta_thing") %>%
  mutate(date = seq.Date(from = as.Date("2021-12-15"), by = "1 day", length.out = n())) %>%
  filter(date >= as.Date("2022-01-01") & date <= as.Date("2022-05-22"))

# ==== define ALS shading periods ====
als_shading <- tibble(
  xmin     = as.Date(c("2022-01-01", "2022-01-04", "2022-02-07", "2022-03-14")),
  xmax     = as.Date(c("2022-01-04", "2022-02-07", "2022-03-14", "2022-05-22")),
  phase    = c("ALS-3", "ALS-4", "ALS-3", "No-ALS"),
  fill_lab = c("ALS-3", "ALS-4", "ALS-3", "No-ALS")
)

# ==== ALS phase lines ====
als_data <- tibble(
  date  = as.Date(c("2022-01-04", "2022-02-07", "2022-03-14")),
  phase = c("ALS-4", "ALS-3", "No-ALS")
)

# ==== fill color scale ====
fill_colors <- c(
  "95% CI" = "violet",
  "ALS-3"  = adjustcolor("#87CEFA", alpha.f = 0.6),
  "ALS-4"  = adjustcolor("#FFD580", alpha.f = 0.4),
  "No-ALS" = adjustcolor("#D3D3D3", alpha.f = 0.6)
)

# ==== annotations for beta(t) ====
beta_annot <- tibble(
  x      = as.Date(c("2022-01-02", "2022-02-02", "2022-03-07", "2022-04-28")),
  y      = c(0.15, 0.12, 0.12, 0.12),
  label  = c("ALS-3", "ALS-4", "ALS-3", "No-ALS"),
  matrix = "beta"
)

# ==== define K–12 bracket regions ====
bracket_df <- tibble(
  xmin       = as.Date(c("2022-01-01", "2022-01-25")),
  xmax       = as.Date(c("2022-01-25", "2022-05-22")),
  sch_label  = c("K-12 School\nClosed", "K-12 Schools Open")
) %>%
  mutate(x_label = as.Date((as.numeric(xmin) + as.numeric(xmax)) / 2, origin = "1970-01-01"))

# ==== K–12 transition lines ====
k12_lines <- tibble(date = as.Date(c("2022-01-01", "2022-01-25")))

# ==== plot 1: Beta(t) with CI and ALS overlays ====
p1 <- ggplot() +
  geom_rect(
    data = als_shading,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill_lab),
    inherit.aes = FALSE, alpha = 0.3
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
    aes(x = x, y = y + 0.15, label = label),
    parse = TRUE,
    size = 8,
    angle = 90,
    hjust = 1,
    color = "black",
    face = "bold"
  ) +
  geom_text(
    data = filter(beta_annot, x >= as.Date("2022-01-10")),
    aes(x = x, y = y + 0.05, label = label),
    parse = TRUE,
    size = 8,
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
    values = c("ALS-3" = "dashed", "ALS-4" = "dashed", "No-ALS" = "solid")
  ) +
  scale_x_date(
    limits = c(as.Date("2022-01-01"), as.Date("2022-05-22")),
    date_breaks = "1 month",
    date_labels = "%b %d"
  ) +
  labs(
    title = expression("Time-varying Transmission Rate ("*beta*")"),
    y = "Transmission Rate",
    x = NULL
  ) +
  theme_clean() +
  theme(
    axis.text.x    = element_blank(),
    axis.ticks.x   = element_blank(),
    axis.text.y    = element_text(size = 20),
    axis.title.y   = element_text(size = 20, color = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box      = "horizontal",
    legend.title    = element_text(size = 20),
    legend.text     = element_text(size = 20),
    plot.title      = element_text(size = 20, color = "black", hjust = 0.5),
    plot.background = element_blank()
  )

# ==== plot 2: K–12 bracket timeline ====
p2 <- ggplot() +
  geom_segment(
    data = bracket_df,
    aes(x = xmin, xend = xmax, y = 1, yend = 1),
    colour = c("red", "navy"),
    size = 1.5,
    arrow = arrow(angle = 90, ends = "both", length = unit(0.5, "cm"))
  ) +
  geom_text(
    data = bracket_df,
    aes(x = x_label, y = 0.0, label = sch_label),
    colour = c("red", "navy"),
    size = 10
  ) +
  scale_x_date(
    limits = c(as.Date("2022-01-01"), as.Date("2022-05-22")),
    date_breaks = "2 week",
    date_labels = "%b %d"
  ) +
  ylim(-1, 1) +
  theme_void() +
  theme(
    axis.text.x   = element_text(size = 20),
    plot.margin   = margin(0, 0, 0, 0)
  )

# ==== combine plots ====
p1_leg   <- p1 + theme(legend.position = "bottom")
legend_grob <- cowplot::get_legend(p1_leg)
p1_noleg   <- p1 + theme(legend.position = "none")

gg <- p1_noleg / p2 / wrap_elements(legend_grob) +
  plot_layout(heights = c(3, 0.7, 0.25))

print(gg)
