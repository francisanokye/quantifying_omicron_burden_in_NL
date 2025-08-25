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

# set simulation period
start_date <- as.Date("2022-01-01") - offset0 #as.Date("2021-12-15") - offset0
last_date <- "2022-05-22"

# read inputs
calibrator <- rdsRead("calibrate.rds")
seroprevdata <- rdsRead("seroprevdata.rds")
time_steps <- max(seroprevdata$time)
upper_plot_time <- 300

# fill missing dates
seroprevdata <- seroprevdata %>%
  complete(date = seq.Date(from = as.Date("2022-01-01"), to = max(date), by = "1 day")) %>%
  select(date, value) %>%
  filter(date >= as.Date("2022-01-01") & date <= as.Date("2022-05-22"))

# get trajectory for seroprevalence
sims <- calibrator %>%
  mp_trajectory_sd(conf.int = TRUE, back_transform = TRUE) %>%
  filter(time >= offset0, matrix == c("serop")) %>%
  mutate(date = seq.Date(from = as.Date("2022-01-01"), by = "1 day", length.out = n())) %>%
  filter(date >= as.Date("2022-01-01") & date <= as.Date("2022-05-22"))

# define als phase shading
als_shading <- tibble(
  xmin = as.Date(c("2022-01-01", "2022-01-08", "2022-02-07", "2022-03-14")),
  xmax = as.Date(c("2022-01-08", "2022-02-07", "2022-03-14", "2022-05-22")),
  phase = c("ALS-3", "ALS-4", "ALS-3", "No-ALS"),
  fill_lab = c("ALS-3", "ALS-4", "ALS-3", "No-ALS")
)

als_data <- tibble(
  date = as.Date(c("2021-01-01", "2022-01-08", "2022-02-07", "2022-03-14")),
  phase = c( "ALS-3", "ALS-4", "ALS-3", "No-ALS")
)

# define fill colors
fill_colors <- c(
  "95% CI" = "red",
  "ALS-2" = adjustcolor("#66D1B5", alpha.f = 0.4),
  "ALS-3" = adjustcolor("#87CEFA", alpha.f = 0.6),
  "ALS-4" = adjustcolor("#FFD580", alpha.f = 0.4),
  "No-ALS" = adjustcolor("#D3D3D3", alpha.f = 0.6)
)

# generate plot
model_fit <- ggplot() +
  geom_rect(
    data = als_shading,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill_lab),
    inherit.aes = FALSE,
    alpha = 0.3,
    show.legend = FALSE
  ) +
  geom_point(
    data = seroprevdata,
    aes(x = date, y = value*100, color = "Data"),
    size = 3,
    show.legend = TRUE
  ) +
  geom_ribbon(
    data = sims,
    aes(x = date, ymin = conf.low*100, ymax = conf.high*100),
    fill = "red",
    alpha = 0.3,
    show.legend = FALSE
  ) +
  geom_line(
    data = sims,
    aes(x = date, y = value*100, color = "Model"),
    linewidth = 1,
    show.legend = TRUE
  ) +
  geom_vline(
    data = als_data,
    aes(xintercept = date, linetype = phase),
    color = "gold4",
    linewidth = 0.8,
    show.legend = FALSE
  ) +
  facet_wrap(
    ~matrix,
    scales = "free_y",
    ncol = 1,
    labeller = labeller(matrix = c(serop = "Mechanistic compartmental model fit to CITF seroprevalence data for NL"))
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Data" = "black", "Model" = "red"),
    labels = c("Data", "Model")
  ) +
  scale_fill_manual(
    name = NULL,
    values = fill_colors,
    guide = "none"
  ) +
  scale_linetype_manual(
    name = "ALS Phases",
    values = c("ALS-3" = "dashed", "ALS-4" = "dashed", "Mod-ALS-3" = "dashed", "No-ALS" = "solid"),
    guide = "none"
  ) +
  labs(
    x = "Date (Dec 15, 2021 - May 26, 2022)",
    y = "Cumulative % infected with SARS-CoV-2"
  ) +
  scale_x_date(
    expand = c(0, 0),
    date_breaks = "2 week",
    date_labels = "%b %d"
  ) +
  theme_clean() +
  theme(
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.85),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    strip.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.background = element_rect(color = NA),
    legend.margin = margin(0, 0, 0, 0),
    plot.background = element_blank(),
    legend.position = c(0.2, 0.8)
  ) +
  guides(fill = "none", linetype = "none")

print(model_fit)

#png("../figures/Figure_2.png", width = 5000, height = 2500, res = 300, bg = "white", type = "cairo")
#model_fit
#dev.off()
