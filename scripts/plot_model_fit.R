library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
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

start_date <- as.Date("2021-12-15") - offset0
last_date <-"2022-05-22"

calibrator <- rdsRead("calibrate.rds")
seroprevdata <- rdsRead("seroprevdata.rds")
time_steps = max(seroprevdata$time)
upper_plot_time = 300

seroprevdata <- seroprevdata %>%
  complete(date = seq.Date(
    from = as.Date("2021-12-15"),
    to   = max(date),
    by   = "1 day"
))

seroprevdata <- (seroprevdata
	|> select(c("date","value"))
	|> filter(date >= as.Date("2021-12-15") & date <= as.Date("2022-05-22"))
)

sims = (calibrator
  |> mp_trajectory_sd(conf.int = TRUE, back_transform = TRUE)
  |> dplyr::filter(time >= offset0)
  |> dplyr::filter(matrix == c("serop"))
  |> mutate(date = seq.Date(from = as.Date("2021-12-15"), by = "1 day", length.out = n())) 
  |> filter(date >= as.Date("2021-12-15") & date <= as.Date("2022-05-22"))
)

# ALS phase shading 
als_shading <- tibble::tibble(
  xmin = as.Date(c("2021-12-15", "2021-12-24", "2022-01-08", "2022-02-07", "2022-03-14")),
  xmax = as.Date(c("2021-12-24", "2022-01-08", "2022-02-07", "2022-03-14", "2022-05-22")),
  phase = c("ALS-2", "ALS-3", "ALS-4", "Mod-ALS-3","No-ALS"),
  fill_lab = c("ALS-2", "ALS-3", "ALS-4", "Mod-ALS-3","No-ALS")
)

als_data <- tibble::tibble(
  date = as.Date(c("2021-12-15","2021-12-24", "2022-01-08", "2022-02-07", "2022-03-14")),
  phase = c("ALS-2", "ALS-3", "ALS-4", "Mod-ALS-3","No-ALS")
)

fill_colors <- c(
  "95% CI" = "red",
  "ALS-2" = adjustcolor("#66D1B5", alpha.f = 0.4),
  "ALS-3" = adjustcolor("#87CEFA", alpha.f = 0.4),
  "ALS-4" = adjustcolor("#FFD580", alpha.f = 0.4),
  "Mod-ALS-3" = adjustcolor("#F7E2E2", alpha.f = 0.6),
  "No-ALS" = adjustcolor("#D3D3D3", alpha.f = 0.6))

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
    aes(x = date, y = value, color = "Data"),
    size = 3,
    show.legend = TRUE
  ) +
  geom_ribbon(
    data = sims,
    aes(x = date, ymin = conf.low, ymax = conf.high),
    fill = "red",
    alpha = 0.3,
    show.legend = FALSE
  ) +
  geom_line(
    data = sims,
    aes(
      x = date,
      y = value,
      color = ifelse(matrix == "beta", "Transmission Rate", "Model")
    ),
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
    labeller = labeller(matrix = c(
      serop = "Infection-induced Seroprevalence Fit"
    ))
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Data" = "black",
      "Model" = "red"
    ),
    labels = c(
      "Data",
      "Model"
    )
  ) +
  scale_fill_manual(
    name = NULL,
    values = fill_colors,
    guide = "none"
  ) +
  scale_linetype_manual(
    name = "ALS Phases",
    values = c(
      "ALS-3" = "dashed",
      "ALS-4" = "dashed",
      "Mod-ALS-3" = "dashed",
      "No-ALS" = "solid"
    ),
    guide = "none"
  ) +
  labs(
    x = "Date (Dec 15, 2021 - May 26, 2022)",
    y = "Seroprevalence Estimate (%)"
  ) +
  scale_x_date(
    expand = c(0, 0),
    date_breaks = "2 week",
    date_labels = "%b %d"
  ) +
  #scale_x_continuous(limits = c(offset0, upper_plot_time)) +
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
  guides(
    fill = "none",
    linetype = "none"
  )

png("../figures/model_fit.png", width = 2500, height = 1500, res = 300, bg = "white", type = "cairo")
model_fit
dev.off()





