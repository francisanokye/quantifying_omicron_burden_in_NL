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

pop = 510550

start_date <- as.Date("2021-12-15") - offset0
last_date <-"2022-05-26"

calibrator <- rdsRead("calibrate_inc.rds")

seroprevdata <- rdsRead("fitsero.rds") |>
  mutate(matrix = as.character(matrix)) |>
  mutate(value = ifelse(is.na(value) & time == 0, 700, value)) |>
  group_by(matrix) |>
  mutate(date = seq(as.Date("2022-05-26"), by = "-1 day", length.out = n()) |> rev()) |>
  ungroup()

sims <- calibrator |>
  mp_trajectory_sd(conf.int = TRUE) |>
  select(-any_of(c("row", "col"))) |>
  group_by(matrix) |>
  mutate(matrix = as.character(matrix)) |>
  mutate(date = seq(as.Date("2022-05-26"), by = "-1 day", length.out = n()) |> rev()) |>
  ungroup()

# filter date range
seroprevdata <- seroprevdata |> filter(date >= as.Date("2021-12-15") & date <= as.Date("2022-05-26"))
sims <- sims |> filter(date >= as.Date("2021-12-15") & date <= as.Date("2022-05-26"))

# ALS phase shading 
als_shading <- tibble::tibble(
  xmin = as.Date(c("2021-12-15", "2021-12-24", "2022-01-08", "2022-02-07", "2022-03-14")),
  xmax = as.Date(c("2021-12-24", "2022-01-08", "2022-02-07", "2022-03-14", "2022-05-26")),
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

beta_annot <- tibble::tibble(
  x = as.Date(c("2021-12-18", "2022-01-02", "2022-02-02", "2022-03-06", "2022-05-01")),
  y = c(0.21, 0.21, 0.18, 0.18, 0.18),
  label = c(
  "beta[2] == 0.21",
  "beta[3] == 0.21",
  "beta[4] == 0.23",
  "beta[3[m]] == 0.28",
  "beta[0] == 0.26"
),
  matrix = "beta"
)

model_fit <- ggplot() +
  geom_rect(data = als_shading,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill_lab),inherit.aes = FALSE, alpha = 0.3) +
  geom_point(data = seroprevdata, aes(x = date, y = value, color = "Data"), size = 5) +
  geom_ribbon(data = sims, aes(x = date, ymin = conf.low, ymax = conf.high, fill = "95% CI"), alpha = 0.3) +
  geom_ribbon(data = filter(sims, matrix == "sero_inc"),
              aes(x = date, ymin = pmax(0, value - 0.25 * sd(value)), ymax = value + 0.25 * sd(value)),fill = "red", alpha = 0.3) +
  geom_ribbon(data = filter(sims, matrix == "beta"),
              aes(x = date, ymin = pmax(0, value - 0.25 * sd(value)), ymax = value + 0.25 * sd(value)),fill = "red", alpha = 0.3) +
  geom_line(data = sims,
            aes(x = date, y = value, color = ifelse(matrix == "beta", "Transmission rate", "Model fit")),linewidth = 1.5) +
  geom_text(data = filter(beta_annot, x < as.Date("2022-01-10")), 
          aes(x = x, y = y, label = label), parse = TRUE,size = 6, angle = 90, hjust = 1, color = "black",face = "bold") +
  geom_text(data = filter(beta_annot, x >= as.Date("2022-01-10")), 
          aes(x = x, y = y, label = label), parse = TRUE,size = 6, angle = 0, hjust = 1, color = "black",face = "bold")+
  geom_vline(data = als_data, aes(xintercept = date, linetype = phase),color = "gold4", linewidth = 0.8, show.legend = FALSE) +
  facet_wrap(~matrix, scales = "free_y", ncol = 1,
	     labeller = labeller(matrix = c(serop = "Seroprevalence (%)",
					    sero_inc = "Estimated Sero-Incidence",
					    beta = "Transmission rate"))
	     ) +
  scale_color_manual(name = NULL,values = c("Data" = "black", 
					  "Model fit" = "red", 
					  "Transmission rate" = "blue"),
		   labels = c("Data", "Model fit", expression(beta(t)))) +
  guides(color = guide_legend(override.aes = list(linetype = c("blank", "solid", "solid"),
                                                shape = c(16, NA, NA),
                                                color = c("black", "red", "blue"))))+  
  scale_fill_manual(name = NULL, values = fill_colors) +
  labs(x = "Date (Dec 15, 2021 - May 26, 2022)") +
  scale_linetype_manual(name = "ALS Phases",
                        values = c("ALS-3" = "dashed", "ALS-4" = "dashed", "Mod-ALS-3" = "dashed","No-ALS" = "solid")) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_clean() +
  theme(
    axis.text.x = element_text(size = 18, angle = 45, hjust = 0.85),
    axis.title.x = element_text(size = 18, face = "bold", color = "black"),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 18, face = "bold", color = "black"),
    plot.title = element_text(size = 18, face = "bold", color = "black", hjust = 0.5),
    strip.text = element_text(size = 18, face = "bold", color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    legend.background = element_rect(color = NA),
    legend.margin = margin(0, 0, 0, 0),
    plot.background = element_blank()
  )

print(model_fit)


png("../figures/model_fit.png", width = 3400, height = 3400, res = 300, bg = "white", type = "cairo")
model_fit
dev.off()
