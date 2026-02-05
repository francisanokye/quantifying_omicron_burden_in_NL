
library(dplyr)
library(lubridate)
library(ggplot2)
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

# --- config ----------------------------------------------------------------
date_start <- as.Date("2021-12-15")
date_end   <- as.Date("2022-05-22")

# --- inputs ----------------------------------------------------------------
calibrator   <- rdsRead("R1_calibrate.rds")
fitserodata <- rdsRead("R1_fitsero.rds")
# correct first value by replacing with the second
fitserodata$value[1] <- fitserodata$value[2]

# --- seroincidence (daily; filled) ----------------------------------------
fitserodata <- fitserodata %>%
  complete(date = seq.Date(from = date_start, to = date_end, by = "1 day")) %>%
  select(date, value) %>%
  filter(date >= date_start, date <= date_end) 

# --- model trajectory (if needed later; kept) ------------------------------
sims <- calibrator %>%
  mp_trajectory_sd(conf.int = TRUE, back_transform = TRUE, conf.level = 0.99) %>%
  filter(time >= offset0, matrix == "newR") %>% #   "serop_total"
  mutate(date = seq.Date(from = date_start, by = "1 day", length.out = n())) %>%
  filter(date >= date_start, date <= date_end)

stopifnot(nrow(sims) > 0)

# --- als shading + phase lines ---------------------------------------------
als_shading <- tibble(
  xmin     = as.Date(c("2021-12-15", "2021-12-24", "2022-01-08", "2022-02-07", "2022-03-14")),
  xmax     = as.Date(c("2021-12-24", "2022-01-08", "2022-02-07", "2022-03-14", "2022-05-22")),
  fill_lab = c("ALS-2", "ALS-3", "ALS-4", "ALS-3", "No-ALS")
) %>%
  mutate(fill_lab = factor(fill_lab, levels = c("ALS-2","ALS-3","ALS-4","No-ALS")))

als_lines <- tibble(
  date = as.Date(c("2021-12-15", "2021-12-24", "2022-01-08", "2022-02-07", "2022-03-14"))
)

als_fill_colors <- c(
  "ALS-2"  = "#66D1B5",
  "ALS-3"  = "#87CEFA",
  "ALS-4"  = "#FFD580",
  "No-ALS" = "pink"
)

facet_lab <- c("newR" = "Seroincidence estimates in NL")

# --- core plot --------------------------------------------------------------
p <- ggplot() +
  geom_rect(
    data = tibble(xmin = date_start, xmax = as.Date("2022-01-01"),
                  ymin = -Inf, ymax = Inf),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey20", alpha = 0.50, show.legend = FALSE
  ) +
  geom_rect(
    data = als_shading,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill_lab),
    inherit.aes = FALSE,
    alpha = 0.30,
    show.legend = TRUE
  ) +
  geom_point(
    data = fitserodata,
    aes(x = date, y = value, color = "serosurveillance"),
    size = 3.5, show.legend = TRUE
  ) +
  geom_ribbon(
    data = sims,
    aes(x = date, ymin = conf.low, ymax = conf.high),
    fill = "red", alpha = 0.40, show.legend = FALSE
  ) +
  geom_line(
    data = sims,
    aes(x = date, y = value, color = "model"),
    linewidth = 1.5, show.legend = TRUE
  ) +
  geom_vline(
    data = als_lines,
    aes(xintercept = date),
    colour = "gold4", linewidth = 0.8,
    show.legend = FALSE
  ) +
  facet_wrap(
    ~matrix, scales = "free_y", ncol = 1,
    labeller = labeller(matrix = facet_lab)
  ) +
  scale_colour_manual(
    name   = NULL,
    values = c("serosurveillance" = "black", "model" = "red"),
    breaks = c("serosurveillance", "model"),
    labels = c("serosurveillance", "model"),
    guide  = guide_legend(override.aes = list(linetype = c(0, 1), shape = c(16, NA), linewidth  = c(0, 1.2)))
  ) +
  scale_fill_manual(
    name   = "ALS level",
    values = als_fill_colors,
    breaks = c("ALS-2","ALS-3","ALS-4","No-ALS"),
    guide  = guide_legend(override.aes = list(alpha = 0.80, shape = NA, colour = NA))
  ) +
  labs(
    y = "Number of infection incidence",
    title = "Infection incidence estimated from serosurveillance and model fit"
  ) +
  scale_x_date(
    limits = c(date_start, date_end + days(2)),
    expand = expansion(mult = c(0.00, 0.01)),
    date_breaks = "2 week",
    date_labels = "%b %d"
  ) +
  theme_clean() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_text(size = 25, hjust = 0.85),
    axis.text.y  = element_text(size = 28),
    axis.title.y = element_text(size = 25, colour = "black"),
    strip.text   = element_text(size = 0, colour = "black"),  
    strip.background = element_blank(),
    plot.title   = element_text(size = 30, hjust = 0.5, face = "plain"),
    legend.title = element_text(size = 25),
    legend.position = c(0.20, 0.80), 
    legend.text  = element_text(size = 25),
    legend.key            = element_blank(),
    legend.key.background = element_blank(),
    legend.background     = element_blank(),
    plot.background   = element_blank()
  )

# --- legend to overlay (Data+Model only) -----------------------------------
p_inside_leg <- p +
  guides(fill = "none") +                 
  theme(
    legend.position = "right",            
    legend.box.margin = margin(0, 0, 0, 0),
    legend.key            = element_blank(),
    legend.key.background = element_blank(),
    legend.background     = element_blank()
  )

inside_leg <- cowplot::get_legend(p_inside_leg)

# --- base plot (ALS legend bottom only) ------------------------------------
p_base <- p +
  guides(colour = "none") +               
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.box       = "horizontal",
    legend.key = element_blank(),
    legend.key.background = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )

final_plot <- cowplot::ggdraw(p_base) +
  cowplot::draw_grob(inside_leg, x = 0.12, y = 0.62, width = 0.30, height = 0.22)

print(final_plot)


png("../figures/Fig3.png", width = 5000, height = 2500, res = 300, bg = "white", type = "cairo")
final_plot
dev.off()
