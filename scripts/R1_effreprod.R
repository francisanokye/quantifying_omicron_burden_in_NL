# ==== Load libraries ====
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(tidyr)
  library(zoo)
  library(ggthemes)
  library(patchwork)
  library(shellpipes)
rpcall("R1_muzeta.effreprod.Rout R1_effreprod.R R1_seroprevdata.rds R1_fitsero.rds params.rda R1_muzeta.calibrate.rds")
rpcall("R1_effreprod.Rout R1_effreprod.R R1_seroprevdata.rds R1_fitsero.rds params.rda R1_calibrate.rds")
  library(tidyverse)
  library(macpan2)
  library(grid)   # unit()
})

# ==== initialize ====
set.seed(2025)
options(macpan2_log_dir = ".")
loadEnvironments()

# ==== inputs / window ====
calibrator     <- readRDS("R1_calibrate.rds")
seroprevdata   <- readRDS("R1_seroprevdata.rds")
time_steps     <- max(seroprevdata$time)
upper_plot_time <- 300

anchor_start <- as.Date("2021-12-15")
anchor_end   <- as.Date("2022-05-22")

# ==== constants for Re(t) scaling (replace with params if desired) ====
kappa1 <- 1; kappa2 <- 0.91; kappa3 <- 0.3
gamma_i <- 1/7; gamma_a <- 1/10; mu <- 0.678; zeta <- 0.75
p1 <- 0.15; p2 <- 0.85; p3 <- 0

bracket_term   <- (mu/gamma_i) + ((1 - mu) * zeta / gamma_a)
susceptibility <- p1*kappa1 + p2*kappa2 + p3*kappa3
mult_const     <- bracket_term * susceptibility

# ==== extract fitted beta(t) within window (for phase labels / summaries) ====
fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE) %>%
  filter(matrix == "beta_thing") %>%
  mutate(date = anchor_start + (time - offset0)) %>%
  filter(date >= anchor_start, date <= anchor_end) %>%
  select(date, beta_thing = value)

# ==== label phases (artifacts inclusive to dec 31) ====
fitted_data <- fitted_data %>%
  mutate(
    alert_level = case_when(
      date <= as.Date("2021-12-31") ~ "Early",
      date <  as.Date("2022-01-04") ~ "ALS-3\nK-12 Closed",
      date <  as.Date("2022-01-25") ~ "ALS-4\nK-12 Closed",
      date <  as.Date("2022-02-08") ~ "ALS-4\nK-12 Open",
      date <  as.Date("2022-03-15") ~ "ALS-3\nK-12 Open",
      TRUE                          ~ "No-ALS\nK-12 Open"
    )
  )

# ==== beta summaries by phase (kept, even if not used below) ====
beta_summary <- fitted_data %>%
  group_by(alert_level) %>%
  summarise(
    mean_value = mean(beta_thing, na.rm = TRUE),
    sd_value   = sd(beta_thing,   na.rm = TRUE),
    .groups    = "drop"
  )

# ==== prepare seroprevalence data (kept) ====
seroprevdata <- seroprevdata %>%
  complete(date = seq.Date(from = anchor_start, to = max(date), by = "1 day")) %>%
  select(date, value) %>%
  filter(date >= anchor_start & date <= anchor_end)

# ==== extract beta(t) with uncertainty -> convert to Re(t) with uncertainty ====
# NOTE: This REPLACES the old 'sims' beta(t) plot target with Re(t) = beta(t) * mult_const
sims <- calibrator %>%
  mp_trajectory_sd(conf.int = TRUE, back_transform = TRUE) %>%
  filter(time >= offset0, matrix == "beta_thing") %>%
  mutate(date = seq.Date(from = anchor_start, by = "1 day", length.out = n())) %>%
  filter(date >= anchor_start & date <= anchor_end) %>%
  mutate(
    value     = value     * mult_const,
    conf.low  = conf.low  * mult_const,
    conf.high = conf.high * mult_const
  )

# ==== define ALS shading periods ====
als_shading <- tibble(
  xmin     = as.Date(c("2021-12-15", "2021-12-24", "2022-01-08", "2022-02-07", "2022-03-14")),
  xmax     = as.Date(c("2021-12-24", "2022-01-08", "2022-02-07", "2022-03-14", "2022-05-22")),
  phase    = c("ALS-2", "ALS-3", "ALS-4", "ALS-3", "No-ALS"),
  fill_lab = c("ALS-2", "ALS-3", "ALS-4", "ALS-3", "No-ALS")
) %>%
  mutate(fill_lab = factor(fill_lab, levels = c("ALS-2", "ALS-3", "ALS-4", "No-ALS")))

als_data <- tibble(
  date  = as.Date(c("2021-12-15", "2021-12-24", "2022-01-08", "2022-02-07", "2022-03-14")),
  phase = c("ALS-2", "ALS-3", "ALS-4", "ALS-3", "No-ALS")
)

# ==== ALS fill colors (solid for legend; alpha applied in geom_rect) ====
als_fill_colors <- c(
  "ALS-2"  = "#66D1B5",
  "ALS-3"  = "#87CEFA",
  "ALS-4"  = "#FFD580",
  "No-ALS" = "pink"
)

# ==== annotations (kept; y updated for Re(t) scale) ====
re_annot <- tibble(
  x      = as.Date(c("2021-12-24","2022-01-05", "2022-02-02", "2022-03-07", "2022-04-28")),
  y      = rep(1.5, 5),
  label  = c("ALS-2","ALS-3", "ALS-4", "ALS-3", "No-ALS"),
  matrix = "Re"
)

# --- define K–12 bracket regions
bracket_df <- tibble(
  xmin      = as.Date(c("2021-12-15","2021-12-20","2022-01-26")),
  xmax      = as.Date(c("2021-12-19","2022-01-25","2022-05-22")),
  sch_label = c("K-12\nSch.\nOpen","K-12 Schools Closed","K-12 Schools Open"),
  seg_col   = c("navy","red","navy"),
  lab_col   = c("navy","red","navy")
) %>%
  mutate(x_label = as.Date((as.numeric(xmin) + as.numeric(xmax))/2, origin = "1970-01-01"))

# ==== K–12 transition lines ====
k12_lines <- tibble(date = as.Date(c("2021-12-20", "2022-01-25")))

# ==== plot 1: Re(t) with CI and ALS overlays (kept structure; sims now is Re(t)) ====
p1 <- ggplot() +
  geom_rect(
    data = data.frame(
      xmin = anchor_start,
      xmax = as.Date("2022-01-01"),
      ymin = -Inf,
      ymax =  Inf
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey20",
    alpha = 0.35,
    show.legend = FALSE
  ) +
  geom_rect(
    data = als_shading,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill_lab),
    inherit.aes = FALSE,
    alpha = 0.50,
    show.legend = TRUE
  ) +
  geom_ribbon(
    data = sims,
    aes(x = date, ymin = conf.low, ymax = conf.high),
    fill = "gray30",
    alpha = 0.30,
    show.legend = FALSE
  ) +
  geom_line(
    data = sims,
    aes(x = date, y = value, color = "Effective reproduction number"),
    linewidth = 1.5
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 2.0, color = "black") +
  geom_vline(
    data = als_data,
    aes(xintercept = date),
    color = "gold4",
    linewidth = 0.8,
    show.legend = FALSE
  ) +
  geom_vline(
    xintercept = as.Date("2022-03-14"),
    color = "black",
    linetype = "dashed",
    linewidth = 2.0
  ) +
  geom_vline(
    data = k12_lines,
    aes(xintercept = date),
    linetype = "dotted",
    color = "red",
    linewidth = 1.5
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Effective reproduction number" = "black")
  ) +
  scale_fill_manual(
    name   = "ALS level",
    values = als_fill_colors,
    breaks = c("ALS-2", "ALS-3", "ALS-4", "No-ALS"),
    guide  = guide_legend(override.aes = list(alpha = 0.80))
  ) +
  scale_x_date(
    limits = c(anchor_start, anchor_end),
    date_breaks = "2 week",
    date_labels = "%b %d",
    expand = c(0, 0.5)
  ) +
  labs(
    title = expression("Time-varying effective reproduction number across alert-level phases and school closure"),
    y = expression(R[e]*"(t)"),
    x = NULL
  ) +
  theme_clean() +
  theme(
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank(),
    axis.text.y      = element_text(size = 25),
    axis.title.y     = element_text(size = 25, color = "black"),
    legend.position  = c(0.83, 0.18),
    legend.justification = c(0.5, 0.5),
    legend.direction = "horizontal",
    legend.box       = "horizontal",
    legend.title     = element_text(size = 22),
    legend.text      = element_text(size = 22),
    legend.background = element_blank(),
    plot.title       = element_text(size = 25, color = "black", hjust = 0.5),
    plot.background  = element_blank()
  )

# ==== plot 2: K–12 bracket timeline ====
p2 <- ggplot() +
  geom_segment(
    data = bracket_df,
    aes(x = xmin, xend = xmax, y = 1, yend = 1),
    colour = c("navy", "red", "navy"),
    linewidth = 1.5,
    arrow = arrow(angle = 90, ends = "both", length = unit(0.5, "cm"))
  ) +
  geom_text(
    data = bracket_df,
    aes(x = x_label, y = 0.0, label = sch_label),
    colour = c("navy", "red", "navy"),
    size = 5
  ) +
  scale_x_date(
    limits = c(anchor_start, anchor_end),
    date_breaks = "2 week",
    date_labels = "%b %d",
    expand = c(0, 0.5)
  ) +
  ylim(-1, 1) +
  theme_void() +
  theme(
    axis.text.x = element_text(size = 20),
    plot.margin = margin(0, 0, 0, 0)
  )

# == combine plots (NO '&' to avoid patchwork method error) ====
gg <- (p1 / p2) +
  plot_layout(heights = c(3, 0.7), guides = "collect") +
  plot_annotation(theme = theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.box       = "horizontal",
    legend.background = element_blank()
  ))

print(gg)

png("../figures/R1_Figure_5.png", width = 5000, height = 2500, res = 300, bg = "white", type = "cairo")
gg
dev.off()
