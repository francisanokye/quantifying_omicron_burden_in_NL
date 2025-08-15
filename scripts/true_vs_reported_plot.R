suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(tidyr)
  library(ggthemes)
  library(cowplot)
  library(shellpipes)
  library(macpan2)
})

# --- setup --------------------------------------------------------------------

# load analysis environment and set seed
loadEnvironments()
set.seed(2025)

# study window (model start uses offset0; plotting window uses jan 1–may 22, 2022)
start_date <- as.Date("2021-12-15") - offset0
last_date  <- "2022-05-22"

# --- model output (estimated infections) --------------------------------------

# simulate trajectory with confidence intervals from calibrated model
calibrator   <- rdsRead("calibrate.rds")
fitted_data  <- mp_trajectory_sd(calibrator, conf.int = TRUE)

# convert long trajectory to daily frame for the variables of interest
fitted_data <- fitted_data %>%
  mutate(date = as.Date(start_date) + as.numeric(time) - 1) %>%
  dplyr::filter(between(date, as.Date(start_date), as.Date(last_date))) %>%
  dplyr::filter(matrix %in% c("date", "inc", "conf.low", "conf.high"))

# widen matrices to columns and keep one record per date
true_infections <- fitted_data %>%
  select(-any_of(c("row", "col"))) %>%
  pivot_wider(names_from = matrix, values_from = value) %>%
  group_by(date) %>%
  mutate(across(everything(), ~ first(na.omit(.)), .names = "{.col}")) %>%
  ungroup() %>%
  distinct(date, .keep_all = TRUE) %>%
  drop_na() %>%
  dplyr::filter(date >= as.Date("2022-01-01") & date <= as.Date("2022-05-22"))

# --- reported data ------------------------------------------------------------

# load line list of reported daily cases (expects columns: date, cases)
reported_cases <- csvRead()
reported_cases$date <- as.Date(reported_cases$date, format = "%Y-%m-%d")

# trim to study window and remove missing
reported_cases <- reported_cases %>%
  dplyr::filter(date >= as.Date("2022-01-01") & date <= as.Date("2022-05-22")) %>%
  drop_na()

# --- assemble panel a data (daily series) -------------------------------------

# standardize columns for binding
d1 <- reported_cases[c("date", "cases")]
d1$type <- "reported"
colnames(d1) <- c("date", "inc", "type")

d2 <- true_infections[c("date", "inc")]
d2$type <- "true_infections"

allcases <- rbind(d1, d2) %>%
  mutate(type = factor(type, levels = c("reported", "true_infections")))

# --- panel a: daily incidence (reported vs estimated) with testing cuts -------

combined_case_plot <- ggplot(allcases, aes(x = date)) +
  geom_point(data = dplyr::filter(allcases, type == "true_infections"),
             aes(y = inc, color = "Seroincidence"), size = 2) +
  geom_line(data = dplyr::filter(allcases, type == "true_infections"),
            aes(y = inc, color = "Seroincidence"), linewidth = 1.2) +
  geom_point(data = dplyr::filter(allcases, type == "reported"),
             aes(y = inc, color = "Reported Cases"), size = 2) +
  geom_line(data = dplyr::filter(allcases, type == "reported"),
            aes(y = inc, color = "Reported Cases"), linewidth = 1.2) +
  scale_color_manual(NULL, values = c("Reported Cases" = "darkgreen",
                                      "Seroincidence"  = "red")) +
  labs(x = "Date (dec 15, 2021 – may 22, 2022)",
       y = "Number of cases",
       title = "Daily incidence: reported vs estimated seroincidence") +
  scale_x_date(expand = c(0, 0), date_breaks = "2 week", date_labels = "%b %d") +
  geom_vline(aes(xintercept = as.Date("2022-01-03")), colour = "purple", linetype = 4, linewidth = 1) +
  geom_vline(aes(xintercept = as.Date("2022-01-24")), colour = "purple", linetype = 4, linewidth = 1) +
  geom_vline(aes(xintercept = as.Date("2022-02-25")), colour = "purple", linetype = 4, linewidth = 1) +
  geom_vline(aes(xintercept = as.Date("2022-03-17")), colour = "purple", linetype = 4, linewidth = 1) +
  annotate("text", x = as.Date("2022-01-18"), y = 1000, label = "T1", size = 6, hjust = 1, fontface = "bold") +
  annotate("text", x = as.Date("2022-02-15"), y = 1000, label = "T2", size = 6, hjust = 1, fontface = "bold") +
  annotate("text", x = as.Date("2022-03-15"), y = 1000, label = "T3", size = 6, hjust = 1, fontface = "bold") +
  annotate("text", x = as.Date("2022-04-25"), y = 1000, label = "T4", size = 6, hjust = 1, fontface = "bold") +
  theme_clean() +
  theme(axis.text.x   = element_text(size = 14),
        axis.title.x  = element_text(size = 20),
        axis.text.y   = element_text(size = 20),
        axis.title.y  = element_text(size = 20),
        plot.title    = element_text(size = 20, hjust = 0.5, face = "plain"),
        legend.position = "bottom",
        legend.text   = element_text(size = 20),
        legend.background = element_rect(color = NA),
        plot.background   = element_blank())

# --- panel b data (underreporting ratio + period means) -----------------------

# keep only needed columns and ensure ordering
reported_cases <- reported_cases %>%
  arrange(date) %>%
  select(date, cases)

true_infections <- true_infections %>%
  arrange(date) %>%
  select(date, inc)

# daily ratio (safe division)
underreporting_df <- left_join(true_infections, reported_cases, by = "date") %>%
  mutate(ratio = ifelse(cases > 0, inc / cases, NA_real_))

# testing-eligibility periods
t_breaks <- as.Date(c("2022-01-03","2022-01-24","2022-02-25","2022-03-17","2022-05-22"))
t_labels <- c("T1","T2","T3","T4")

underreporting_df <- underreporting_df %>%
  mutate(T = cut(date, breaks = t_breaks, labels = t_labels, right = FALSE))

# per-period summaries (mean ratio + date bounds and midpoints)
period_summary <- underreporting_df %>%
  filter(!is.na(T)) %>%
  group_by(T) %>%
  summarise(
    mean_ratio = mean(ratio, na.rm = TRUE),
    x_start    = min(date, na.rm = TRUE),
    x_end      = max(date, na.rm = TRUE),
    x_mid      = x_start + (x_end - x_start) / 2,
    .groups    = "drop"
  )

# manual y-positions for period mean lines (edit values to adjust heights)
ypos_map <- tibble::tibble(
  T     = factor(c("T1","T2","T3","T4"), levels = levels(period_summary$T)),
  y_pos = c(1.1, 2.9, 2.6, 21.4)
)

period_summary <- dplyr::left_join(period_summary, ypos_map, by = "T")

# set y-limits to ensure lines/labels are visible
ylim_top <- max(
  1.1 * max(underreporting_df$ratio, na.rm = TRUE),
  1.1 * max(period_summary$y_pos, na.rm = TRUE)
)

# --- panel b: underreporting ratio over time with period mean overlays --------

# -- data for translucent bars (0 up to period mean) --
bars_df <- period_summary %>%
  transmute(xmin = x_start, xmax = x_end, ymin = 0, ymax = y_pos, T)

# -- Panel B with transparent bars + mean line on top --

const_cum <- ggplot(underreporting_df, aes(x = date, y = ratio)) +
  geom_rect(
    data = bars_df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = "Testing period mean"),
    inherit.aes = FALSE, alpha = 0.18, color = NA
  ) +
  geom_segment(
    data = period_summary,
    aes(x = x_start, xend = x_end, y = y_pos, yend = y_pos),
    inherit.aes = FALSE, linewidth = 1.4, color = "maroon"
  ) +
  # label the period mean on the bar
  geom_text(
    data = period_summary,
    aes(x = x_mid, y = y_pos, label = paste0(T, ": ", round(mean_ratio, 1))),
    inherit.aes = FALSE, vjust = -1.5, hjust = 0.68, size = 6, fontface = "bold"
  ) +
  # daily ratio curve (legend key)
  geom_line(aes(color = "Daily ratio"), linewidth = 1.2, na.rm = TRUE) +
  geom_point(aes(color = "Daily ratio"), size = 2, na.rm = TRUE) +
  # vertical cut lines (kept out of legend)
  geom_vline(
    xintercept = as.Date(c("2022-01-03","2022-01-24","2022-02-25","2022-03-17")),
    colour = "purple", linetype = 4, linewidth = 1
  ) +
  labs(
    x = "Date (dec 15, 2021 – may 22, 2022)",
    y = "Underreporting ratio (estimated / reported)",
    title = "Underreporting ratio over time with testing period means",
    fill = NULL, color = NULL
  ) +
  scale_x_date(expand = c(0, 0), date_breaks = "2 week", date_labels = "%b %d") +
  scale_y_continuous(limits = c(0, ylim_top)) +
  # legend entries and colors
  scale_fill_manual(values = c("Testing period mean" = "pink4")) +
  scale_color_manual(values = c("Daily ratio" = "blue")) +
  # make the bar legend patch translucent like the plot
  guides(
    fill = guide_legend(override.aes = list(alpha = 0.18, linetype = 0)),
    color = guide_legend(override.aes = list(size = 2))
  ) +
  theme_clean() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title  = element_text(size = 20, hjust = 0.5, face = "plain"),
    legend.position = "bottom",
    legend.text = element_text(size = 20),
    legend.background = element_rect(color = NA),
    plot.background = element_blank()
  )



# --- combine panels and export ------------------------------------------------

gg <- cowplot::plot_grid(
  combined_case_plot,
  const_cum,
  labels = "AUTO",
  nrow = 1,
  align = "v",
  axis = "tb",
  rel_widths = c(1, 1)
)

print(gg)

# png("../figures/Figure_3.png", width = 5000, height = 2500, res = 300, bg = "white", type = "cairo")
# gg
# dev.off()