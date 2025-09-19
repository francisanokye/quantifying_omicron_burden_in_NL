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
  dplyr::filter(date >= as.Date("2021-12-15") & date <= as.Date("2022-05-22"))

# --- reported data ------------------------------------------------------------

# load line list of reported daily cases (expects columns: date, cases)
reported_cases <- csvRead()
reported_cases$date <- as.Date(reported_cases$date, format = "%Y-%m-%d")

# trim to study window and remove missing
reported_cases <- reported_cases %>%
  dplyr::filter(date >= as.Date("2021-12-15") & date <= as.Date("2022-05-22")) %>%
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
  geom_rect(
  data = data.frame(xmin = as.Date("2021-12-15"), xmax = as.Date("2022-01-02"), ymin = -Inf,ymax =  Inf),
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  inherit.aes = FALSE,
  fill = "grey20",
  alpha = 0.35,
  show.legend = FALSE
)+
  geom_point(data = dplyr::filter(allcases, type == "true_infections"),
             aes(y = inc, color = "seroincidence (estimated)"), size = 2) +
  geom_line(data = dplyr::filter(allcases, type == "true_infections"),
            aes(y = inc, color = "seroincidence (estimated)"), linewidth = 1.2) +
  geom_point(data = dplyr::filter(allcases, type == "reported"),
             aes(y = inc, color = "reported"), size = 2) +
  geom_line(data = dplyr::filter(allcases, type == "reported"),
            aes(y = inc, color = "reported"), linewidth = 1.2) +
  scale_color_manual(NULL, values = c("reported" = "#4D4D4D","seroincidence (estimated)"  = "red")) +
  labs(y = "Number of infections",title = "Daily infections: reported vs estimated") +
  scale_x_date(expand = c(0, 0), date_breaks = "2 week", date_labels = "%b %d") +
  geom_vline(aes(xintercept = as.Date("2021-12-15")), colour = "gold4", linetype = 4, linewidth = 1.5) +
  geom_vline(aes(xintercept = as.Date("2022-01-03")), colour = "gold4", linetype = 4, linewidth = 1.5) +
  geom_vline(aes(xintercept = as.Date("2022-01-24")), colour = "gold4", linetype = 4, linewidth = 1.5) +
  geom_vline(aes(xintercept = as.Date("2022-02-25")), colour = "gold4", linetype = 4, linewidth = 1.5) +
  geom_vline(aes(xintercept = as.Date("2022-03-17")), colour = "gold4", linetype = 4, linewidth = 1.5) +
  annotate("text", x = as.Date("2021-12-25"), y = 1000, label = "T1",colour = "purple", size = 7, hjust = 1, fontface = "bold") +
  annotate("text", x = as.Date("2022-01-18"), y = 1000, label = "T2", colour = "purple", size = 7, hjust = 1, fontface = "bold") +
  annotate("text", x = as.Date("2022-02-15"), y = 1000, label = "T3", colour = "purple",size = 7, hjust = 1, fontface = "bold") +
  annotate("text", x = as.Date("2022-03-15"), y = 1000, label = "T4", colour = "purple",size = 7, hjust = 1, fontface = "bold") +
  annotate("text", x = as.Date("2022-04-25"), y = 1000, label = "T5", colour = "purple",size = 7, hjust = 1, fontface = "bold") +
  theme_clean() +
  theme(axis.text.x   = element_text(size = 12),
        axis.title.x  = element_blank(),
        axis.text.y   = element_text(size = 20),
        axis.title.y  = element_text(size = 20),
        plot.title    = element_text(size = 24, hjust = 0.5, face = "plain"),
        legend.position = c(0.35,0.9),
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

# --- testing-eligibility periods (make end inclusive by +1 day) ---
t_breaks <- as.Date(c("2021-12-15","2022-01-03","2022-01-24","2022-02-25","2022-03-17","2022-05-23"))
t_labels <- c("T1","T2","T3","T4","T5")

underreporting_df <- underreporting_df %>%
  mutate(T = cut(date, breaks = t_breaks, labels = t_labels, right = FALSE))

# --- per-period summaries: mean & geometry helpers ---
period_summary <- underreporting_df %>%
  filter(!is.na(T)) %>%
  group_by(T) %>%
  summarise(
    mean_ratio = mean(ratio, na.rm = TRUE),
    x_start    = min(date, na.rm = TRUE),
    x_end      = max(date, na.rm = TRUE),
    x_mid      = x_start + (x_end - x_start) / 2,
    .groups    = "drop"
  ) %>%
  mutate(
    mean_ratio = if_else(is.nan(mean_ratio), NA_real_, mean_ratio),  
    label_txt  = scales::number(mean_ratio, accuracy = 0.1)         
  )

# --- y-limits to show lines/labels ---
ylim_top <- 1.1 * max(c(underreporting_df$ratio, period_summary$mean_ratio), na.rm = TRUE)

# --- plot ---
const_cum <- ggplot(underreporting_df, aes(x = date, y = ratio)) +
  geom_rect(
  data = data.frame(xmin = as.Date("2021-12-15"), xmax = as.Date("2022-01-02"), ymin = -Inf,ymax =  Inf),
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  inherit.aes = FALSE,
  fill = "grey20",
  alpha = 0.35,
  show.legend = FALSE
)+  
# period mean line drawn AT the mean
  geom_segment(
    data = period_summary,
    aes(x = x_start, xend = x_end, y = mean_ratio, yend = mean_ratio),
    inherit.aes = FALSE, linewidth = 1.4, color = "maroon", na.rm = TRUE
  ) +
  geom_text(
  data = dplyr::mutate(
           period_summary,
           x_lab = if_else(x_end == max(x_end, na.rm = TRUE), x_mid - 3, x_mid)  # 3 days earlier only for last
         ),
  aes(x = x_lab, y = mean_ratio, label = label_txt),
  inherit.aes = FALSE, vjust = -0.6, size = 7, fontface = "bold", na.rm = TRUE
) +

  # daily ratio curve
  geom_line(aes(color = "Daily ratio"), linewidth = 1.2, na.rm = TRUE,show.legend = FALSE) +
  geom_point(aes(color = "Daily ratio"), size = 2, na.rm = TRUE,show.legend = FALSE) +
  # vertical cut lines (all but the last break)
  geom_vline(xintercept = t_breaks[-length(t_breaks)],colour = "gold4", linetype = 4, linewidth = 1.5) +
  annotate("text", x = as.Date("2021-12-25"), y = 10, label = "T1", colour = "purple",size = 7, hjust = 1, fontface = "bold") +
  annotate("text", x = as.Date("2022-01-18"), y = 10, label = "T2", colour = "purple",size = 7, hjust = 1, fontface = "bold") +
  annotate("text", x = as.Date("2022-02-15"), y = 10, label = "T3", colour = "purple",size = 7, hjust = 1, fontface = "bold") +
  annotate("text", x = as.Date("2022-03-15"), y = 10, label = "T4", colour = "purple",size = 7, hjust = 1, fontface = "bold") +
  annotate("text", x = as.Date("2022-04-25"), y = 10, label = "T5", colour = "purple",size = 7, hjust = 1, fontface = "bold") +
  labs(
    y = "Underreporting ratio (estimated / reported)",
    title = "Underreporting ratio over time",
    fill = NULL, color = NULL
  ) +
  scale_x_date(expand = c(0,0), date_breaks = "2 week", date_labels = "%b %d") +
  scale_y_continuous(limits = c(0, ylim_top)) +
  scale_color_manual(values = c("Daily ratio" = "blue"), guide = "none") +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_clean() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title  = element_text(size = 24, hjust = 0.5, face = "plain"),
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

# png("../figures/Figure_4.png", width = 5000, height = 2500, res = 300, bg = "white", type = "cairo")
# gg
# dev.off()
