library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(zoo)
library(forcats)
library(ggthemes)
library(cowplot)
library(patchwork)
library(shellpipes)
library(macpan2)

# load environment and set seed
loadEnvironments()
set.seed(2025)

# define date range
start_date <- as.Date("2021-12-15") - offset0
last_date <-"2022-05-22"

# model simulation with calibrated parameters
calibrator <- rdsRead("calibrate.rds")
fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE)

# filter and reshape simulation output
fitted_data <- (fitted_data
                |> mutate(date = as.Date(start_date) + as.numeric(time) -1 )
                |> dplyr::filter(between(date, as.Date(start_date), as.Date(last_date)))
                |> dplyr::filter(matrix %in% c("date","inc","conf.low","conf.high"))
)

# convert matrix values into columns
true_infections <- fitted_data |>
  select(-any_of(c("row", "col"))) |>
  pivot_wider(names_from = matrix, values_from = value) |>
  group_by(date) |>
  mutate(across(everything(), ~ first(na.omit(.)), .names = "{.col}")) |>
  ungroup() |>
  distinct(date, .keep_all = TRUE) |>
  drop_na()

# trim date range
true_infections <- true_infections |>
  dplyr::filter(date >= as.Date("2022-01-01") & date <= as.Date("2022-05-22"))

# load reported data
reported_cases <- csvRead()
reported_cases$date <- as.Date(reported_cases$date, format = "%Y-%m-%d")

# trim and clean reported data
reported_cases <- reported_cases |>
  dplyr::filter(date >= as.Date("2022-01-01") & date <= as.Date("2022-05-22")) |>
  drop_na()

# align data formats
d1 <- reported_cases[c("date","cases")]
d1$type <- "reported"
colnames(d1) <- c("date","inc","type")

d2 <- true_infections[c("date","inc")]
d2$type <- "true_infections"

allcases <- rbind(d1,d2)

# factor levels
allcases <- (allcases |>
               mutate(type = factor(type, levels = c("reported", "true_infections"))))

# compute cumulative totals
last_date <- max(allcases$date)
final_vals <- allcases %>%
  group_by(type) %>%
  arrange(date) %>%
  summarise(
    cum_total = sum(inc, na.rm = TRUE),
    .groups = "drop"
  )

# prepare text labels
text_labels <- tibble(
  date = last_date - 17,
  cum_total = final_vals$cum_total * 1.01,
  label = ifelse(
    final_vals$type == "reported",
    paste0(scales::comma(final_vals$cum_total)),
    paste0(scales::comma(final_vals$cum_total))
  ),
  type = ifelse(final_vals$type == "reported", "Reported Cases", "Seroincidence")
)

# build main plot
combined_case_plot <- (ggplot(allcases, aes(x = date)) +
                         geom_rect(aes(xmin=ymd('2022-03-17'), xmax = ymd('2022-05-22'), ymin = 0, ymax = Inf), fill = adjustcolor("#F7E2E2", alpha = 0.03), alpha = 0.05) +
                         geom_line(data = true_infections, aes(y = cumsum(inc), color = "Seroincidence"),linewidth = 1.5) +
                         geom_point(data = filter(allcases, type == "reported"), aes(y = cumsum(inc), color = "Reported Cases"),size = 1.5) +
                         geom_line(data = filter(allcases, type == "reported"), aes(y = cumsum(inc), color = "Reported Cases"),linewidth = 0.5) +
                         scale_color_manual(name = NULL,values = c("Reported Cases" = "darkgreen", "Seroincidence" = "red")) +
                         labs(x = "Date (Dec 15, 2021 - May 22, 2022)",y = "Cumulative Number of Cases",title = "Cumulative Infections: Reported vrs Estimated Seroincidence") +
                         scale_x_date(date_breaks = "2 week",date_labels = "%b %d") +
                         geom_segment(aes(x = as.Date("2022-01-04"), y = 0, yend = Inf),linetype = "dashed",color = "gold4",linewidth = 1) +
                         geom_segment(aes(x = as.Date("2022-02-07"), y = 0, yend = Inf),linetype = "dashed",color = "gold4",linewidth = 1) +
                         geom_segment(aes(x = as.Date("2022-03-14"), y = 0, yend = Inf),linetype = "solid",color = "black",linewidth = 1) +
                         annotate("text", x = as.Date("2022-01-01"), y = 70000, label = "ALS-3", size = 6, angle = 90, hjust = 1) +
                         annotate("text", x = as.Date("2022-01-30"), y = 70000, label = "ALS-4", size = 6, hjust = 1) +
                         annotate("text", x = as.Date("2022-03-05"), y = 70000, label = "ALS-3", size = 6, hjust = 1) +
                         annotate("text", x = as.Date("2022-04-20"), y = 70000, label = "No-ALS", size = 6, hjust = 1) +
                         theme_clean() +
                         theme(
                           axis.text.x = element_text(size = 12),
                           axis.title.x = element_text(size = 16),
                           axis.text.y = element_text(size = 16),
                           axis.title.y = element_text(size = 16),
                           plot.title = element_text(size = 16, hjust = 0.5),
                           legend.position = "bottom",
                           legend.text = element_text(size = 16),
                           legend.background = element_rect(color = NA),
                           plot.background = element_blank()
                         ))

# add cumulative labels
combined_case_plot <- combined_case_plot +
  geom_text(
    data = text_labels,
    aes(
      x = date,
      y = cum_total,
      label = label,
      color = type
    ),
    hjust = 0.1,
    vjust = 0,
    size = 6,
    fontface = "plain",
    show.legend = FALSE
  )

# compute cumulative curves
reported_cases_cum <- reported_cases %>%
  arrange(date) %>%
  mutate(cum_cases = cumsum(cases)) %>%
  select(date, cum_cases)

true_infections_cum <- true_infections %>%
  arrange(date) %>%
  mutate(cum_inc = cumsum(inc)) %>%
  select(date, cum_inc)

# join and compute underreporting ratio
underreporting_df <- left_join(true_infections_cum, reported_cases_cum, by = "date") %>%
  mutate(ratio = cum_inc / cum_cases) %>%
  mutate(ratio = ifelse(is.finite(ratio), ratio, NA)) %>%
  mutate(ratio_clipped = pmin(ratio, 10))

# compute overall underreporting
tot_obs <- sum(reported_cases$cases, na.rm = TRUE)
tot_const <- sum(true_infections$inc, na.rm = TRUE)
overall_ratio <- tot_const / tot_obs
underreporting_rate <- 1 - (1 / overall_ratio)

# build inset plot
const_cum <- ggplot(underreporting_df, aes(x = date, y = ratio_clipped, color = "underreporting ratio")) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  annotate(
    "text",
    x = min(underreporting_df$date) + 100,
    y = 5.5,
    label = paste0("Factor: ", round(overall_ratio, 1),
                   "\nRate: ", scales::percent(underreporting_rate, accuracy = 0.1)),
    size = 5,
    hjust = 0,
    color = "black",
    fontface = "bold"
  ) +
  scale_y_continuous(
    limits = c(0, 7),
    breaks = seq(0, 7, by = 2),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  geom_vline(data = underreporting_df, aes(xintercept = as.Date("2022-01-03")), colour = "purple", linetype = 4, linewidth = 1) +
  geom_vline(data = underreporting_df, aes(xintercept = as.Date("2022-01-24")), colour = "purple", linetype = 4, linewidth = 1) +
  geom_vline(data = underreporting_df, aes(xintercept = as.Date("2022-02-25")), colour = "purple", linetype = 4, linewidth = 1)+
  geom_vline(data = underreporting_df, aes(xintercept = as.Date("2022-03-17")), colour = "purple", linetype = 4, linewidth = 1)+
  annotate("text", x = as.Date("2022-01-18"), y = 5, label = "T2", size = 6, hjust = 1) +
  annotate("text", x = as.Date("2022-02-15"), y = 5, label = "T3", size = 6, hjust = 1) +
  annotate("text", x = as.Date("2022-03-15"), y = 5, label = "T4", size = 6, hjust = 1) +
  annotate("text", x = as.Date("2022-04-25"), y = 2, label = "T5", size = 6, hjust = 1) +
  labs(x = "Date (Dec 15, 2021 - May 22, 2022)",y = "Underreporting Ratio",title = "Underreporting Ratio Over Time") +
  scale_x_date(expand = c(0, 0),date_breaks = "2 week",date_labels = "%b %d") +
  scale_color_manual(name = NULL,values = c("underreporting ratio" = "steelblue")) +
  theme_clean() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.background = element_rect(color = NA),
    legend.position = "bottom",
    plot.background = element_blank()
  )

# combine plots side-by-side
final_plot <- plot_grid(
  combined_case_plot, 
  const_cum,
  labels = "AUTO",
  nrow = 1,
  align = "v",
  axis = "tb",
  rel_widths = c(1, 1)
)

# export figure
png("../figures/Figure_2.png", width = 5000, height = 2500, res = 300, bg = "white", type = "cairo")
final_plot
dev.off()
