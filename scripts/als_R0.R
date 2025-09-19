# ==== Libraries ====
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggthemes)
  library(macpan2)
  library(shellpipes)
  library(grid)       # for unit()
  library(patchwork)  # for stacking plots
})

# ==== Initialize ====
rpcall("beta_plot.Rout beta_plot.R calibrate_inc.rds params.rda")
loadEnvironments()
set.seed(2025)

start_date <- as.Date("2021-12-15") - offset0
calibrator <- rdsRead("calibrate.rds")

# ==== Extract beta(t) over study window ====
fitted_data <- mp_trajectory_sd(calibrator, conf.int = TRUE) %>%
  mutate(date = as.Date(start_date) + time - 1) %>%
  filter(matrix == "beta_thing",
         date >= as.Date("2021-12-15"),
         date <= as.Date("2022-05-22")) %>%
  transmute(date, beta_thing = value)

# ==== Map dates to ALS phases ====
fitted_data <- fitted_data %>%
  mutate(alert_level = case_when(
    date >= as.Date("2021-12-15") & date <= as.Date("2021-12-31") ~ "Early",
    date >= as.Date("2022-01-01") & date <= as.Date("2022-01-03") ~ "ALS-3",
    date >= as.Date("2022-01-04") & date <= as.Date("2022-02-07") ~ "ALS-4",
    date >= as.Date("2022-02-08") & date <= as.Date("2022-03-14") ~ "ALS-3",
    date >= as.Date("2022-03-15") & date <= as.Date("2022-05-22") ~ "No-ALS",
    TRUE ~ NA_character_
  )) %>% filter(!is.na(alert_level))

# ==== Summaries per phase ====
beta_summary <- fitted_data %>%
  group_by(alert_level) %>%
  summarise(mean_value = mean(beta_thing),
            sd_value   = sd(beta_thing), .groups = "drop")

# ==== Constants for R0 ====
kappa1 <- 1; kappa2 <- 0.91; kappa3 <- 0.3
gamma_i <- 1/7; gamma_a <- 1/10; mu <- 0.324; zeta <- 0.75
p1 <- 0.15; p2 <- 0.85; p3 <- 0
bracket_term   <- mu/gamma_i + (1 - mu)*zeta/gamma_a
susceptibility <- p1*kappa1 + p2*kappa2 + p3*kappa3
mult_const     <- bracket_term * susceptibility

# ==== R0 by phase ====
r0_data <- beta_summary %>%
  mutate(R0_mean = mean_value * mult_const,
         R0_sd   = sd_value   * mult_const) %>%
  mutate(chrono  = factor(alert_level, levels = c("Early","ALS-4","ALS-3","No-ALS")))

als_colors <- c("Early" = "gray","ALS-4"="#FFD580", "ALS-3"="#87CEFA", "No-ALS"="pink")

# for label offsets
y_min_ci <- min(r0_data$R0_mean - 1.96*r0_data$R0_sd, na.rm = TRUE)
y_max_ci <- max(r0_data$R0_mean + 1.96*r0_data$R0_sd, na.rm = TRUE)
y_span   <- y_max_ci - y_min_ci

# ==== Main plot ====
gg_main <- ggplot(r0_data, aes(x = chrono, y = R0_mean, color = chrono)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = R0_mean - 1.96*R0_sd,
                    ymax = R0_mean + 1.96*R0_sd),
                width = 0.2, linewidth = 0.9) +
  geom_text(aes(label = sprintf("%.2f", R0_mean),
                y = R0_mean + 0.06*y_span),
            size = 9, color = "black", hjust = 1.1, vjust = 1.0) +
  scale_color_manual(values = als_colors, guide = "none") +
  labs(title = "Estimated basic reproduction number for alert levels",
       y = expression(R[0]*"(t)"), x = NULL) +
  theme_clean() + 
  theme(axis.text.x = element_text(size = 25), 
        axis.title.x = element_text(size = 25), 
        axis.text.y = element_text(size = 25), 
        axis.title.y = element_text(size = 25), 
        plot.title = element_text(size = 25, hjust = 0.5,face = "plain"), 
        legend.text = element_text(size = 25), 
        legend.background = element_rect(color = NA), 
        legend.position = "bottom", 
        plot.background = element_blank())

# ==== Arrow strip with extra space ====
# start arrow at first non-"Early" level; end at the last level
lvl      <- levels(r0_data$chrono)
x_start  <- if ("ALS-4" %in% lvl) match("ALS-4", lvl) else min(which(lvl != "Early"))
x_end    <- length(lvl)

gg_arrow <- ggplot() +
  annotate("segment",
           x = x_start, xend = x_end, y = 0, yend = 0,
           arrow = arrow(length = unit(0.25, "inches"), ends = "last", type = "closed"),
           linewidth = 1.5, color = "black") +
  annotate("text",
           x = (x_start + x_end)/2, y = -0.5,
           label = "decreasing strictness", size = 11, color = "black") +
  xlim(1, x_end) +
  ylim(-1, 1) +
  theme_void()

# ==== Stack and save ====
final_plot <- gg_main / gg_arrow + plot_layout(heights = c(12, 2)) # more bottom space

print(final_plot)

# png("../figures/als_R0.png", width = 5000, height = 2500, res = 300, bg = "white", type = "cairo")
# final_plot
# dev.off()
