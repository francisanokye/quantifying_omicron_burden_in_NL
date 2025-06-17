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
library(minpack.lm)
library(coda)
library(xtable)
library(gridExtra)
library(mgcv)
library(tidyverse)
library(growthcurver)
options(scipen = 999) 

vaccination_coverage <- read.csv("vaccination-coverage-map.csv")
vaccination_coverage <- vaccination_coverage |>
  mutate(week_end = as.Date(vaccination_coverage$week_end, format = "%Y-%m-%d")) |>
  filter((vaccination_coverage$prename == "Newfoundland and Labrador") & 
           (vaccination_coverage$week_end >= "2021-12-11") & 
           (vaccination_coverage$week_end < "2022-06-20")
         ) 

vaccination_coverage <- vaccination_coverage[c("week_end","numtotal_fully","numtotal_additional")] # 

vaccination_coverage$numtotal_additional[1] <- 0

vaccination_coverage$day <- as.integer(difftime(vaccination_coverage$week_end, vaccination_coverage$week_end[1], units = "days")) + 1

# --- prepare Data ---
df_double_vac <- tibble(days = vaccination_coverage$day,full_dose = vaccination_coverage$numtotal_fully)

# normalize for scaled logistic
y_min <- min(df_double_vac$full_dose, na.rm = TRUE)
y_max <- max(df_double_vac$full_dose, na.rm = TRUE)

df_double_vac <- df_double_vac %>% mutate(yscaled = (full_dose - y_min) / (y_max - y_min))

log4p_model <- nlsLM(full_dose ~ L + (K - L) / (1 + exp((t0 - days) * r)),
                     data = df_double_vac, start = list(L = y_min, K = y_max, t0 = 58, r = 0.07))

# predictions
df_double_vac <- df_double_vac %>%
  mutate(fit_log4p = predict(log4p_model))

# individual model plots
plot_model_fit <- function(data, yvar, fitvar, model_name) {
  ggplot(data, aes(x = days)) +
    geom_point(aes_string(y = yvar), color = "black", size = 1.5) +
    geom_line(aes_string(y = fitvar), color = "red", linewidth = 1) +
    labs(title = model_name, x = "Day", y = "Fully Vaccinated") +
    theme_minimal()+
    theme(
      axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5),
      axis.title.x = element_text(size = 8, color = "black", face = "bold"),
      axis.text.y = element_text(size = 8),
      axis.title.y = element_text(size = 8, color = "black", face = "bold"),
      plot.title = element_text(size = 0, face = "bold", color = "black", hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(size = 0),
      legend.text = element_text(size = 8),
      legend.background = element_rect(color = NA),
      legend.margin = margin(0, 0, 0, 0),
      plot.background = element_blank()
    )
}

plot_model_fit(df_double_vac, "full_dose", "fit_log4p", "4p Logistic")

df_double_vac <- df_double_vac %>%
  mutate(double_daily_rate = abs(diff(c(NA, df_double_vac$fit_log4p))/7)) %>%
  mutate(double_daily_rate = as.integer(double_daily_rate))

df_double_vac$double_daily_rate[1] <- 0

df_double_vac <- df_double_vac[c("days","double_daily_rate")]
df_double_vac
# write.csv(double_vac,"~/Documents/MUN/quantifying_omicron_burden_in_NL/data/daily_2vac_rate.csv",row.names = FALSE)
