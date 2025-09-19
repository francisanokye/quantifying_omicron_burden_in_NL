# packages
library(readr); library(dplyr); library(lubridate)
library(ggplot2); library(tidyr); library(ggthemes)

# load and window data ----
dat <- read_tsv("../data/BA.1.tsv", show_col_types = FALSE)
names(dat) <- sub("%\\s*", "pct_", tolower(names(dat)))  

dat <- dat |>
  mutate(date = ymd(date)) |>
  filter(date >= ymd("2021-12-10"), date <= ymd("2022-06-10"))

# collapse specified ba.1 sublineages into "BA.1"; keep ba.1.20 and ba.1.3; others -> "other lineages" 
collapse_list <- c("BA.1","BA.1.1","BA.1.1.10","BA.1.1.16",
                   "BA.1.1.18","BA.1.1.6","BA.1.14","BA.1.15",
                   "BA.1.17","BA.1.17.2","BA.1.20","BA.1.3")

dat <- dat |>
  mutate(
    lineage2 = case_when(
      lineage %in% collapse_list ~ "BA.1",
      TRUE                       ~ "other lineages"
    ),
    frac_raw = suppressWarnings(readr::parse_number(pct_frequency) / 100),
    frac_raw = ifelse(is.finite(frac_raw), frac_raw, 0),
    week     = floor_date(date, "week", week_start = 1)
  )

# aggregate to week × lineage and normalize so each week sums to 1 ----
wk <- dat |>
  group_by(week, lineage = lineage2) |>
  summarize(frac = mean(frac_raw, na.rm = TRUE), .groups = "drop") |>
  group_by(week) |>
  mutate(total = sum(frac, na.rm = TRUE),
         frac  = ifelse(total > 0, frac / total, 0)) |>
  ungroup() |>
  transmute(
    week,
    lineage = factor(lineage, levels = c("BA.1","BA.1.20","BA.1.3","other lineages")),
    frac    = pmin(pmax(as.numeric(frac), 0), 1)
  )

# ---- color-blind–friendly palette (okabe–ito); ba.1 is red ----
cols <- c(
  "BA.1"            = "red",  
  "other lineages"  = "blue"   
)

# ---- plot: fraction scale 0–1, plain title, larger font ----
p <- ggplot(wk, aes(week, frac, fill = lineage)) +
  geom_col(width = 6) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.1),
                     labels = scales::number_format(accuracy = 0.1),
                     expand = expansion(mult = c(0, 0))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  scale_fill_manual(values = cols, drop = FALSE) +
  labs(
    title = "sublineage composition by week (canada)",
    x = "date",
    y = "weekly lineage share (fraction)",
    fill = "lineages"
  ) +
  theme_clean() +
  theme(
    plot.title      = element_text(face = "plain", size = 14, hjust = 0.5),
    axis.title.x    = element_text(size = 10),
    axis.title.y    = element_text(size = 10),
    axis.text.x     = element_text(size = 10),
    axis.text.y     = element_text(size = 10),
    legend.position = "bottom",
    legend.title    = element_text(size = 0),
    legend.text     = element_text(size = 10),
    plot.background = element_blank(),
    legend.background = element_blank()
  )

p