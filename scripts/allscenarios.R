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
library(fuzzyjoin)
library(shellpipes)

# load reported data
reported_cases <- csvRead("serop_avgcase_data")
reported_cases$date <- as.Date(reported_cases$date, format = "%Y-%m-%d")
reported_cases <- reported_cases |>
  drop_na()

# load the saved estimated true infections
true_infections <- csvRead("true_infections_data")
# load the estimated predicted reported based on the RT-PCR eligibility fractions
eligibility_adjusted_cases <- csvRead("eligibility_adjusted_cases")

# convert matrix values into columns
true_infections <- true_infections |>
  select(-any_of(c("row", "col"))) |>
  pivot_wider(names_from = matrix, values_from = value) |>
  group_by(dates) |>
  mutate(across(everything(), ~ first(na.omit(.)), .names = "{.col}")) |>
  ungroup() |>
  distinct(dates, .keep_all = TRUE) |>
  drop_na() |>
  select(c(dates, cases, serop, beta, report_prob, conf.low, conf.high)) |>
  mutate(dates = as.Date(dates), cases = as.integer(cases))
true_infections$eligfrac <- "true_infections"

# convert matrix values into columns
eligibility_adjusted_cases <- eligibility_adjusted_cases |>
  select(-any_of(c("row", "col"))) |>
  pivot_wider(names_from = matrix, values_from = value) |>
  group_by(dates) |>
  mutate(across(everything(), ~ first(na.omit(.)), .names = "{.col}")) |>
  ungroup() |>
  distinct(dates, .keep_all = TRUE) |>
  drop_na() |>
  select(c(dates, cases, serop, beta, report_prob, conf.low, conf.high)) |>
  mutate(dates = as.Date(dates), cases = as.integer(cases))
eligibility_adjusted_cases$eligfrac <- "elig_adj_cases"


d1 <- reported_cases[c("date","cases")]
d1$type <- "reported"
colnames(d1) <- c("dates","cases","type")
d2 <- eligibility_adjusted_cases[c("dates","cases")]
d2$type <- "elig_adj_cases"
d3 <- true_infections[c("dates","cases")]
d3$type <- "true_infections"
allcases <- rbind(d1,d2,d3)

# data prep ---
allcases <- allcases %>%
  mutate(type = factor(type, levels = c("reported", "elig_adj_cases", "true_infections")))
allcases
# Optional global SD (not used now, but kept if needed)
model_sd <- sd(allcases$cases[allcases$type != "reported"])
geom_bar(data = true_infections, aes(x = dates, y = cases, fill = "model estimated cases"),
         stat = "identity", position = "stack", width = 0.35, alpha = 1, show.legend = TRUE)
combined_case_plot <- ggplot(allcases, aes(x = dates)) +
  geom_rect(aes(xmin=ymd('2022-03-17'), xmax = ymd('2022-05-26'), ymin = 0, ymax = Inf), 
            fill = adjustcolor("#F7E2E2", alpha = 0.03), alpha = 0.05) +
  geom_bar(data = filter(allcases, type == "true_infections"),
           aes(y = cases), fill = "blue", stat = "identity", position = "stack",width = 0.25, alpha = 1) +
  geom_bar(data = filter(allcases, type == "elig_adj_cases"),
           aes(y = cases), fill = "red", stat = "identity",position = "stack", width = 0.25, alpha = 1) +
  geom_bar(data = filter(allcases, type == "reported"),
           aes(y = cases), fill = "darkgreen", stat = "identity",position = "stack", width = 0.25, alpha = 1) +
  geom_ribbon(data = filter(allcases, type == "true_infections"), aes(ymin = pmax(0, cases - 0.25 * sd(cases)),
                                                            ymax = cases + 0.25 * sd(cases)), fill = "grey", alpha = 0.5)+
  geom_ribbon(data = filter(allcases, type == "elig_adj_cases"), aes(ymin = pmax(0, cases - 0.25 * sd(cases)),
                                                                ymax = cases + 0.25 * sd(cases)), fill = "grey", alpha = 0.5)+
  geom_smooth(data = filter(allcases, type == "elig_adj_cases"), aes(y = cases, color = "PCR-Elig. Adjusted Cases"),
              se = FALSE, span = 0.15, linewidth = 1.5) +
  geom_smooth(data = filter(allcases, type == "true_infections"), aes(y = cases, color = "True Infections"),
              se = FALSE, span = 0.15, linewidth = 1.5) +
  geom_line(data = filter(allcases, type == "reported"), aes(y = cases, color = "Reported Cases"),
            se = FALSE, span = 0.1, linewidth = 1.5) +
  scale_color_manual(name = NULL,
                     values = c("Reported Cases" = "darkgreen", "PCR-Elig. Adjusted Cases" = "red", "True Infections" = "blue")) +
  labs(x = "Date (Dec 15, 2021 - May 26, 2022)",y = "Number of Cases",
       title = "Reported vrs Estimated Daily Omicron Infections") +
  geom_segment(aes(x = as.Date("2021-12-24"), y = 0, yend = Inf),linetype = "dashed",color = "gold4",
               linewidth = 1) +
  geom_segment(aes(x = as.Date("2022-01-08"), y = 0, yend = Inf),linetype = "dashed",color = "gold4",
               linewidth = 1) +
  geom_segment(aes(x = as.Date("2022-02-07"), y = 0, yend = Inf),linetype = "dashed",color = "gold4",
               linewidth = 1) +
  geom_segment(aes(x = as.Date("2022-03-14"), y = 0, yend = Inf),linetype = "solid",color = "black",
               linewidth = 1) +
  annotate("text", x = as.Date("2021-12-18"), y = 1000, label = "ALS-2", size = 5, angle = 90, hjust = 1) +
  annotate("text", x = as.Date("2021-12-28"), y = 1500, label = "ALS-3", size = 5, angle = 90, hjust = 1) +
  annotate("text", x = as.Date("2022-01-30"), y = 1200, label = "ALS-4", size = 5, hjust = 1) +
  annotate("text", x = as.Date("2022-03-04"), y = 1200, label = "Mod-ALS-3", size = 5, hjust = 1) +
  annotate("text", x = as.Date("2022-04-28"), y = 1000, label = "No-ALS", size = 5, hjust = 1) +
  theme_clean() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 15, face = "bold"),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.background = element_rect(color = NA),
    plot.background = element_blank()
  )

# inset cumulative totals ---
tot_obs <- allcases %>% filter(type == "reported") %>% summarise(total = sum(cases)) %>% pull(total)
tot_piecewise <- allcases %>% filter(type == "elig_adj_cases") %>% summarise(total = sum(cases)) %>% pull(total)
tot_const <- allcases %>% filter(type == "true_infections") %>% summarise(total = sum(cases)) %>% pull(total)

# data for bar plot
const_final_totals <- data.frame(Group = c("Reported\nCases", "PCR-Elig.\nAdjusted Cases", "True\nInfections"),Total_Cases = c(tot_obs, tot_piecewise, tot_const))

# reorder the groups from smallet to largest
const_final_totals <- const_final_totals %>%
  mutate(Group = fct_reorder(Group, -Total_Cases))  # shortest to tallest


# inset plot
const_cum <- ggplot(const_final_totals, aes(x = Group, y = Total_Cases, fill = Group)) +
  geom_col(width = 0.6, alpha = 0.35, color = "black", show.legend = FALSE) +
  geom_text(aes(label = Total_Cases), vjust = 2.0, size = 2.5) +
  scale_fill_manual(values = c("Reported\nCases" = "darkgreen", "PCR-Elig.\nAdjusted Cases" = "red", "True\nInfections" = "blue")) +
  labs(title = "Total Infections: Reported vs Estimated", x = NULL, y = "Total Cases") +
  theme_clean() +
  theme(
    plot.background = element_rect(fill = "white", color = "black"),
    axis.title.x = element_text(size = 8, face = "bold"),
    axis.title.y = element_text(size = 8, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 6),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 8, hjust = 0.95)
  )

# combine plots ---
final_combined_plot <- ggdraw() +
  draw_plot(combined_case_plot, 0, 0, 1, 1) +
  draw_plot(const_cum, 0.23, 0.66, 0.3, 0.28)


print(final_combined_plot)

png("../figures/allscenarios.png", width = 2600, height = 1800, res = 300, bg = "white", type = "cairo")
final_combined_plot
dev.off()
