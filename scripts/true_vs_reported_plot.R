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

# load reported data
reported_cases <- csvRead("serop_avgcase_data")
reported_cases$date <- as.Date(reported_cases$date, format = "%Y-%m-%d")

# drop all NAs
reported_cases <- reported_cases |>
  drop_na()

# load the saved estimated true infections
true_infections <- csvRead("true_infections_data")

# rename column 'true_inf' to 'cases' for conformity with the reported cases
true_infections <- true_infections %>%
  rename_at(vars("true_inf"), ~"cases")

d1 <- reported_cases[c("date","cases")]
d1$type <- "reported"
colnames(d1) <- c("dates","cases","type")

d2 <- true_infections[c("dates","cases")]
d2$type <- "true_infections"
allcases <- rbind(d1,d2)

# data prep
allcases <- allcases %>%
  mutate(type = factor(type, levels = c("reported", "true_infections")))

combined_case_plot <- ggplot(allcases, aes(x = dates)) +
  geom_rect(aes(xmin=ymd('2022-03-17'), xmax = ymd('2022-05-26'), ymin = 0, ymax = Inf), fill = adjustcolor("#F7E2E2", alpha = 0.03), alpha = 0.05) +
  geom_bar(data = filter(allcases, type == "true_infections"), aes(y = cases), fill = "blue", stat = "identity", position = "stack",width = 0.25, alpha = 1) +
  geom_bar(data = filter(allcases, type == "reported"), aes(y = cases), fill = "darkgreen", stat = "identity",position = "stack", width = 0.25, alpha = 1) +
  geom_ribbon(data = filter(allcases, type == "true_infections"), aes(ymin = pmax(0, cases - 0.25 * sd(cases)),ymax = cases + 0.25 * sd(cases)), fill = "grey", alpha = 0.5)+
  geom_point(data = filter(allcases, type == "true_infections"), aes(y = cases, color = "True Infections"),size = 1.0) +
  geom_line(data = filter(allcases, type == "true_infections"), aes(y = cases, color = "True Infections"),linewidth = 0.5) +
  geom_point(data = filter(allcases, type == "reported"), aes(y = cases, color = "Reported Cases"),size = 1.0) +
  geom_line(data = filter(allcases, type == "reported"), aes(y = cases, color = "Reported Cases"),linewidth = 0.5) +
  scale_color_manual(name = NULL,values = c("Reported Cases" = "darkgreen", "True Infections" = "blue")) +
  labs(x = "Date (Dec 15, 2021 - May 26, 2022)",y = "Number of Cases",title = "Reported vrs Estimated True Omicron Infections") +
  geom_segment(aes(x = as.Date("2021-12-24"), y = 0, yend = Inf),linetype = "dashed",color = "gold4",linewidth = 1) +
  geom_segment(aes(x = as.Date("2022-01-08"), y = 0, yend = Inf),linetype = "dashed",color = "gold4",linewidth = 1) +
  geom_segment(aes(x = as.Date("2022-02-07"), y = 0, yend = Inf),linetype = "dashed",color = "gold4",linewidth = 1) +
  geom_segment(aes(x = as.Date("2022-03-14"), y = 0, yend = Inf),linetype = "solid",color = "black",linewidth = 1) +
  annotate("text", x = as.Date("2021-12-18"), y = 1000, label = "ALS-2", size = 5, angle = 90, hjust = 1) +
  annotate("text", x = as.Date("2021-12-28"), y = 1500, label = "ALS-3", size = 5, angle = 90, hjust = 1) +
  annotate("text", x = as.Date("2022-01-30"), y = 950, label = "ALS-4", size = 5, hjust = 1) +
  annotate("text", x = as.Date("2022-03-07"), y = 700, label = "Mod-ALS-3", size = 5, hjust = 1) +
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

# cumulative cases for reported and true cases 
tot_obs <- allcases %>% filter(type == "reported") %>% summarise(total = sum(cases)) %>% pull(total)
tot_const <- allcases %>% filter(type == "true_infections") %>% summarise(total = sum(cases)) %>% pull(total)

# data for bar plot
const_final_totals <- data.frame(Group = c("Reported\nCases", "True\nInfections"),Total_Cases = c(tot_obs, tot_const))

# reorder the groups from smallet to largest
const_final_totals <- const_final_totals %>%
  mutate(Group = fct_reorder(Group, -Total_Cases))  

# inset plot
const_cum <- ggplot(const_final_totals, aes(x = Group, y = Total_Cases, fill = Group)) +
  geom_col(width = 0.6, alpha = 0.35, color = "black", show.legend = FALSE) +
  geom_text(aes(label = Total_Cases), vjust = 2.0, size = 3) +
  scale_fill_manual(values = c("Reported\nCases" = "darkgreen", "True\nInfections" = "blue")) +
  labs(title = "Infections: True vs Reported", x = NULL, y = "Total Cases") +
  theme_clean() +
  theme(
    plot.background = element_rect(fill = "white", color = "black"),
    axis.title.x = element_text(size = 9, face = "bold"),
    axis.title.y = element_text(size = 9, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.95)
  )

# combine plots ---
final_combined_plot <- ggdraw() +
  draw_plot(combined_case_plot, 0, 0, 1, 1) +
  draw_plot(const_cum, 0.24, 0.66, 0.33, 0.28)

print(final_combined_plot)

png("../figures/reported_vs_true.png", width = 2600, height = 1800, res = 300, bg = "white", type = "cairo")
final_combined_plot
dev.off()
