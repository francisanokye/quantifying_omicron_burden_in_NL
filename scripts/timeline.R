# load libraries
library(ggplot2)
library(dplyr)
library(patchwork)

###### als timeline ###########

# timeline axis data
timeline <- data.frame(
  date = as.Date(c("2021-12-15", "2022-01-01", "2022-02-01", 
                   "2022-03-01", "2022-04-01", "2022-05-01", "2022-05-22")),
  label = c("Dec 15", "Jan 1", "Feb 1", "Mar 1", "Apr 1", "May 1", "May 22")
)

# event arrows
events <- data.frame(
  date = as.Date(c("2021-12-15", "2022-03-14")),
  label = c("1st Omicron\ncase", "End\nPHE"),
  y_tip = 0.15,
  y_text = 0.25
)

# als segments
als_segments <- data.frame(
  start = as.Date(c("2021-12-15", "2021-12-24", "2022-01-08", "2022-02-07", "2022-03-14")),
  end   = as.Date(c("2021-12-24", "2022-01-08", "2022-02-07", "2022-03-14", "2022-05-22")),
  level = c("ALS-2", "ALS-3", "ALS-4", "ALS-3", "No-ALS"),
  y     = -0.2
)

# compute midpoints
als_segments <- als_segments %>%
  mutate(midpoint = start + (end - start) / 2)

# k-12 school closures
schools_closed <- data.frame(
  start = as.Date("2021-12-20"),
  end = as.Date("2022-01-25"),
  y = -0.35
)
schools_closed$midpoint <- schools_closed$start + (schools_closed$end - schools_closed$start) / 2

# als colors
als_colors <- c(
  "ALS-2" = "#66D1B566",
  "ALS-3" = "#87CEFA66",
  "ALS-4" = "#FFD58066",
  "ALS-3" = "#87CEFA66",
  "No-ALS" = "#D3D3D399"
)

# plot als timeline
als_timeline_plot <- ggplot() +
  geom_segment(aes(x = min(timeline$date), xend = max(timeline$date), y = 0, yend = 0), size = 1) +
  geom_segment(data = timeline, aes(x = date, xend = date, y = 0.05, yend = -0.05)) +
  geom_text(data = timeline, aes(x = date, y = -0.1, label = label), size = 3) +
  geom_segment(data = events, aes(x = date, xend = date, y = 0, yend = y_tip),
               arrow = arrow(length = unit(0.1, "cm")), size = 0.5) +
  geom_text(data = events, aes(x = date, y = y_text, label = label), size = 3, hjust = 0.5) +
  geom_segment(data = als_segments, aes(x = start, xend = start, y = 0, yend = y), linetype = "dashed") +
  geom_segment(data = als_segments, aes(x = end, xend = end, y = 0, yend = y), linetype = "dashed") +
  geom_segment(data = schools_closed, aes(x = start, xend = start, y = 0, yend = y), linetype = "dashed", color = "red") +
  geom_segment(data = schools_closed, aes(x = end, xend = end, y = 0, yend = y), linetype = "dashed", color = "red") +
  geom_rect(data = als_segments, aes(xmin = start, xmax = end, ymin = y - 0.05, ymax = y + 0.05, fill = level), color = "black") +
  scale_fill_manual(values = als_colors) +
  geom_text(data = als_segments, aes(x = midpoint, y = y, label = level), size = 3, color = "black") +
  geom_rect(data = schools_closed, aes(xmin = start, xmax = end, ymin = y - 0.025, ymax = y + 0.025),
            fill = "white", color = "black", alpha = 0.5) +
  geom_text(data = schools_closed, aes(x = midpoint, y = y, label = "K-12 schools closed"), size = 3, color = "blue") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.margin = margin(5, 5, 5, 5),
        legend.position = "none") +
  coord_cartesian(ylim = c(-0.5, 0.6))

print(als_timeline_plot)

######################################
###### testing eligibility ###########

# timeline axis data
testing_timeline <- data.frame(
  date = as.Date(c("2021-12-15", "2022-01-01", "2022-02-01", 
                   "2022-03-01", "2022-04-01", "2022-05-01", "2022-05-22")),
  label = c("Dec 15", "Jan 1", "Feb 1", "Mar 1", "Apr 1", "May 1", "May 22")
)

# testing segments
testing_segments <- data.frame(
  start = as.Date(c("2021-12-15", "2022-01-03", "2022-01-24", "2022-02-25", "2022-03-17")),
  end   = as.Date(c("2022-01-03", "2022-01-24", "2022-02-25", "2022-03-17", "2022-05-22")),
  level = c(
    "T1 = People with one\nsymptom people\n notified of exposure",
    "T2 = Asymptomatic close\ncontacts + symptomatic\npeople who were\nnot close contacts", 
    "T3 = Asymptomatic vaccinated\nnon-household contacts\n + symptomatic people",
    "T4 = Symptomatic \ncontacts+ asymptomatic\nhouseholdcontacts", 
    "T5: high risk (60+ yrs)\n+ healthcare workers"
  ),
  y = -0.2
)

# compute midpoints
testing_segments <- testing_segments %>%
  mutate(midpoint = start + (end - start) / 2)

# testing colors
testing_colors <- c(
  "T1 = People with one\nsymptom people\n notified of exposure" = "#A8DADC66",
  "T2 = Asymptomatic close\ncontacts + symptomatic\npeople who were\nnot close contacts" = "#B5E48C66",
  "T3 = Asymptomatic vaccinated\nnon-household contacts\n + symptomatic people" = "#FFE29A66",
  "T4 = Symptomatic \ncontacts+ asymptomatic\nhouseholdcontacts" = "#CDB4DB66",
  "T5: high risk (60+ yrs)\n+ healthcare workers" = "#FBC4AB66"
)

# event arrows
events <- data.frame(
  date = as.Date(c("2021-12-15", "2022-01-03", "2022-01-24", "2022-02-25", "2022-03-17")),
  label = c("Dec 15\n2021", "Jan 1\n2022", "Jan 24\n2022", "Feb 25\n2022", "March 17\n2022"),
  y_tip = 0.15,
  y_text = 0.25
)

# plot testing eligibility timeline
testing_timeline_plot <- ggplot() +
  geom_segment(aes(x = min(testing_timeline$date), xend = max(testing_timeline$date), y = 0, yend = 0), size = 1) +
  geom_segment(data = testing_timeline, aes(x = date, xend = date, y = 0.05, yend = -0.05)) +
  geom_text(data = testing_timeline, aes(x = date, y = -0.1, label = label), size = 4) +
  geom_segment(data = events, aes(x = date, xend = date, y = 0, yend = y_tip),
               arrow = arrow(length = unit(0.1, "cm")), size = 0.5) +
  geom_text(data = events, aes(x = date, y = y_text, label = label), size = 4, hjust = 0.5) +
  geom_segment(data = testing_segments, aes(x = start, xend = start, y = 0, yend = y), linetype = "dashed") +
  geom_segment(data = testing_segments, aes(x = end, xend = end, y = 0, yend = y), linetype = "dashed") +
  geom_rect(data = testing_segments, aes(xmin = start, xmax = end, ymin = y - 0.15, ymax = y + 0.15, fill = level), color = "black") +
  scale_fill_manual(values = testing_colors) +
  geom_text(data = testing_segments, aes(x = midpoint, y = y, label = level), size = 4, color = "black") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.margin = margin(5, 5, 5, 5),
        legend.position = "none") +
  coord_cartesian(ylim = c(-0.5, 0.6))

print(testing_timeline_plot)
