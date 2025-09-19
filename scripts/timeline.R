# load libraries
library(ggplot2)
library(dplyr)

# shared axis
axis_dates <- data.frame(
  date  = as.Date(c("2021-12-15","2022-01-01","2022-02-01","2022-03-01","2022-04-01","2022-05-01","2022-05-22")),
  label = c("Dec 15","Jan 1","Feb 1","Mar 1","Apr 1","May 1","May 22")
)

# als timeline
als_segments <- data.frame(
  start = as.Date(c("2021-12-15","2021-12-24","2022-01-08","2022-02-07","2022-03-14")),
  end   = as.Date(c("2021-12-24","2022-01-08","2022-02-07","2022-03-14","2022-05-22")),
  level = c("ALS-2","ALS-3","ALS-4","ALS-3","No-ALS")
) %>%
  mutate(midpoint = start + (end - start)/2)

als_colors <- c(
  "ALS-2" = "#66D1B566",
  "ALS-3" = "#87CEFA66",
  "ALS-4" = "#FFD58066",
  "No-ALS" = "#D3D3D399"
)

# notable events (arrows go upward; nothing crosses the als band)
events_als <- data.frame(
  date  = as.Date(c("2021-12-15","2022-03-14")),
  label = c("1st Omicron\ncase","End\nPHE"),
  y_tip = 0.15,  # arrow tip height
  y_txt = 0.20   # text height
)

# k–12 closure bracket (between als and testing) -----
schools_closed <- data.frame(
  start = as.Date("2021-12-20"),
  end   = as.Date("2022-01-25")
) %>%
  mutate(midpoint = start + (end - start)/2)

# testing timeline
testing_segments <- data.frame(
  start = as.Date(c("2021-12-15","2022-01-03","2022-01-24","2022-02-25","2022-03-17")),
  end   = as.Date(c("2022-01-03","2022-01-24","2022-02-25","2022-03-17","2022-05-22")),
  label = c("T1","T2","T3","T4","T5")
) %>%
  mutate(midpoint = start + (end - start)/2)

# layout y positions (axis, als, k–12, testing) -----
y_axis <- 0.00
y_als  <- -0.06; h_als  <- 0.06
y_k12  <- -0.21; h_k12  <- 0.035
y_test <- -0.35; h_test <- 0.05

# dashed connectors from testing to als bottom (no crossing inside als)
connector_dates <- data.frame(date = sort(unique(c(testing_segments$start, testing_segments$end))))

# red dashed verticals split at als bottom and end at k–12 top
k12_verticals <- data.frame(
  x  = c(schools_closed$start, schools_closed$end),
  y0 = y_axis,           # from axis
  y1 = y_als - h_als,    # to als bottom
  y2 = y_k12 + h_k12     # then to top of k–12 box
)

# plot
p <- ggplot() +
  # main axis line and ticks
  geom_segment(aes(x = min(axis_dates$date), xend = max(axis_dates$date), y = y_axis, yend = y_axis), linewidth = 1) +
  geom_segment(data = axis_dates, aes(x = date, xend = date, y = y_axis + 0.04, yend = y_axis - 0.04)) +
  geom_text(data = axis_dates, aes(x = date, y = y_axis - 0.09, label = label), size = 3.5) +
  # als events (arrows up from axis)
  geom_segment(data = events_als,
               aes(x = date, xend = date, y = y_axis, yend = y_tip),
               arrow = arrow(length = grid::unit(0.09, "cm")), linewidth = 0.5) +
  geom_text(data = events_als, aes(x = date, y = y_txt, label = label), size = 4) +
  
  # als band with shading
  geom_rect(data = als_segments,
            aes(xmin = start, xmax = end, ymin = y_als - h_als, ymax = y_als + h_als, fill = level),
            color = "black") +
  geom_text(data = als_segments, aes(x = midpoint, y = y_als, label = level), size = 5, face = "bold") +
  
  # gray connectors from testing to als bottom
  geom_segment(data = connector_dates,
               aes(x = date, xend = date, y = y_test + h_test, yend = y_als - h_als),
               linetype = "dashed", color = "grey40", size = 1.0) +
  
  # red dashed lines from axis -> als bottom
  geom_segment(data = k12_verticals,
               aes(x = x, xend = x, y = y0, yend = y1),
               linetype = "dashed", color = "red", size = 1) +
  # continue red dashed lines from als bottom -> top of k–12 box
  geom_segment(data = k12_verticals,
               aes(x = x, xend = x, y = y1, yend = y2),
               linetype = "dashed", color = "red", size = 1) +
  
  # k–12 closed box centered between bands + centered label
  geom_rect(data = schools_closed,
            aes(xmin = start, xmax = end, ymin = y_k12 - h_k12, ymax = y_k12 + h_k12),
            fill = "white", color = "black") +
  geom_text(data = schools_closed,
            aes(x = midpoint, y = y_k12, label = "K-12 schools closed"),
            size = 4, color = "blue", vjust = -0.25, hjust = 0.5) +
  
  # testing band (outline only), with labels
  geom_rect(data = testing_segments,
            aes(xmin = start, xmax = end, ymin = y_test - h_test, ymax = y_test + h_test),
            fill = NA, color = "black") +
  geom_text(data = testing_segments, aes(x = midpoint, y = y_test, label = label), size = 4) +
  
  # colors and theme
  scale_fill_manual(values = als_colors) +
  coord_cartesian(ylim = c(-0.62, 0.44)) +
  theme_void() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = -90)),
    plot.margin = margin(2, 6, 6, 6),
    legend.position = "none")+
  labs(title = "Timeline for Alert levels, K–12 school closure and testing eligibility changes")

print(p)
