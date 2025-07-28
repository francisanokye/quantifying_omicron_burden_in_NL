library(ggplot2)
library(dplyr)
library(patchwork)


###### ALS Timeline ###########

# Timeline axis data
timeline <- data.frame(date = as.Date(c("2021-12-15", "2022-01-01", "2022-02-01", 
                                        "2022-03-01", "2022-04-01", "2022-05-01", "2022-05-22")),
                       label = c("Dec 15", "Jan 1", "Feb 1", "Mar 1", "Apr 1", "May 1", "May 22")
                       )

# event arrows
events <- data.frame(
  date = as.Date(c("2021-12-15", "2022-03-14")),
  label = c("1st Omicron\ncase", "End\nMeasures"),
  y_tip = 0.15,
  y_text = 0.25
)

# ALS segments
als_segments <- data.frame(
  start = as.Date(c("2021-12-15","2021-12-24","2022-01-08", "2022-02-07", "2022-03-14")),
  end   = as.Date(c("2021-12-24","2022-01-08","2022-02-07", "2022-03-14", "2022-05-22")),
  level = c("ALS-2","ALS-3", "ALS-4","Mod-ALS-3", "No-ALS"),
  y     = -0.2
)

# midpoints using Date math
als_segments <- als_segments %>%
  mutate(midpoint = start + (end - start) / 2)

# K-12 school closure
schools_closed <- data.frame(start = as.Date("2021-12-20"),end   = as.Date("2022-01-25"), y = -0.35)

schools_closed$midpoint <- schools_closed$start + (schools_closed$end - schools_closed$start)/2

# ALS colors
als_colors <- c(
  "ALS-2" = "#66D1B566",
  "ALS-3" = "#87CEFA66",
  "ALS-4" = "#FFD58066",
  "Mod-ALS-3" = "#F7E2E299",
  "No-ALS" = "#D3D3D399"
)

# Plot
als_timeline_plot <- (ggplot() +
  # timeline axis
  geom_segment(aes(x=min(timeline$date), xend=max(timeline$date),y=0, yend=0), size=1) +
  geom_segment(data=timeline,aes(x=date, xend=date, y=0.05, yend=-0.05)) +
  geom_text(data=timeline,aes(x=date, y=-0.1, label=label), size=3) +
  # event arrows
  geom_segment(data=events,aes(x=date, xend=date, y=0, yend=y_tip),arrow=arrow(length=unit(0.1,"cm")), size=0.5) +
  geom_text(data=events,aes(x=date, y=y_text, label=label),size=3, hjust=0.5) +
  # dotted lines for ALS start and end
  geom_segment(data=als_segments,aes(x=start, xend=start, y=0, yend=y),linetype="dashed", color="black") +
  geom_segment(data=als_segments,aes(x=end, xend=end, y=0, yend=y),linetype="dashed", color="black") +
  # dotted lines for K-12 start and end
  geom_segment(data=schools_closed,aes(x=start, xend=start, y=0, yend=y),linetype="dashed", color="red") +
  geom_segment(data=schools_closed,aes(x=end, xend=end, y=0, yend=y),linetype="dashed", color="red") +
  # ALS bars
  geom_rect(data=als_segments,aes(xmin=start, xmax=end,ymin=y-0.05, ymax=y+0.05,fill=level),color="black") +
  scale_fill_manual(values=als_colors) +
  geom_text(data=als_segments,aes(x=midpoint, y=y, label=level),size=3, color="black") +
  # K-12 schools bar
  geom_rect(data=schools_closed,aes(xmin=start, xmax=end,ymin=y-0.025, ymax=y+0.025),fill="white", color="black", alpha=0.5) +
  geom_text(data=schools_closed,aes(x=midpoint, y=y, label="K-12 schools closed"),size=3, color="blue") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"),plot.margin = margin(t = 5, r = 5, b = 5, l = 5))+
  coord_cartesian(ylim = c(-0.5, 0.6)) +
  theme(legend.position="none")
)

print(als_timeline_plot)

######################################
###### Testing Eligibility###########

# Timeline axis data
testing_timeline <- data.frame(date = as.Date(c("2021-12-15", "2022-01-01", "2022-02-01", 
                                                "2022-03-01", "2022-04-01", "2022-05-01", "2022-05-22")),
                               label = c("Dec 15", "Jan 1", "Feb 1", "Mar 1", "Apr 1", "May 1", "May 22")
)

# Testing segments
testing_segments <- data.frame(
  start = as.Date(c("2021-12-15","2022-01-03","2022-01-24", "2022-02-25", "2022-03-17")),
  end   = as.Date(c("2022-01-03","2022-01-24","2022-02-25", "2022-03-17", "2022-05-22")),
  level = c("T1 = People with one\nsymptom people\n notified of exposure",
            "T2 = Asymptomatic close\ncontacts + symptomatic\npeople who were\nnot close contacts", 
            "T3 = Asymptomatic vaccinated\nnon-household contacts\n + symptomatic people",
            "T4 = Symptomatic \ncontacts+ asymptomatic\nhouseholdcontacts", 
            "T5: high risk (60+ yrs)\n+ healthcare workers"),
  y     = -0.2
)

# midpoints using Date math
testing_segments <- testing_segments %>%
  mutate(midpoint = start + (end - start) / 2)

testing_colors <- c(
  "T1 = People with one\nsymptom people\n notified of exposure" = "#A8DADC66",
  "T2 = Asymptomatic close\ncontacts + symptomatic\npeople who were\nnot close contacts" = "#B5E48C66",
  "T3 = Asymptomatic vaccinated\nnon-household contacts\n + symptomatic people" = "#FFE29A66",
  "T4 = Symptomatic \ncontacts+ asymptomatic\nhouseholdcontacts" = "#CDB4DB66",
  "T5: high risk (60+ yrs)\n+ healthcare workers" = "#FBC4AB66"
)

# event arrows
events <- data.frame(
  date = as.Date(c("2021-12-15","2022-01-03","2022-01-24", "2022-02-25", "2022-03-17")),
  label = c("Dec 15\n2021","Jan 1\n2022","Jan 24\n2022", "Feb 25\n2022", "March 17\n2022"),
  y_tip = 0.15,
  y_text = 0.25
)

testing_timeline_plot <- (ggplot() +
                            # timeline axis
                            geom_segment(aes(x=min(testing_timeline$date), xend=max(testing_timeline$date),y=0, yend=0), size=1) +
                            geom_segment(data=testing_timeline,aes(x=date, xend=date, y=0.05, yend=-0.05)) +
                            geom_text(data=testing_timeline,aes(x=date, y=-0.1, label=label), size=4) +
                            # event arrows
                            geom_segment(data=events,aes(x=date, xend=date, y=0, yend=y_tip),arrow=arrow(length=unit(0.1,"cm")), size=0.5) +
                            geom_text(data=events,aes(x=date, y=y_text, label=label),size=4, hjust=0.5) +
                            # dotted lines for testing eligibility start and end
                            geom_segment(data=testing_segments,aes(x=start, xend=start, y=0, yend=y),linetype="dashed", color="black") +
                            geom_segment(data=testing_segments,aes(x=end, xend=end, y=0, yend=y),linetype="dashed", color="black") +
                            # ALS bars
                            geom_rect(data = testing_segments,aes(xmin=start, xmax=end,ymin=y-0.15, ymax=y+0.15,fill=level),color="black") +
                            scale_fill_manual(values = testing_colors) +
                            geom_text(data=testing_segments,aes(x=midpoint, y=y, label=level),size=4, color="black") +
                            theme_void() +
                            theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"),plot.margin = margin(t = 5, r = 5, b = 5, l = 5))+
                            coord_cartesian(ylim = c(-0.5, 0.6)) +
                            theme(legend.position="none"))

print(testing_timeline_plot)



png("../figures/als_timeline_plot.png", width = 3000, height = 1800, res = 300, bg = "white", type = "cairo")
als_timeline_plot
dev.off()

png("../figures/testing_timeline_plot.png", width = 3000, height = 1800, res = 300, bg = "white", type = "cairo")
testing_timeline_plot
dev.off()
