library(cowplot)
library(png)
library(grid)

# load saved PNG images
img1 <- readPNG("../figures/transmission_rate.png")
img2 <- readPNG("../figures/R0_errorplot.png")

# convert plots to grobs
g1 <- rasterGrob(img1, interpolate = TRUE)
g2 <- rasterGrob(img2, interpolate = TRUE)

# stack vertically
combined_plot <- plot_grid(
  g1, g2,
  ncol = 1, 
  labels = c("F","G"),label_size = 20,
  align = "v",     
  rel_heights = c(1, 1) 
)

print(combined_plot)
png("../figures/transmission_R0.png", width = 5000, height = 5000, res = 300, bg = "white", type = "cairo")
combined_plot
dev.off()