library(cowplot)
library(png)
library(grid)

# build panels that fill their space (no odd margins)
p1 <- ggdraw() + draw_image("../figures/transmission_rate.png")
p2 <- ggdraw() + draw_image("../figures/reprod_numb.png")
p3 <- ggdraw() + draw_image("../figures/als_R0.png")

combined_plot <- plot_grid(
  p1, p2, p3,
  ncol = 1,
  rel_heights = c(1, 1, 1),        # one height per panel
  labels = c("A", "B", "C"),
  label_size = 10,
  label_fontface = "plain",
  label_x = 0.01,                   # pull labels closer to left edge
  label_y = 0.99,                   # near top edge
  hjust = 0.5, vjust = 1,
  align = "v"
)

png("../figures/Figure_4.png", width = 3000, height = 4800, res = 300, bg = "white", type = "cairo")
combined_plot
dev.off()