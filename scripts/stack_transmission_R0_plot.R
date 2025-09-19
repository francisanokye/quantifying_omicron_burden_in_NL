library(cowplot)
library(png)
library(grid)

p1 <- ggdraw() + draw_image("../figures/reprod_numb.png")
p2 <- ggdraw() + draw_image("../figures/als_R0.png")

gg <- plot_grid(
   p1, p2,
  ncol = 1,
  rel_heights = c(1, 1),        # one height per panel
  rel_widths = c(1, 1),
  labels = c("A", "B"),
  label_size = 18,
  label_fontface = "plain",
  label_x = 0.16,                   # pull labels closer to left edge
  label_y = 0.99,                   # near top edge
  hjust = 0.001, vjust = 1.2,
  align = "v"
)

print(gg)

# png("../figures/Figure_6.png", width = 24, height = 16,units = "in", res = 300, bg = "white", type = "cairo")
# gg
# dev.off()
