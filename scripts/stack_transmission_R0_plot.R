library(cowplot)
library(png)
library(grid)

p1 <- ggdraw() + draw_image("../figures/reprod_numb.png")
p2 <- ggdraw() + draw_image("../figures/als_R0.png")

gg <- plot_grid(
   p1, p2,
  ncol = 1,
  rel_heights = c(1, 1, 1),        # one height per panel
  labels = c("A", "B"),
  label_size = 10,
  label_fontface = "plain",
  label_x = 0.1,                   # pull labels closer to left edge
  label_y = 0.99,                   # near top edge
  hjust = 0.5, vjust = 1,
  align = "v"
)

print(gg)
