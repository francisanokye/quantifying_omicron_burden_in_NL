library(cowplot)
library(png)
library(grid)

p1 <- ggdraw() + draw_image("../figures/R1_reprod_numb.png")
p2 <- ggdraw() + draw_image("../figures/R1_als_R0.png")

gg <- plot_grid(
  p1, p2,
  ncol = 1,
  rel_heights = c(1, 1),        # one height per panel
  rel_widths = c(1, 1),
  align = "v"
)

print(gg)

png("../figures/R1_Figure_6.png", width = 20, height = 16,units = "in", res = 300, bg = "white", type = "cairo")
gg
dev.off()
