# Load required packages
library(ggimage)
library(ggplot2)
library(MASS)
library(sure)

# Data for plot
data(df1, package = "sure")
fit <- polr(formula = y ~ x, data = df1, method = "probit")
set.seed(101)
res <- data.frame(x = df1$x, y = sure::resids(fit))
p <- autoplot(fit, what = "covariate", x = df1$x, alpha = 0.5) +
  theme_light() +
  theme(axis.line = element_blank(),axis.text.x=element_blank(),
        axis.text.y = element_blank(),axis.ticks=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
# print(p)

# Hexagon data
hex <- data.frame(x = 1.35 * 1 * c(-sqrt(3) / 2, 0, rep(sqrt(3) / 2, 2), 0,
                                   rep(-sqrt(3) / 2, 2)),
                  y = 1.35 * 1 * c(0.5, 1, 0.5, -0.5, -1, -0.5, 0.5))

# Color palettes
greys <- RColorBrewer::brewer.pal(9, "Greys")

# Hexagon logo
g <- ggplot() +
  geom_polygon(data = hex, aes(x, y), color = greys[7L], fill = "white",
               size = 3) +
  geom_subview(subview = p, x = 0, y = 0, width = 1.75, height = 1.25) +
  annotate(geom = "text", label = "sure", x = 0, y = -0.8,
           # family = "Open Sans Light",
           color = "black", size = 7) +
  annotate(geom = "text", label = "SUrrogate REsiduals", x = 0, y = 0.7,
           # family = "Open Sans Light",
           color = "black", size = 3.5) +
  coord_equal(xlim = range(hex$x), ylim = range(hex$y)) +
  scale_x_continuous(expand = c(0.04, 0)) +
  scale_y_reverse(expand = c(0.04, 0)) +
  theme_void() +
  theme_transparent() +
  theme(axis.ticks.length = unit(0, "mm"))
print(g)

png("man/figures/sure-logo.png", width = 181, height = 209, bg = "transparent",
    type = "quartz")
print(g)
dev.off()

svg("tools/sure-logo.svg", width = 181 / 72, height = 209 / 72,
    bg = "transparent")
print(g)
dev.off()
