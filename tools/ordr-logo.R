#' @author Johannes Rainer with modification from Guangchuang Yu and Sebastian
#' Gibb
whiteTrans <- function(alpha = 0.4) {
  function(n) {
    rgb(red = rep(1, n), green = rep(1, n), blue = rep(1, n),
        alpha = seq(0, alpha, length.out = n))
  }
}



#' @author Johannes Rainer with modification from Guangchuang Yu and further
#' modifications by Brandon Greenwell
spotlight <- function(alpha, n = 500000, mean = 0, sd = 2) {
  x <- rnorm(n = n, mean = mean, sd = sd)
  y <- rnorm(n = n, mean = mean, sd = sd)
  hexbinplot(x ~ y, colramp = whiteTrans(alpha), colorkey = FALSE,
             bty = "n", scales = list(draw = FALSE), xlab = "", ylab = "",
             border = NA, par.settings = list(axis.line = list(col = NA)))
}


#' @author Guangchuang Yu
loadFont <- function(family) {
  if (family == "Aller") {
    family <- "Aller_Rg"
  }
  fonts <- list.files(system.file("fonts", package = "hexSticker"),
                      pattern = "ttf$", recursive = TRUE, full.names = TRUE)
  i <- family == sub(".ttf", "", basename(fonts))
  if (any(i)) {
    font.add(family, fonts[which(i)[1]])
    showtext.auto()
  }
  family
}


#' @author Guangchuang Yu
theme_sticker <- function(...) {
  center <- 1
  radius <- 1
  h <- radius
  w <- sqrt(3) / 2 * radius
  m <- 1.02
  list(theme_transparent() +
         theme(plot.margin = grid::unit(c(0, 0, -0.2, -0.2), "lines"),
               strip.text = element_blank(),
               line = element_blank(),
               text = element_blank(),
               title = element_blank(), ...),
       coord_fixed(),
       scale_y_continuous(expand = c(0, 0),
                          limits = c(center - h * m, center + h * m)),
       scale_x_continuous(expand = c(0, 0),
                          limits = c(center - w * m, center + w * m))
  )
}


# Open new sticker device; written by guangchuang yu
sticker_dev <- function() {
  dev.new(width = sqrt(3), height = 2, noRStudioGD = TRUE)
}


# Load required packages
library(ggplot2)
library(ggimage)
library(grDevices)
library(hexbin)  # only required if using spotlight function
library(hexSticker)  # for fonts
library(showtext)
library(sysfonts)

# Read and process imgae
img <- png::readPNG("tools\\gavel.png")
g <- grid::rasterGrob(img, interpolate = TRUE,
                      width = unit(0.74, "npc"),
                      height = unit(0.6, "npc"))

# Font family
font.family <- loadFont("Aller")

# Construct sticker
sticker <- ggplot() +
  geom_hexagon(size = 1.5, fill = "white", color = "black") +
  hexagon(size = 1.5, fill = "white", color = "black") +
  annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_pkgname("ordr", x = 0.65, y = 0.6, color = "black", size = 25)

# Add package name
pkg <- data.frame(x = 0.65, y = 0.6, label = "ordr")
sticker <- sticker +
  geom_text(aes(x, y, label = label), data = pkg,  size = 25, color = "black",
            family = font.family)

# Add sticker theme
sticker <- sticker + theme_sticker()

# View sticker
sticker

# Save sticker
ggsave(sticker, width = 43.9, height = 50.8,
       filename = "tools\\ordr-logo.png",
       bg = "transparent",
       units = "mm")
