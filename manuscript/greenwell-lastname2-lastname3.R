################################################################################
# Setup
################################################################################

# Load required packages
library(ggplot2)
library(MASS)
library(ordinal)
library(rms)
library(VGAM)
library(ordr)


################################################################################
# Detecting heteroscedasticty
################################################################################

# Function to simulate heteroscedastic data
simHeteroscedasticData <- function(n = 2000) {
  threshold <- c(0, 30, 70, 100)
  x <- runif(n, min = 2, max = 7)
  y <-   sapply(36 + 4 * x + rnorm(n, sd = x ^ 2), FUN = function(zz) {
    ordinal.value <- 1
    index <- 1
    while(index <= length(threshold) && zz > threshold[index]) {
      ordinal.value <- ordinal.value + 1
      index <- index + 1
    }
    ordinal.value
  })
  data.frame("y" = as.ordered(y), "x" = x)
}

# Simulate heteroscedastic data
set.seed(108)
hd <- simHeteroscedasticData(n = 2000)
table(hd$y)

# Fit a probit model
fit.polr <- polr(y ~ x, data = hd, method = "probit")
set.seed(101)  # for reproducibility
sur.res <- resids(fit.polr)  # surrogate-based residuals

# Compute Li-Shepherd/probability scale residuals
ls.res <- PResiduals::presid(fit.polr)

# Residual vs covariate plots
p1 <- ggplot(data.frame(x = hd$x, y = sur.res), aes(x, y)) +
  geom_point(size = 2, alpha = 0.25) +
  geom_smooth(color = "red", se = FALSE) +
  ylab("Surrogate residual")
p2 <-   ggplot(data.frame(x = hd$x, y = ls.res), aes(x, y)) +
  geom_point(size = 2, alpha = 0.25) +
  geom_smooth(col = "red", se = FALSE) +
  ylab("Probability scale residual")

# Figure ?
pdf(file = "manuscript\\heteroscedasticity.pdf", width = 8, height = 4)
grid.arrange(p1, p2, ncol = 2)
dev.off()
