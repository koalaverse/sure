################################################################################
# Setup
################################################################################

# Load packages for fitting cumulative link models
library(MASS)     # function polr()
library(ordinal)  # function clm()
library(rms)      # functions lrm() and orm()
library(VGAM)     # function vglm()

# Load packages required to run Dungang's original code
library(tmvtnorm)  # for simulating from a truncated MV normal dist

# Load our package
library(sure)      # for surrogate-based residuals
library(ggplot2)   # for plotting


################################################################################
# Simulate data
################################################################################

# Function to simulate latent variable Z from a quadratic function of X plus
# noise; the ordinal outcome W is obtained by discretizing Z.
simData <- function(n = 2000, alpha = 36, beta = 4,
                    threshold = c(0, 30, 70, 100)) {
  x <- runif(n, min = 2, max = 7)
  y <-   sapply(alpha + beta * x + rlogis(n, scale = x ^ 2), FUN = function(zz) {
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

# Simulate data
set.seed(108)
d <- simData(n = 2000)
table(d$y)
#
#   1    2    3    4    5
# 156  266 1039  292  247


################################################################################
# Fit ordinal regression models with probit link
################################################################################

# Fitted models
fit.clm <- clm(y ~ x, data = d, link = "logit")
fit.polr <- polr(y ~ x, data = d, method = "logistic")
fit.vglm <- vglm(y ~ x, data = d,
                 family = cumulative(link = logit, parallel = TRUE))
fit.lrm <- lrm(y ~ x, data = d)
fit.orm <- orm(y ~ x, data = d, family = logistic)


################################################################################
# Create plots
################################################################################

# Compare to Figure 6(a)
p1 <- autoplot(fit.clm, what = "covariate", x = d$x) + ggtitle("ordinal::clm")
p2 <- autoplot(fit.polr, what = "covariate", x = d$x) + ggtitle("MASS::polr")
p3 <- autoplot(fit.vglm, what = "covariate", x = d$x) + ggtitle("VGAM::vglm")
p4 <- autoplot(fit.lrm, what = "covariate", x = d$x) + ggtitle("rms::lrm")
p5 <- autoplot(fit.orm, what = "covariate", x = d$x) + ggtitle("rms::orm")
p6 <- ggplot(data.frame(x = d$x, y = res.boot), aes(x, y)) +
  geom_point(size = 2, color = "#444444") +
  geom_smooth(color = "red", se = FALSE) +
  ylab("Surrogate residual") +
  ggtitle("Figure 6(a)")
# pdf("slowtests\\figures\\heteroscedasticity.pdf", width = 7, height = 6)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
# dev.off()
