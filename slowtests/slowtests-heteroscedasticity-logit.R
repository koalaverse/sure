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



# Residual plots ---------------------------------------------------------------

# Compare to Figure 6(a)
p1 <- autoplot(fit.clm, what = "covariate", x = d$x, alpha = 0.5) +
  ggtitle("ordinal::clm")
p2 <- autoplot(fit.polr, what = "covariate", x = d$x, alpha = 0.5) +
  ggtitle("MASS::polr")
p3 <- autoplot(fit.vglm, what = "covariate", x = d$x, alpha = 0.5) +
  ggtitle("VGAM::vglm")
p4 <- autoplot(fit.lrm, what = "covariate", x = d$x, alpha = 0.5) +
  ggtitle("rms::lrm")
p5 <- autoplot(fit.orm, what = "covariate", x = d$x, alpha = 0.5) +
  ggtitle("rms::orm")
grid.arrange(p1, p2, p3, p4, p5, ncol = 2)



autoplot(fit.clm, x = d$x, alpha = 0.5)
autoplot(fit.clm, x = d$x, alpha = 0.5, nsim = 10)
autoplot(fit.clm, x = d$x, alpha = 0.5, method = "jitter")
autoplot(fit.clm, x = d$x, alpha = 0.5, method = "jitter", nsim = 10)
autoplot(fit.clm, x = d$x, alpha = 0.5, method = "jitter",
         jitter.scale = "probability")
autoplot(fit.clm, x = d$x, alpha = 0.5, method = "jitter", nsim = 10,
         jitter.scale = "probability")
