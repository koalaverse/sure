################################################################################
# Setup
################################################################################

# Load required packages
library(MASS)      # for fitting ordinal regression models
library(ordinal)   # for fitting ordinal regression models
library(ordr)      # for ordinal regression diagnostics
library(tmvtnorm)  # for simulating from a truncated MV normal dist
library(VGAM)      # for fitting ordinal regression models


################################################################################
# Simulate data
################################################################################

# Function to simulate latent variable Z from a quadratic function of X plus
# noise; the ordinal outcome W is obtained by discretizing Z.
simData <- function(n = 2000, alpha = 16, beta = c(-8, 1),
                    threshold = c(0, 4, 8)) {
  x <- runif(n, min = 1, max = 7)
  z <- alpha + beta[1L] * x + beta[2L] * x ^ 2 + rnorm(n)  # rlnorm(n)
  y <- sapply(z, FUN = function(zz) {
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
set.seed(977)
d <- simData(n = 2000)
table(d$y)
#
#   1    2    3    4
# 275 1047  544  134


################################################################################
# Fit ordinal regression models with probit link
################################################################################

# Fitted models
probit.clm <- clm(formula = y ~ x + I(x ^ 2), data = d, link = "probit")
probit.polr <- polr(formula = y ~ x + I(x ^ 2), data = d, method = "probit")
probit.vglm <- vglm(formula = y ~ x + I(x ^ 2), data = d,
                    family = cumulative(link = probit, parallel = TRUE))

# Q-Q plots
par(mfrow = c(2, 3))
resplot(logit.clm, nsim = 10)
resplot(logit.polr, nsim = 10)
resplot(logit.vglm, nsim = 10)
resplot(probit.clm, nsim = 10)
resplot(probit.polr, nsim = 10)
resplot(probit.vglm, nsim = 10)

# Residual-by-covariate plot
par(mfrow = c(2, 3))
resplot(logit.clm, what = "covariate", x = d$x, nsim = 10)
resplot(logit.polr, what = "covariate", x = d$x, nsim = 10)
resplot(logit.vglm, what = "covariate", x = d$x, nsim = 10)
resplot(probit.clm, what = "covariate", x = d$x, nsim = 10)
resplot(probit.polr, what = "covariate", x = d$x, nsim = 10)
resplot(probit.vglm, what = "covariate", x = d$x, nsim = 10)
