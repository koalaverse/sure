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
simData <- function(n = 2000, alpha = 36, beta = 4,
                    threshold = c(0, 30, 70, 100)) {
  x <- runif(n, min = 2, max = 7)
  # z <- alpha + beta * x + rnorm(n, mean = 0, sd = x ^ 2)
  z <- alpha + beta * x + rlogis(n, location = 0, scale = x ^ 2)
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
set.seed(108)
d <- simData(n = 2000)


################################################################################
# Fit ordinal regression models with probit link
################################################################################

# Fitted models
good.clm <- clm(formula = y ~ x, data = d, link = "logit")
good.polr <- polr(formula = y ~ x, data = d, method = "logistic")
good.vglm <- vglm(formula = y ~ x, data = d,
                 family = cumulative(link = logit, parallel = TRUE))
bad.clm <- clm(formula = y ~ x, data = d, link = "probit")
bad.polr <- polr(formula = y ~ x, data = d, method = "probit")
bad.vglm <- vglm(formula = y ~ x, data = d,
                 family = cumulative(link = probit, parallel = TRUE))

# Q-Q plots
par(mfrow = c(2, 3))
resplot(good.clm, nsim = 10)
resplot(good.polr, nsim = 10)
resplot(good.vglm, nsim = 10)
resplot(bad.clm, nsim = 10)
resplot(bad.polr, nsim = 10)
resplot(bad.vglm, nsim = 10)
