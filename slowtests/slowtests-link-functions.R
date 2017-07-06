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
  y <-   sapply(z, FUN = function(zz) {
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
fit.clm <- clm(formula = y ~ x, data = d, link = "logit")
fit.polr <- polr(formula = y ~ x, data = d, method = "logistic")
fit.vglm <- vglm(formula = y ~ x, data = d,
                 family = cumulative(link = logit, parallel = TRUE))

# Residuals
res.clm <- resids(fit.clm, nsim = 50)
res.polr <- resids(fit.polr, nsim = 50)
res.vglm <- resids(fit.vglm, nsim = 50)

# Compare to Figure 6
par(mfrow = c(2, 2))
resplot(res.clm, what = "covariate", x = d$x, main = "ordinal::clm",
        ylab = "Surrogate residual", alpha = 0.1)
resplot(res.polr, what = "covariate", x = d$x, main = "MASS::polr",
        ylab = "Surrogate residual", alpha = 0.1)
resplot(res.vglm, what = "covariate", x = d$x, main = "VGAM::vglm",
        ylab = "Surrogate residual", alpha = 0.1)
plot(d$x, ordr:::getLSResiduals(fit.vglm), main = "VGAM::vglm",
     xlab = "x", ylab = "LS residual")
abline(h = 0, lwd = 2, col = "red")


fit2 <- polr(y ~ x, data = d)
res.fit2 <- resids(fit2)
resplot(res.fit2)
