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
  z <- alpha + beta * x + rnorm(n, mean = 0, sd = x ^ 2)
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
table(d$y)
#  1    2    3    4    5
# 48  217 1345  292   98


################################################################################
# Code from the paper
################################################################################

# Code needed for function to run
model <- vglm(y ~ x, data = d,
              family = cumulative(link = probit, parallel = TRUE))
alpha.hat <- -coef(model)[1L]
beta.hat <- -coef(model)[5L]
thrd.hat <- c(0, coef(model)[2L] - coef(model)[1L],
              coef(model)[3L] - coef(model)[1L],
              coef(model)[4L]- coef(model)[1L])

# Residual function used in the paper
residual.bootstrap <- function(y, x) {
  y <- as.integer(y)
  cc <- c(-Inf, thrd.hat, Inf)
  res <- rtmvnorm(1, mean = (alpha.hat + beta.hat * x), sigma = 1,
                  lower = cc[y], upper = cc[y + 1]) - (alpha.hat + beta.hat * x)
}

# Residuals used in Figure 6(a)
res.boot <- rep(NA, n)
for(i in 1:n) {
  res.boot[i] <- residual.bootstrap(d$y[i], d$x[i])
}


################################################################################
# Fit ordinal regression models with probit link
################################################################################

# Fitted models
fit.clm <- clm(formula = y ~ x, data = d, link = "probit")
fit.polr <- polr(formula = y ~ x, data = d, method = "probit")
fit.vglm <- vglm(formula = y ~ x, data = d,
                 family = cumulative(link = probit, parallel = TRUE))


################################################################################
# Create plots
################################################################################

# Compare to Figure 6(a)
pdf("slowtests\\figures\\heteroscedasticity.pdf", width = 6, height = 6)
par(mfrow = c(2, 2))
resplot(res.clm, what = "covariate", x = d$x, main = "ordinal::clm",
        ylab = "Surrogate residual", alpha = 0.1)
resplot(res.polr, what = "covariate", x = d$x, main = "MASS::polr",
        ylab = "Surrogate residual", alpha = 0.1)
resplot(res.vglm, what = "covariate", x = d$x, main = "VGAM::vglm",
        ylab = "Surrogate residual", alpha = 0.1)
plot(d$x, res.boot, main = "Figure 6(a)",
     xlab = "x", ylab = "LS residual")
lines(smooth.spline(d$x, res.boot), lwd = 2, col = "red")
abline(h = c(-2, 2), lty = 2, col = "red")
dev.off()
