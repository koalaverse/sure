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
# Model diagnosis when the model is specified correctly
################################################################################

# Function to generate data for Figure 1(a)-(b)
figure1 <- function(model, data) {
  alpha.hat <- -coef(model)[1L]
  beta_1.hat <- -coef(model)[4L]
  beta_2.hat <- -coef(model)[5L]
  thrd.hat <- c(0, coef(model)[2L] - coef(model)[1L],
                coef(model)[3L] - coef(model)[1L])
  x <- data$x
  y <- as.integer(data$y)
  residual.bootstrap <- function(w, x){
    cc <- c(-Inf, thrd.hat, Inf)
    res <- rtmvnorm(
      n = 1,
      mean = (alpha.hat + beta_1.hat * x + beta_2.hat * (x ^ 2)),
      sigma = 1,
      lower = cc[w],
      upper = cc[w + 1]
    ) - (alpha.hat + beta_1.hat * x + beta_2.hat * (x ^ 2))
  }
  res.boot <- rep(NA, n)
  for(i in 1:n) {
    res.boot[i] <- residual.bootstrap(y[i], x[i])
  }
  data.frame(x = x, res = res.boot)
}


# Function to generate data for Figure 3(a)
figure3 <- function(model, data) {
  alpha.hat <- -coef(model)[1L]
  beta.hat <- -coef(model)[4L]
  thrd.hat <- c(0, coef(model)[2L] - coef(model)[1L],
                coef(model)[3L] - coef(model)[1L])
  x <- data$x
  y <- as.integer(data$y)
  residual.bootstrap <- function(w, x){
    cc <- c(-Inf, thrd.hat, Inf)
    res <- rtmvnorm(
      n = 1,
      mean = alpha.hat + beta.hat * x,
      sigma = 1,
      lower = cc[w],
      upper = cc[w + 1]
    ) - (alpha.hat + beta.hat * x)

  }
  res.boot <- rep(NA, n)
  for(i in 1:n) {
    res.boot[i] <- residual.bootstrap(y[i], x[i])
  }
  data.frame(x = x, res = res.boot)
}


################################################################################
# Model specified correctly
################################################################################

# Fitted models
house.clm <- clm(formula = y ~ x + I(x ^ 2), data = d, link = "probit")
house.polr <- polr(formula = y ~ x + I(x ^ 2), data = d, method = "probit")
house.vglm <- vglm(formula = y ~ x + I(x ^ 2), data = d,
                    family = cumulative(link = probit, parallel = TRUE))

# Generate data from Figure 1
fig1 <- figure1(house.vglm, data = d)

# Plots
pdf("slowtests\\figures\\figure1.pdf", width = 7, height = 12)
par(mfrow = c(4, 2))
resplot(house.clm, what = "covariate", x = d$x, main = "ordinal::clm")
resplot(house.clm, what = "qq", main = "ordinal::clm")
resplot(house.polr, what = "covariate", x = d$x, main = "MASS::polr")
resplot(house.polr, what = "qq", main = "MASS::polr")
resplot(house.vglm, what = "covariate", x = d$x, main = "VGAM::vglm")
resplot(house.vglm, what = "qq", main = "VGAM::vglm")
plot(fig1, ylab = "Residual", main = "Figure 1(a)")
lines(lowess(fig1), lwd = 2, col = "red")
qqnorm(fig1$res, main = "Figure 1(b)")
qqline(fig1$res, col = "red")
dev.off()


################################################################################
# Model specified incorrectly (quadratic term is missed)
################################################################################

# Fitted models
house.clm.wrong <- clm(formula = y ~ x, data = d, link = "probit")
house.polr.wrong <- polr(formula = y ~ x, data = d, method = "probit")
house.vglm.wrong <- vglm(formula = y ~ x, data = d,
                         family = cumulative(link = probit, parallel = TRUE))

# Generate data from Figure 1
fig3 <- figure3(house.vglm.wrong, data = d)

# Plots
pdf("slowtests\\figures\\figure3.pdf", width = 7, height = 7)
par(mfrow = c(2, 2))
resplot(house.clm.wrong, what = "covariate", x = d$x, main = "ordinal::clm")
resplot(house.polr.wrong, what = "covariate", x = d$x, main = "MASS::polr")
resplot(house.vglm.wrong, what = "covariate", x = d$x, main = "VGAM::vglm")
plot(fig3, ylab = "Residual", main = "Figure 3(a)")
lines(lowess(fig3), lwd = 2, col = "red")
dev.off()

# Qhat about jittering?
res <- resids(house.polr, type = "jitter")
plot(d$x, res)
