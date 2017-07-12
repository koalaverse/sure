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
  set.seed(977)
  x <- runif(n, min = 1, max = 7)
  z <- alpha + beta[1L] * x + beta[2L] * x ^ 2 + exp(rnorm(n)) - exp(1 / 2)
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
d <- simData(n = 2000)
table(d$y)
#
#   1    2    3    4
# 275 1047  544  134


################################################################################
# Model diagnosis when the link function is incorrect
################################################################################

# Function to generate data for Figure 4(a)-(b)
figure4 <- function(model, data) {
  set.seed(977)
  n <- nrow(data)
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



################################################################################
# Model specified correctly
################################################################################

# Fitted models
fit.clm <- clm(formula = y ~ x + I(x ^ 2), data = d, link = "probit")
fit.polr <- polr(formula = y ~ x + I(x ^ 2), data = d, method = "probit")
fit.vglm <- vglm(formula = y ~ x + I(x ^ 2), data = d,
                    family = cumulative(link = probit, parallel = TRUE))

# Generate data from Figure 1
fig4 <- figure4(fit.vglm, data = d)
res <- fig4$res
class(res) <- c("numeric", "resid")

# Plots
pdf("slowtests\\figures\\figure4.pdf", width = 7, height = 12)
grid.arrange(
  autoplot(fit.clm, what = "covariate", x = d$x, main = "ordinal::clm"),
  autoplot(fit.clm, what = "qq", main = "ordinal::clm"),
  autoplot(fit.polr, what = "covariate", x = d$x, main = "MASS::polr"),
  autoplot(fit.polr, what = "qq", main = "MASS::polr"),
  autoplot(fit.vglm, what = "covariate", x = d$x, main = "VGAM::vglm"),
  autoplot(fit.vglm, what = "qq", main = "VGAM::vglm"),
  autoplot(res, what = "covariate", x = d$x, main = "Figure 4(a)"),
  autoplot(res, what = "qq", main = "Figure 4(b)"),
  ncol = 2
)
dev.off()
