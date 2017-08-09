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
genFig1Data <- function(model, data) {
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
  data.frame(x = x, y = res.boot)
}


# Function to generate data for Figure 3(a)
genFig3Data <- function(model, data) {
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
  data.frame(x = x, y = res.boot)
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
fig1 <- genFig1Data(fit.vglm, data = d)

# Residuals-by-covariate and Q-Q plots
p1 <- autoplot(fit.clm, what = "covariate", x = d$x) + ggtitle("ordinal::clm")
p2 <- autoplot(fit.clm, what = "qq") + ggtitle("ordinal::clm")
p3 <- autoplot(fit.polr, what = "covariate", x = d$x) + ggtitle("MASS::polr")
p4 <- autoplot(fit.polr, what = "qq") + ggtitle("MASS::polr")
p5 <- autoplot(fit.vglm, what = "covariate", x = d$x) + ggtitle("VGAM::vglm")
p6 <- autoplot(fit.vglm, what = "qq") + ggtitle("VGAM::vglm")
p7 <- ggplot(fig1, aes(x, y)) +
  geom_point(color = "#444444", size = 2) +
  geom_smooth(color = "red", se = FALSE) +
  ylab("Surrogate residual") +
  ggtitle("Figure 1(a)")
p8 <- ggplot(fig1, aes(sample = y)) +
  geom_qq(color = "#444444", size = 2) +
  ggtitle("Figure 1(b)")

# Save plot
pdf("slowtests\\figures\\model-specified-correctly.pdf", width = 7, height = 12)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 2)
dev.off()


################################################################################
# Model specified incorrectly (quadratic term is missed)
################################################################################

# Fitted models
fit.clm <- clm(formula = y ~ x, data = d, link = "probit")
fit.polr <- polr(formula = y ~ x, data = d, method = "probit")
fit.vglm <- vglm(formula = y ~ x, data = d,
                 family = cumulative(link = probit, parallel = TRUE))

# Generate data from Figure 1
fig3 <- genFig3Data(fit.vglm, data = d)

# Residuals-by-covariate and Q-Q plots
p1 <- autoplot(fit.clm, what = "covariate", x = d$x) + ggtitle("ordinal::clm")
p2 <- autoplot(fit.clm, what = "qq") + ggtitle("ordinal::clm")
p3 <- autoplot(fit.polr, what = "covariate", x = d$x) + ggtitle("MASS::polr")
p4 <- autoplot(fit.polr, what = "qq") + ggtitle("MASS::polr")
p5 <- autoplot(fit.vglm, what = "covariate", x = d$x) + ggtitle("VGAM::vglm")
p6 <- autoplot(fit.vglm, what = "qq") + ggtitle("VGAM::vglm")
p7 <- ggplot(fig3, aes(x, y)) +
  geom_point(color = "#444444", size = 2) +
  geom_smooth(color = "red", se = FALSE) +
  ylab("Surrogate residual") +
  ggtitle("Figure 3(a)")
p8 <- ggplot(fig1, aes(sample = y)) +
  geom_qq(color = "#444444", size = 2) +
  ggtitle("Figure 3(b)")

# Save plot
pdf("slowtests\\figures\\model-specified-incorrectly.pdf", width = 7, height = 12)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 2)
dev.off()
