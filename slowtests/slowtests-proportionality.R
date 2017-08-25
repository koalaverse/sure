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
  y <-   sapply(alpha + beta * x + rnorm(n, sd = x ^ 2), FUN = function(zz) {
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
n <- 2000
res.boot <- rep(NA, n)
for(i in 1:n) {
  res.boot[i] <- residual.bootstrap(d$y[i], d$x[i])
}


################################################################################
# Fit ordinal regression models with probit link
################################################################################

# Fitted models
fit.clm <- clm(y ~ x, data = d, link = "probit")
fit.polr <- polr(y ~ x, data = d, method = "probit")
fit.vglm <- vglm(y ~ x, data = d,
                 family = cumulative(link = probit, parallel = TRUE))
fit.orm <- orm(y ~ x, data = d, family = probit)


################################################################################
# Create plots
################################################################################

# Compare to Figure 6(a)
p1 <- autoplot(fit.clm, what = "covariate", x = d$x) + ggtitle("ordinal::clm")
p2 <- autoplot(fit.polr, what = "covariate", x = d$x) + ggtitle("MASS::polr")
p3 <- autoplot(fit.vglm, what = "covariate", x = d$x) + ggtitle("VGAM::vglm")
p4 <- ggplot(data.frame(x = d$x, y = res.boot), aes(x, y)) +
  geom_point(size = 2, color = "#444444") +
  geom_smooth(color = "red", se = FALSE) +
  ylab("Surrogate residual") +
  ggtitle("Figure 6(a)")
# pdf("slowtests\\figures\\heteroscedasticity.pdf", width = 7, height = 6)
grid.arrange(p1, p2, p3, p4, ncol = 2)
# dev.off()
