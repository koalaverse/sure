################################################################################
# Setup
################################################################################

# Load packages for fitting binary regression
library(rms)      # functions lrm() and orm()
library(VGAM)
# library(stats)

# Load our package
library(sure)      # for surrogate-based residuals
library(ggplot2)   # for plotting

# Load other packages
library(dplyr)  # for balancing data


################################################################################
# Simulate data
################################################################################

# Function to simulate logsitic regression data
simData <- function(n = 2000) {
  x <- runif(n, min = 1, max = 7)
  y <- rbinom(n, size = 1, prob = plogis(16 - 8 * x + 1 * x ^ 2))
  data.frame("x" = x, "y" = as.factor(y))
}

# Simulate balanced data
d <- simData(n = 10000)
d <- d %>%
  group_by(y) %>%
  slice(1:1000) %>%
  ungroup() %>%
  as.data.frame()
table(d$y)


################################################################################
# Fit ordinal regression models with probit link
################################################################################

# Fitted models
fit1 <- glm(y ~ x + I(x ^ 2), data = d, family = binomial(link = "probit"))
fit2 <- glm(y ~ x, data = d, family = binomial(link = "cloglog"))

# Response scale
grid.arrange(
  autoplot(fit1, jitter.scale = "response", what = "covariate", x = d$x),
  autoplot(fit2, jitter.scale = "response", what = "covariate", x = d$x),
  ncol = 2
)

# Probability scale
grid.arrange(
  autoplot(fit1, jitter.scale = "probability", what = "covariate", x = d$x),
  autoplot(fit2, jitter.scale = "probability", what = "covariate", x = d$x),
  ncol = 2
)

# Probability scale
grid.arrange(
  autoplot(fit1, jitter.scale = "probability", what = "qq"),
  autoplot(fit2, jitter.scale = "probability", what = "qq"),
  ncol = 2
)
