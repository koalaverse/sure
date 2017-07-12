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
# Simulate data based on gumbel distributed errors
################################################################################

# Function to simulate latent variable Z from a quadratic function of X plus
# noise; the ordinal outcome W is obtained by discretizing Z.
simData <- function(n = 2000, alpha = 16, beta = c(-8, 1),
                    threshold = c(0, 4, 8)) {
  set.seed(977)
  x <- runif(n, min = 1, max = 7)
  z <- alpha + beta[1L] * x + beta[2L] * x ^ 2 + ordr:::rgumbel(n)
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
# Model using probit (incorrect) and log-log (correct) link function
################################################################################

# Fitted models
clm.probit <- clm(y ~ x + I(x ^ 2), data = d, link = "probit")
clm.loglog <- clm(y ~ x + I(x ^ 2), data = d, link = "loglog")
polr.probit <- polr(y ~ x + I(x ^ 2), data = d, method = "probit")
polr.loglog <- polr(y ~ x + I(x ^ 2), data = d, method = "loglog")

# Q-Q plots
pdf("slowtests\\figures\\link-gumbel.pdf", width = 7, height = 7)
grid.arrange(
  autoplot(clm.probit, nsim = 50, what = "qq", main = "clm: probit link"),
  autoplot(clm.loglog, nsim = 50, what = "qq", main = "clm: log-log link"),
  autoplot(polr.probit, nsim = 50, what = "qq", main = "polr: probit link"),
  autoplot(polr.loglog, nsim = 50, what = "qq", main = "polr: log-log link"),
  ncol = 2
)
dev.off()

# Save figure
ggsave("slowtests\\figures\\link-gumbel.pdf", width = 7, height = 7,
       device = "pdf")
