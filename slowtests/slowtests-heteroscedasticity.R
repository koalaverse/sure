################################################################################
# Setup
################################################################################

# Load required packages
library(ordr)
library(tmvtnorm)
library(VGAM)


################################################################################
# Simulate data
################################################################################

# Function to ordinalize a continous variable
ordinalize <- function(z, threshold){
  sapply(z, FUN = function(zz) {
    ordinal.value <- 1
    index <- 1
    while(index <= length(threshold) && zz > threshold[index]) {
      ordinal.value <- ordinal.value + 1
      index <- index + 1
    }
    ordinal.value
  })
}

# Function to simulate latent variable Z from a quadratic function of X plus
# noise; the ordinal outcome W is obtained by discretizing Z.
simData <- function() {
  n <- 2000
  alpha <- 36
  beta_1 <- 4
  thrd <- c(0, 30, 70, 100)
  X <- runif(n, min = 2, max = 7)
  sigma <- X ^ 2
  RES <- rnorm(n, mean = 0, sd = sigma)
  Z <- alpha + beta_1 * X + RES
  W <- ordinalize(Z, thrd)
  data.frame(W, X)
}

# Simulate data
set.seed(108)
d <- simData()


################################################################################
# Fit ordinal regression models
################################################################################

# Fitted models
fit.vglm <- vglm(formula = (W-1) ~ X, data = d,
                 family = cumulative(link = probit, parallel = TRUE))

# Residuals
res.vglm <- resids(fit.vglm, nsim = 10)

# Compare to Figure 6 (a)
par(mfrow = c(1, 2))
plot(d$X, res.vglm, main = "nsim = 1")  # original residuals
plot(d$X, res.vglm, main = "nsim = 10")  # original residuals w/ bootstrap reps
boot.reps <- attr(res.vglm, "boot.reps")
boot.id <- attr(res.vglm, "boot.id")
for (i in 1:100) {  # add bootstrap residuals
  points(d$X[boot.id[, i]], boot.reps[, i], col = adjustcolor(1, alpha.f = 0.1))
}


resplot <- function(object, what = c("qq", "covariate"), x, nsim = 1, alpha = 1,
                    xlab = NULL, ylab = NULL, main = NULL, ...) {
  # TODO: Support other link functions in Q-Q plot.
  res <- resids(object, nsim = nsim)
  what <- match.arg(what)
  if (what == "qq") {  # Q-Q plot
    if (nsim == 1) {
      qqnorm(res, xlab = xlab, ylab = ylab, main = main)
      qqline(res, col = "red")
    } else {
      qqnorm(as.numeric(attr(res.vglm, "boot.reps")),
             xlab = xlab, ylab = ylab, main = main)
      qqline(as.numeric(attr(res.vglm, "boot.reps")), col = "red")
    }
  } else if (what == "covariate") {  # residual-by-covariate
    if (is.null(ylab)) {
      ylab <- "residual"
    }
    if (nsim == 1) {
      plot(x, res, xlab = xlab, ylab = ylab, main = main)
      lines(lowess(x, res), lwd = 2, col = "red")
      abline(h = c(-2, 2), lty = 2, lwd = 1, col = "red")
    } else {
      plot(x, res, xlab = xlab, ylab = ylab, main = main)  # original residuals w/ bootstrap reps
      boot.reps <- attr(res, "boot.reps")
      boot.id <- attr(res, "boot.id")
      for (i in seq_len(nsim)) {  # add bootstrap residuals
        points(x[boot.id[, i]], boot.reps[, i],
               col = adjustcolor(1, alpha.f = alpha))
      }
    }
    abline(h = c(-2, 2), lty = 2, lwd = 1, col = "red")
  }
}
