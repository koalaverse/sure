################################################################################
# Setup
################################################################################

# Load required packages
library(tmvtnorm)
library(truncdist)


################################################################################
# Test that truncdist::rtrunc and tmvtnorm::rtmvnorm give the same results
################################################################################

# Simulate data
set.seed(101)
x <- rtmvnorm(10000, mean = 0, sigma = 1, lower = -1, upper = Inf,
              algorithm = "gibbs")
y <- rtrunc(10000, spec = "norm", a = -1, b = Inf, mean = 0, sd = 1)

# Densities should look the same!
par(mfrow = c(1, 3))
plot(density(x), main = "tmvtnorm::rtmvnorm")
plot(density(y), main = "truncdist::rtrunc")
qqplot(x, y, xlab = "tmvtnorm::rtmvnorm", ylab = "truncdist::rtrunc")
abline(0, 1, col = "red")

# Test the null hypothesis that x and y were drawn from the same distirbution
ks.test(x, y)
# Two-sample Kolmogorov-Smirnov test
#
# data:  x and y
# D = 0.0088, p-value = 0.8335
# alternative hypothesis: two-sided

################################################################################
# Test that truncdist::rtrunc is vectorized
################################################################################

# Simulate data
set.seed(101)
n <- 10000
mu <- sample(c(-5, 0, 5), size = n, replace = TRUE)
a <- sample(c(-Inf, -7), size = n, replace = TRUE)
b <- sample(c(7, Inf), size = n, replace = TRUE)
x <- numeric(n)
for (i in seq_len(n)) {  # non-vectorized way
  x[i] <- rtrunc(n = 1, spec = "norm", a = a[i], b = b[i], mean = mu[i])
}
y <- rtrunc(n = 10000, spec = "norm", a = a, b = b, mean = mu)

# Densities should look the same!
par(mfrow = c(1, 3))
plot(density(x), main = "Non-vectorized")
plot(density(y), main = "Vectorized")
qqplot(x, y, xlab = "Non-vectorized", ylab = "Vectorized")
abline(0, 1, col = "red")
