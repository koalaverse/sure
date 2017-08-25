# Function to simulate quadratic data
simQuadraticData <- function(n = 2000) {
  threshold <- c(0, 4, 8)
  x <- runif(n, min = 1, max = 7)
  z <- 16 - 8 * x + 1 * x ^ 2 + rnorm(n)  # rlnorm(n)
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
df1 <- simQuadraticData(n = 2000)
table(df1$y)


# Function to simulate heteroscedastic data
simHeteroscedasticData <- function(n = 2000) {
  threshold <- c(0, 30, 70, 100)
  x <- runif(n, min = 2, max = 7)
  y <-   sapply(36 + 4 * x + rnorm(n, sd = x ^ 2), FUN = function(zz) {
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

# Simulate heteroscedastic data
set.seed(108)
df2 <- simHeteroscedasticData(n = 2000)
table(df2$y)


# Function to simulate data with Gumbel errors on the linear predictor scale
simGumbelData <- function(n = 2000) {
  x <- runif(n, min = 1, max = 7)
  z <- 16 - 8 * x + 1 * x ^ 2 + ordr:::rgumbel(n)
  y <- ordinalize(z, threshold = c(0, 4, 8))
  data.frame("y" = as.ordered(y), "x" = x)
}

# Simulate data
set.seed(977)
df3 <- simGumbelData(n = 2000)
table(df3$y)

ordinalize <- function(z, threshold) {
  sapply(z, FUN = function(x) {
    ordinal.value <- 1
    index <- 1
    while(index <= length(threshold) && x > threshold[index]) {
      ordinal.value <- ordinal.value + 1
      index <- index + 1
    }
    ordinal.value
  })
}

# Function to simulate the data from Example 5 in Dungang and Zhang (2017).
simProportionalityData <- function(n = 2000) {
  x <- runif(n, min = -3, max = 3)
  z1 <- 0 - 1 * x + rnorm(n)
  z2 <- 0 - 1.5 * x + rnorm(n)
  y1 <- ordinalize(z1, threshold = c(-1.5, 0))
  y2 <- ordinalize(z2, threshold = c(1, 3))
  data.frame("y" = as.ordered(c(y1, y2)), "x" = c(x, x))
}

# Simulate data
set.seed(977)
df4 <- simProportionalityData(n = 2000)
table(df4$y)


# Save data sets
save(df1, file = "data\\df1.RData")
save(df2, file = "data\\df2.RData")
save(df3, file = "data\\df3.RData")
save(df4, file = "data\\df4.RData")
