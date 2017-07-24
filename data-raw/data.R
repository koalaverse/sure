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
df.heteroscedastic <- simHeteroscedasticData(n = 2000)
table(df.heteroscedastic$y)


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
df.quadratic <- simQuadraticData(n = 2000)
table(df.quadratic$y)


# Save data sets
save(df.heteroscedastic, file = "data\\df.heteroscedastic.RData")
save(df.quadratic, file = "data\\df.quadratic.RData")
