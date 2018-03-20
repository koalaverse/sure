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
  z <- 16 - 8 * x + 1 * x ^ 2 + sure:::rgumbel(n)
  y <- ordinalize(z, threshold = c(0, 4, 8))
  data.frame("y" = as.ordered(y), "x" = x)
}

# Simulate data
set.seed(977)
df3 <- simGumbelData(n = 2000)
table(df3$y)

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


# Function to simulate data from an ordered probit model with an interaction
# term
simInteractionData <- function(n = 2000) {
  threshold <- c(0, 20, 40)
  x1 <- runif(n, min = 1, max = 7)
  x2 <- gl(2, n / 2, labels = c("Control", "Treatment"))
  z <- 16 - 5 * x1 + 3 * (x2 == "Treatment") + 10 * x1 * (x2 == "Treatment") +
    rnorm(n)
  y <- sapply(z, FUN = function(zz) {
    ordinal.value <- 1
    index <- 1
    while(index <= length(threshold) && zz > threshold[index]) {
      ordinal.value <- ordinal.value + 1
      index <- index + 1
    }
    ordinal.value
  })
  data.frame("y" = as.ordered(y), "x1" = x1, "x2" = x2)
}

# Simulate data
set.seed(922)
df5 <- simInteractionData(n = 2000)
pairs(df5)

# Save data sets
save(df1, file = "data/df1.RData")
save(df2, file = "data/df2.RData")
save(df3, file = "data/df3.RData")
save(df4, file = "data/df4.RData")
save(df5, file = "data/df5.RData")
