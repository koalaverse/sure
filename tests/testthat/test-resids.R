context("Surrogate residuals")


test_that("resids work for \"clm\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("ordinal")

  # Load data
  data(df1)

  # Fit cumulative link model
  fit <- ordinal::clm(y ~ x + I(x ^ 2), data = df1, link = "logit")

  # Compute residuals
  res1 <- resids(fit)
  res2 <- resids(fit, nsim = 10)

  # Expectations
  expect_equal(length(res1), nrow(df1))
  expect_equal(length(res2), nrow(df1))
  expect_null(attr(res1, "boot.reps"))
  expect_null(attr(res1, "boot.id"))
  expect_is(attr(res2, "boot.reps"), "matrix")
  expect_is(attr(res2, "boot.id"), "matrix")
  expect_equal(dim(attr(res2, "boot.reps")), c(nrow(df1), 10))
  expect_equal(dim(attr(res2, "boot.id")), c(nrow(df1), 10))

})


test_that("resids work for \"lrm\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("rms")

  # Load data
  data(df1)

  # Fit cumulative link model
  fit <- rms::lrm(y ~ x, data = df1)

  # Compute residuals
  res1 <- resids(fit)
  res2 <- resids(fit, nsim = 10)

  # Expectations
  expect_equal(length(res1), nrow(df1))
  expect_equal(length(res2), nrow(df1))
  expect_null(attr(res1, "boot.reps"))
  expect_null(attr(res1, "boot.id"))
  expect_is(attr(res2, "boot.reps"), "matrix")
  expect_is(attr(res2, "boot.id"), "matrix")
  expect_equal(dim(attr(res2, "boot.reps")), c(nrow(df1), 10))
  expect_equal(dim(attr(res2, "boot.id")), c(nrow(df1), 10))

})


test_that("resids work for \"orm\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("rms")

  # Load data
  data(df1)

  # Fit cumulative link model
  fit <- rms::orm(y ~ x, data = df1, family = logistic)

  # Compute residuals
  res1 <- resids(fit)
  res2 <- resids(fit, nsim = 10)

  # Expectations
  expect_equal(length(res1), nrow(df1))
  expect_equal(length(res2), nrow(df1))
  expect_null(attr(res1, "boot.reps"))
  expect_null(attr(res1, "boot.id"))
  expect_is(attr(res2, "boot.reps"), "matrix")
  expect_is(attr(res2, "boot.id"), "matrix")
  expect_equal(dim(attr(res2, "boot.reps")), c(nrow(df1), 10))
  expect_equal(dim(attr(res2, "boot.id")), c(nrow(df1), 10))

})


test_that("resids work for \"polr\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("MASS")

  # Load data
  data(df1)

  # Fit cumulative link model
  fit <- MASS::polr(y ~ x + I(x ^ 2), data = df1, method = "logistic")

  # Compute residuals
  res1 <- resids(fit)
  res2 <- resids(fit, nsim = 10)

  # Expectations
  expect_equal(length(res1), nrow(df1))
  expect_equal(length(res2), nrow(df1))
  expect_null(attr(res1, "boot.reps"))
  expect_null(attr(res1, "boot.id"))
  expect_is(attr(res2, "boot.reps"), "matrix")
  expect_is(attr(res2, "boot.id"), "matrix")
  expect_equal(dim(attr(res2, "boot.reps")), c(nrow(df1), 10))
  expect_equal(dim(attr(res2, "boot.id")), c(nrow(df1), 10))

})


test_that("resids work for \"vglm\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("rms")

  # Load data
  data(df1)

  # Fit cumulative link model
  suppressWarnings(
    fit <- VGAM::vglm(y ~ x + I(x ^ 2), data = df1,
                      family = VGAM::cumulative(link = "logit",
                                                parallel = TRUE))
  )

  # Compute residuals
  res1 <- resids(fit)
  res2 <- resids(fit, nsim = 10)

  # Expectations
  expect_equal(length(res1), nrow(df1))
  expect_equal(length(res2), nrow(df1))
  expect_null(attr(res1, "boot.reps"))
  expect_null(attr(res1, "boot.id"))
  expect_is(attr(res2, "boot.reps"), "matrix")
  expect_is(attr(res2, "boot.id"), "matrix")
  expect_equal(dim(attr(res2, "boot.reps")), c(nrow(df1), 10))
  expect_equal(dim(attr(res2, "boot.id")), c(nrow(df1), 10))

})




