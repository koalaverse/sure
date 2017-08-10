context("Truncated distributions")

test_that("truncated distributions work", {
  set.seed(101)
  x <- .rtrunc(1000, spec = "norm", a = 0)
  expect_true(min(x) > 0)
  expect_identical(.qtrunc(0.5, spec = "norm"), 0)
})
