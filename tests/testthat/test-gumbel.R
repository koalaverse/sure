context("Gumbel functions")

test_that("qgumbel is the inverse of pgumbel", {
  expect_equal(qgumbel(pgumbel(3)), 3)
  expect_equal(qGumbel(pGumbel(3)), 3)
  expect_equal(length(rgumbel(10)), 10)
  expect_equal(length(rGumbel(10)), 10)
})
