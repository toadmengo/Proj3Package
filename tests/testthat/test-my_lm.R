test_that("linear regression works", {
  x <- my_lm(mpg ~ hp + wt, mtcars)
  y <- summary(lm(mpg ~ hp + wt, mtcars))
  expect_equal(x, y$coefficients)
})
test_that("throws correct error", {
  expect_error(my_lm(dd ~ ff, mtcars))
})
