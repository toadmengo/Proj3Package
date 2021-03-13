test_that("linear regression works", {
  x <- my_lm(mpg ~ hp + wt, mtcars)
  expect_true(x[11] < 10 ^ -2)
  expect_true(x[12] < 10 ^ -5)
})
