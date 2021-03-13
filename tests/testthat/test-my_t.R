test_that("returns t-test results", {
  x <- my_t.test(mtcars$mpg, "greater", 15)
  expect_equal(x$test_stat > 4, TRUE)
  expect_equal(x$df, 31)
  expect_equal(x$alternative, "greater")
  expect_equal(x$p_val < 10 ^ -4, TRUE)
  x <- my_t.test(mtcars$mpg, "less", 15)
  expect_equal(x$df, 31)
  expect_equal(x$alternative, "less")
  expect_equal(x$p_val > 10 ^ -4, TRUE)
  x <- my_t.test(mtcars$mpg, "two.sided", 15)
  expect_equal(x$df, 31)
  expect_equal(x$alternative, "two.sided")
  expect_equal(x$p_val < 10 ^ -4, TRUE)
})
test_that("throws correct error", {
  expect_error(
    my_t.test(mtcars$mpg, "none", 15),
    "alternative must be \"greater\", \"less\", or \"two.sided\""
  )
})

