test_that("returns correct t-test results", {
  x <- my_t.test(mtcars$mpg, "greater", 15)
  y <- t.test(mtcars$mpg, alternative = "greater", mu = 15)
  expect_equal(x$test_stat, y$statistic[["t"]])
  expect_equal(x$alternative, y$alternative)
  expect_equal(x$df, y$parameter[["df"]])
  expect_equal(x$p_val, y$p.value)
  x <- my_t.test(mtcars$mpg, "less", 15)
  y <- t.test(mtcars$mpg, alternative = "less", mu = 15)
  expect_equal(x$test_stat, y$statistic[["t"]])
  expect_equal(x$alternative, y$alternative)
  expect_equal(x$df, y$parameter[["df"]])
  expect_equal(x$p_val, y$p.value)
  x <- my_t.test(mtcars$mpg, "two.sided", 15)
  y <- t.test(mtcars$mpg, alternative = "two.sided", mu = 15)
  expect_equal(x$test_stat, y$statistic[["t"]])
  expect_equal(x$alternative, y$alternative)
  expect_equal(x$df, y$parameter[["df"]])
  expect_equal(x$p_val, y$p.value)
})
test_that("throws correct error", {
  expect_error(
    my_t.test(mtcars$mpg, "none", 15),
    "alternative must be \"greater\", \"less\", or \"two.sided\""
  )
})

