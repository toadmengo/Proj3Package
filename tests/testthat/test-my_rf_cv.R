test_that("my_rf_cv works", {
  cv_err <- my_rf_cv(5)
  expect_true(cv_err < 200000)
})
