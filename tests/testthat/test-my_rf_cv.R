test_that("my_rf_cv works", {
  cv_err <- my_rf_cv(5)
  expect_type(cv_err, "double")
})
test_that("throws error", {
  expect_error(my_rf_cv(1, 1))
})
