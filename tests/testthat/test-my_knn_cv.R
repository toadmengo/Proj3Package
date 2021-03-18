test_that("knn works", {
  x <- my_knn_cv(iris[c(1:4)], iris$Species, 1, 5)
  expect_type(x$cv_err, "double")
  expect_type(x$class, "integer")
})
test_that("throws error", {
  expect_error(my_knn_cv(iris[c(1:4)], mtcars$mpg, 1, 2))
})
