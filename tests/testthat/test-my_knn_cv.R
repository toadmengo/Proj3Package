test_that("knn works", {
  x <- my_knn_cv(iris[c(1:4)], iris$Species, 1, 5)
  expect_equal(x$class, iris$Species)
})
