#' K Nearest Neighbor Cross-Validation
#'
#' Performs a KNN cross-validation test.
#'
#' @param train Dataframe with training data, without response variable.
#' @param cl Factor of response variables.
#' @param k_nn Integer representing number of nearest neighbors.
#' @param k_cv Integer for number of folds in cross validation.
#' @keywords prediction
#'
#' @return List with elements:
#' class - Factor of classification using the training data,
#' cv_err - Numeric representing cross-validation error.
#'
#' @examples
#' my_knn_cv(iris[1:4], iris$Species, 5, 5)
#' my_knn_cv(iris[1:4], iris$Species, 1, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # categorize data and cl into k_cv parts randomly
  inds <- sample(rep(1:k_cv, length = length(cl)))
  # create NA vector to store predictions
  pred <- c(rep(NA, length(cl)))

  for (i in 1:k_cv) {
    # split data into folds for training and the fold for testing
    data_train <- train[inds != i, ]
    data_test <- train[inds == i, ]
    # predict the class for the test set data using training data
    pred[inds == i] <- as.character(
      class::knn(data_train, data_test, cl[inds != i], k = k_nn)
      )
  }
  # compute the total number of misclassifications
  errs <- cl != pred
  # divide by total observations to get a proportion
  cv_err <- sum(errs) / length(cl)

  # compute k_nn using the full training data
  clas <- class::knn(train, train, cl, k = k_nn)

  list("class" = clas, "cv_err" = cv_err) %>%
    return()
}
