#' Random Forest Cross-Validation
#'
#' Performs a random forest cross-validation test specific to the
#' \code{palmerpenguins} dataset, with variables
#' \code{bill_depth_mm}, \code{bill_length_mm}, \code{flipper_length_mm},
#' with response variable \code{body_mass_g}.
#'
#' @param k Numeric input representing number of folds for cross-validation.
#' @keywords prediction
#'
#' @return Numeric representing MSE of the random forest.
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  # select the data we will use and filter
  train <- my_penguins %>%
    dplyr::select("body_mass_g",
           "bill_depth_mm",
           "bill_length_mm",
           "flipper_length_mm") %>%
    stats::na.omit()
  # categorize data and cl into k_cv parts randomly
  inds <- sample(rep(1:k, length = nrow(train)))
  # create NA vector to store predictions
  pred <- c(rep(NA, nrow(train)))

  for (i in 1:k) {
    # split data into folds for training and the fold for testing
    data_train <- train[inds != i, ]
    data_test <- train[inds == i, ]
    # train the random forest with the training data and 100 trees
    forest <- randomForest::randomForest(
      body_mass_g ~ bill_depth_mm + flipper_length_mm + bill_length_mm,
      data = data_train, ntree = 100)
    # predict the class for the test set data using training data
    pred[inds == i] <- stats::predict(forest, data_test[, -1])
  }
  # calculate and return MSE
  diff <- train$body_mass_g - pred
  return(sum(diff^2) / length(diff))
}
