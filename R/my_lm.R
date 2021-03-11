#' Fits a Linear Model
#'
#' This function fits a linear model using the data.
#'
#' @param lm_form Formula used to fit model.
#' @param lm_data Numeric data used to fit model.
#'
#' @return List with values:
#' Estimate - numeric coefficient of line of best fit,
#' Std. Error - numeric representing the standard error of coefficients,
#' t value - numeric test-statistic of coefficient,
#' Pr(>|t|) - numeric p-value of coefficient.
#'
#' @examples
#' my_lm(speed ~ dist, cars)
#'
#' @export
my_lm <- function(lm_form, lm_data) {
  X <- stats::model.matrix(lm_form, data = lm_data)
  fr <- stats::model.frame(lm_form, data = lm_data)
  Y <- stats::model.response(fr)
  # calculate coefficients
  coeffs <- solve(t(X) %*% X) %*% t(X) %*% Y

  # calculate degrees of freedom
  df <- nrow(lm_data) - length(attr(stats::terms(lm_form),
                                    which = "term.labels")) - 1
  # calculate variance squared and standard errors
  var_squared <- sum(((Y - X %*% coeffs) ^ 2) / df)
  se <- diag((var_squared * solve(t(X) %*% X)) ^ 0.5)
  # calculate t values
  test_stat <- coeffs / se
  # calculate p values
  p_val <- stats::pt(abs(test_stat), df, lower.tail = FALSE) * 2

  # return a table of coeffs, std errors, t values, and p values
  ret <- cbind(coeffs, se, test_stat, p_val)
  colnames(ret) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(ret)
}
