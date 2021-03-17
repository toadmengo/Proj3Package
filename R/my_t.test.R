#' t-test function
#'
#' This function performs a one sample t-test.
#'
#' @param x Numeric input of data.
#' @param alternative Character input, either \code{"greater"},
#' \code{"less"}, or \code{"two.sided"}, corresponding to what hypothesis
#' is being tested.
#' @param mu The numeric null hypothesis value.
#' @keywords inference
#'
#' @return List with values:
#' \code{test_stat} - Numeric representing test_statistic,
#' \code{df} - Numeric representing degrees of freedom,
#' \code{alternative} - Character either "greater", "less", or "two.sided", representing
#' type of t-test,
#' \code{p_val} - Numeric representing the p-value.
#'
#' @examples
#' my_t.test(c(1:5), "greater", 4)
#' my_t.test(c(1:10), "less", 4)
#' my_t.test(c(1:5), "two.sided", 0)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # calculate standard error of x
  se = stats::sd(x) / (length(x) ^ 0.5)
  # get the t statistic
  test_stat <- (mean(x) - mu) / se
  # calculate degrees of freedom
  df <- length(x) - 1

  # perform a test, with alternative representing what type of test
  if (alternative == "less") {
    p_val <- stats::pt(test_stat, df, lower.tail = TRUE)
  } else if (alternative == "greater") {
    p_val <- stats::pt(test_stat, df, lower.tail = FALSE)
  } else if (alternative == "two.sided") {
    p_val <- stats::pt(abs(test_stat), df, lower.tail = FALSE) * 2
  } else {
    # throw error if alternative is not one of the options above
    stop("alternative must be \"greater\", \"less\", or \"two.sided\"")
  }

  # return a list of t stat, degrees of freedom, test type, and p value
  return_list <- list("test_stat" = test_stat,
                      "df" = df,
                      "alternative" = alternative,
                      "p_val" = p_val)
  return(return_list)
}
