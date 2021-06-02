#' Student's t-Test
#'
#' This function performs a one sample t-test.
#'
#' @param x A vector with numeric entries that provides the data values for the test.
#' @param alternative A string that specifies the type of the alternative hypothesis for the test. This must be \code{"two.sided"}, \code{"less"}, or \code{"greater"}.
#' @param mu A numeric input that specifies the true mean according the the null hypothesis.
#' @keywords inference
#'
#' @return A list containing the following fields. The numeric, \code{test_stat}, which is the calculated t-test statistic. The numeric, \code{df}, which is the degrees of freedom of the test. The string, \code{alternative}, which is the alternative hypothesis that was specified. The numeric, \code{p_val}, which is the p-value calculated by the tests.
#'
#' @examples
#' my_t.test(x = rnorm(100), alternative = "two.sided", mu = 1)
#' my_t.test(x = 1:100, alternative = "greater", mu = 60)
#'
#' @export
my_t.test <- function(x, alternative = c("two.sided", "less", "greater"), mu) {
  # check if x is truly a numeric vector
  if (is.vector(x)) {
    for (i in 1:length(x)) {
      if (!is.numeric(x[i])) {
        stop("non-numeric entries found in x")
      }
    }
  }
  else
  {
    stop("x must be a vector")
  }

  # check if mu is numeric
  if (!is.numeric(mu)) {
    stop("mu must be numeric")
  }

  # calculate the test statistic and degrees of freedom
  std_err <- sd(x) / sqrt(length(x))
  df <- length(x) - 1
  mean_diff <- mean(x) - mu
  test_stat <- mean_diff / std_err

  # determine the p-value of the test
  if (alternative == "two.sided") {
    p_val <- 2 * pt(q = abs(test_stat), df = df, lower.tail = FALSE)
  } else if (alternative == "less") {
    p_val <- pt(q = test_stat, df = df, lower.tail = TRUE)
  } else if (alternative == "greater") {
    p_val <- pt(q = test_stat, df = df, lower.tail = FALSE)
  } else {
    stop("invalid input for alternative")
  }

  return(list(test_stat = test_stat, df = df, alternative = alternative, p_val = p_val))
}
