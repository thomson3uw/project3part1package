#' Fitting Linear Models
#'
#' This function fits a linear model in the form of the specified formula using the given data.
#'
#' @param formula A formula class object that specifies the structure of the model.
#' @param data A data frame that contains the variables of the model.
#'
#' @keywords inference prediction
#'
#' @return A matrix in the form of a table that contains an estimate, standard error, t-value, and p-value of each coefficient.
#'
#' @examples
#' ## Load in the penguins data set included in project3part1package
#' data(my_penguins)
#' my_penguins <- na.omit(my_penguins)
#'
#' my_lm(bill_length_mm ~ bill_depth_mm + flipper_length_mm, data = my_penguins)
#' my_lm(body_mass_g ~ bill_length_mm - 1, data = my_penguins)
#'
#' @importFrom stats model.frame model.matrix model.response predict pt sd na.omit
#'
#' @export
my_lm <- function(formula, data) {
  # check if data is a data frame
  if (!is.data.frame(data)) {
    stop("data must be a valid dataframe")
  }

  # format the data for the given formula
  my_revised_data <- model.frame(formula = formula, data = data)
  my_matrix <- model.matrix(object = formula, data = my_revised_data)
  my_response <- model.response(my_revised_data)

  # solve for the linear regression coefficients
  beta_hat <- solve(t(my_matrix) %*% my_matrix) %*% t(my_matrix) %*% my_response

  # calculate the values of the other entries in the table
  covariates <- length(my_matrix[1, ])
  df = length(my_matrix[, 1]) - covariates
  sigma_squared <- sum((my_response - (my_matrix %*% beta_hat)) ** 2) / df
  std_err <- sqrt(diag(sigma_squared * solve(t(my_matrix) %*% my_matrix)))
  t_val <- beta_hat / std_err
  p_val <- 2 * pt(abs(t_val), lower.tail = FALSE, df = df)

  # create the table and enter all values
  my_table <- matrix(NA, nrow = covariates, ncol = 4)
  my_table[, 1] <- beta_hat
  my_table[, 2] <- std_err
  my_table[, 3] <- t_val
  my_table[, 4] <- p_val

  # add the column names and the covariates to the row names
  colnames(my_table) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(my_table) <- colnames(my_matrix)

  return(my_table)
}
