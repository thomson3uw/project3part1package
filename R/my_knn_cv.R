#' Cross Validation for K-Nearest Neighbors
#'
#' Run k-fold cross validations for k-nearest neighbors.
#'
#' @param data The input data frame used for prediction.
#' Note this data should not include NAs.
#' @param cl The true class values of the data, which are compared against the KNN prediction.
#' Note these classification should not include NAs.
#' @param k_nn An integer that represents the number of neighbors KNN should use.
#' @param k_cv The number of folds that the function will use for cross validation.
#'
#' @keywords inference
#'
#' @return A list containing the following items.
#' The vector \code{class}, which contains the predicted classes when KNN train on the entire provided data set.
#' The number \code{cv_err}, which is between 0 and 1 and shows the cross-validation misclassification error.
#'
#' @importFrom stats model.frame model.matrix model.response predict pt sd na.omit
#'
#' @export
my_knn_cv <- function(data, cl, k_nn, k_cv) {
  n <-  length(data[, 1])
  # randomly assign each entry in the data set to a fold
  fold <- sample(rep(1:k_cv, length = n))
  data$fold <- fold
  cl <- data.frame(cl, fold)

  miss_rates <- c()

  # loop through each fold
  for (i in 1:k_cv) {
    data_train <- data %>% dplyr::filter(fold != i)
    data_test <- data %>% dplyr::filter(fold == i)

    cl_train <- (cl %>% dplyr::filter(fold != i))[, 1]
    cl_test <- (cl %>% dplyr::filter(fold == i))[, 1]

    # remove fold from the train/test data
    data_train <- data_train[, -length(data_train)]
    data_test <- data_test[, -length(data_test)]

    knn_cv <- class::knn(train = data_train,
                  test = data_test,
                  cl = cl_train,
                  k = k_nn)

    # record misclassification rates
    miss_rates[i] <- mean(as.numeric(knn_cv != cl_test))
  }

  #remove fold from data
  data <- data[, -length(data)]

  class <- class::knn(train = data,
               test = data,
               cl = cl[, 1],
               k = k_nn)

  # calculate the proportion of misclassifications
  cv_err <- mean(miss_rates)

  # return the classifications and the misclassification error
  return(list(class = class, cv_err = cv_err))
}
