# Function: my_knn_cv
# Description: runs k-fold cross validations for k-nearest neighbors
# Input: data, the input data frame (note in the document this is called train),
#        cl, the true class values of the data,
#        k_nn, an integer representing the number of neighbors for KNN,
#        k_cv, the number of folds
# Output: class, a vector of the predicted classes when KNN trains on the entire data set,
#         cv_err, the cross-validaiton misclassification error
my_knn_cv <- function(data, cl, k_nn, k_cv) {
  n <-  length(data[, 1])
  # randomly assign each entry in the data set to a fold
  fold <- sample(rep(1:k_cv, length = n))
  data$fold <- fold
  cl <- data.frame(cl, fold)

  miss_rates <- c()

  # loop through each fold
  for (i in 1:k_cv) {
    data_train <- data %>% filter(fold != i)
    data_test <- data %>% filter(fold == i)

    # remove fold from the train/test data
    data_train <- data_train[, -length(data_train)]
    data_test <- data_test[, -length(data_test)]

    cl_train <- (cl %>% filter(fold != i))[, 1]
    cl_test <- (cl %>% filter(fold == i))[, 1]

    knn_cv <- knn(train = data_train,
                  test = data_test,
                  cl = cl_train,
                  k = k_nn)

    # record misclassification rates
    miss_rates[i] <- mean(as.numeric(knn_cv != cl_test))
  }

  #remove fold from data
  data <- data[, -length(data)]

  class <- knn(train = data,
               test = data,
               cl = cl[, 1],
               k = k_nn)

  # calculate the proportion of misclassifications
  cv_err <- mean(miss_rates)

  # return the classifications and the misclassification error
  return(list(class = class, cv_err = cv_err))
}
