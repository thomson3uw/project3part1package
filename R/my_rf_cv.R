# Function: my_rf_cv
# Description: runs k-fold cross validation for random forests using the penguins data set
# Input: k, the number of folds
# Output: the average MSE across all k folds
my_rf_cv <- function(k) {
  # load the penguins data set
  data("my_penguins")
  # omit the NAs in my_penguins
  penguins_no_NA <- na.omit(my_penguins)
  # set the relevant columns of penguins as our data
  data <- data.frame("body_mass" = penguins_no_NA$body_mass_g,
                     "bill_length" = penguins_no_NA$bill_length_mm,
                     "bill_depth" = penguins_no_NA$bill_depth_mm,
                     "flipper_length" = penguins_no_NA$flipper_length_mm)

  n <-  length(data[, 1])
  # randomly assign each entry in the data set to a fold
  fold <- sample(rep(1:k, length = n))
  data$fold <- fold

  MSEs <- c()
  # loop through each fold
  for (i in 1:k) {
    data_train <- data %>% filter(fold != i)
    data_test <- data %>% filter(fold == i)

    rf_cv <- randomForest(body_mass ~ bill_length + bill_depth + flipper_length,
                          data = data_train, ntree = 100)

    rf_pred <- predict(rf_cv, data_test[, -1])

    # calculate the MSE for this fold
    MSEs[i] <- mean((rf_pred - data_test[, 1])^2)
  }

  # return the average MSE across all k folds
  return(mean(MSEs))
}