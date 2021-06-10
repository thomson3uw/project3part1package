# test the input, output and errors of my_knn_cv()

# load the penguins data set and create versions with NAs and without
my_penguins <- stats::na.omit(project3part1package::my_penguins)
penguins_no_NA <- stats::na.omit(my_penguins)

my_data <- data.frame("bill_length" = penguins_no_NA$bill_length_mm,
                      "bill_depth" = penguins_no_NA$bill_depth_mm,
                      "flipper_length" = penguins_no_NA$flipper_length_mm,
                      "body_mass" = penguins_no_NA$body_mass_g)

my_data_with_NA <- data.frame("bill_length" = my_penguins$bill_length_mm,
                      "bill_depth" = my_penguins$bill_depth_mm,
                      "flipper_length" = my_penguins$flipper_length_mm,
                      "body_mass" = my_penguins$body_mass_g)
my_data_with_NA[1, 1] <- NA
my_data_add_NA <- my_data
my_data_add_NA[1, 1] <- NA

my_cl <- penguins_no_NA$species
my_cl_with_NA  <- my_penguins$species
my_cl_add_NA <- my_cl
my_cl_add_NA[1] <- NA

test_that("The data must not contain NA values", {
  expect_error(my_knn_cv(data = my_data_with_NA, cl = my_cl_with_NA, k_nn = 2, k_cv = 5))
  expect_error(my_knn_cv(data = my_data, cl = my_cl_add_NA, k_nn = 2, k_cv = 5))
  expect_error(my_knn_cv(data = my_data_addNA, cl = my_cl, k_nn = 2, k_cv = 5))
})

test_that("K_nn and k_cv must be numbers", {
  expect_error(my_knn_cv(data = my_data, cl = my_cl, k_nn = "a", k_cv = 5))
  expect_error(my_knn_cv(data = my_data, cl = my_cl, k_nn = 2, k_cv = "b"))
})

test_that("Output is of type list", {
  expect_type(my_knn_cv(data = my_data, cl = my_cl, k_nn = 2, k_cv = 5), "list")
})

test_that("Entries in the output list are the correct type", {
  expect_type(my_knn_cv(data = my_data, cl = my_cl, k_nn = 2, k_cv = 5)$cv_err, "double")
  expect_type(my_knn_cv(data = my_data, cl = my_cl, k_nn = 2, k_cv = 5)$class, "integer")
})

test_that("There are the same number of predicted classifications as true classifications", {
  expect_equal(length(my_knn_cv(data = my_data, cl = my_cl, k_nn = 2, k_cv = 5)$class), length(my_cl))
})


