test_that("MSE is a number", {
  expect_type(my_rf_cv(5), "double")
  expect_type(my_rf_cv(10), "double")
  expect_type(my_rf_cv(2), "double")
  expect_type(my_rf_cv(7), "double")
  expect_type(my_rf_cv(3), "double")
})

test_that("input must be a positive integer", {
  expect_error(my_rf_cv(5.2))
  expect_error(my_rf_cv(1))
  expect_error(my_rf_cv("test"))
  expect_error(my_rf_cv(-1))
})
