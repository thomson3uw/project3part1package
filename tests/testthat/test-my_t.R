my_data <- stats::na.omit(project3part1package::my_penguins)
my_data2 <- stats::na.omit(project3part1package::my_gapminder)

test_that("Equal outputs, two.sided", {
  expect_equal(my_t.test(x = my_data$bill_depth_mm, alternative = "two.sided", mu = 20)$test_stat,
               as.numeric(t.test(x = my_data$bill_depth_mm, alternative = "two.sided", mu = 20)$statistic))
  expect_equal(my_t.test(x = my_data$bill_depth_mm, alternative = "two.sided", mu = 20)$p_val,
               as.numeric(t.test(x = my_data$bill_depth_mm, alternative = "two.sided", mu = 20)$p.value))
  expect_equal(my_t.test(x = my_data$bill_depth_mm, alternative = "two.sided", mu = 20)$alternative,
               t.test(x = my_data$bill_depth_mm, alternative = "two.sided", mu = 20)$alternative)
  expect_equal(my_t.test(x = my_data$bill_depth_mm, alternative = "two.sided", mu = 20)$df,
               as.numeric(t.test(x = my_data$bill_depth_mm, alternative = "two.sided", mu = 20)$parameter))
})

test_that("Equal outputs, default alternative", {
  expect_equal(my_t.test(x = my_data$bill_depth_mm, mu = 20)$test_stat,
               as.numeric(t.test(x = my_data$bill_depth_mm, alternative = "two.sided", mu = 20)$statistic))
  expect_equal(my_t.test(x = my_data$bill_depth_mm, mu = 20)$p_val,
               as.numeric(t.test(x = my_data$bill_depth_mm, alternative = "two.sided", mu = 20)$p.value))
  expect_equal(my_t.test(x = my_data$bill_depth_mm, mu = 20)$alternative,
               t.test(x = my_data$bill_depth_mm, alternative = "two.sided", mu = 20)$alternative)
  expect_equal(my_t.test(x = my_data$bill_depth_mm, mu = 20)$df,
               as.numeric(t.test(x = my_data$bill_depth_mm, alternative = "two.sided", mu = 20)$parameter))
})

test_that("Equal outputs, two.sided, new data", {
  expect_equal(my_t.test(x = my_data2$pop, alternative = "two.sided", mu = 30000000)$test_stat,
               as.numeric(t.test(x = my_data2$pop, alternative = "two.sided", mu = 30000000)$statistic))
  expect_equal(my_t.test(x = my_data2$pop, alternative = "two.sided", mu = 30000000)$p_val,
               as.numeric(t.test(x = my_data2$pop, alternative = "two.sided", mu = 30000000)$p.value))
  expect_equal(my_t.test(x = my_data2$pop, alternative = "two.sided", mu = 30000000)$alternative,
               t.test(x = my_data2$pop, alternative = "two.sided", mu = 30000000)$alternative)
  expect_equal(my_t.test(x = my_data2$pop, alternative = "two.sided", mu = 30000000)$df,
               as.numeric(t.test(x = my_data2$pop, alternative = "two.sided", mu = 30000000)$parameter))
})

test_that("Equal outputs, greater", {
  expect_equal(my_t.test(x = my_data$bill_depth_mm, alternative = "greater", mu = 20)$test_stat,
               as.numeric(t.test(x = my_data$bill_depth_mm, alternative = "greater", mu = 20)$statistic))
  expect_equal(my_t.test(x = my_data$bill_depth_mm, alternative = "greater", mu = 20)$p_val,
               as.numeric(t.test(x = my_data$bill_depth_mm, alternative = "greater", mu = 20)$p.value))
  expect_equal(my_t.test(x = my_data$bill_depth_mm, alternative = "greater", mu = 20)$alternative,
               t.test(x = my_data$bill_depth_mm, alternative = "greater", mu = 20)$alternative)
  expect_equal(my_t.test(x = my_data$bill_depth_mm, alternative = "greater", mu = 20)$df,
               as.numeric(t.test(x = my_data$bill_depth_mm, alternative = "greater", mu = 20)$parameter))
})

test_that("Equal outputs, less", {
  expect_equal(my_t.test(x = my_data$bill_depth_mm, alternative = "less", mu = 20)$test_stat,
               as.numeric(t.test(x = my_data$bill_depth_mm, alternative = "less", mu = 20)$statistic))
  expect_equal(my_t.test(x = my_data$bill_depth_mm, alternative = "less", mu = 20)$p_val,
               as.numeric(t.test(x = my_data$bill_depth_mm, alternative = "less", mu = 20)$p.value))
  expect_equal(my_t.test(x = my_data$bill_depth_mm, alternative = "less", mu = 20)$alternative,
               t.test(x = my_data$bill_depth_mm, alternative = "less", mu = 20)$alternative)
  expect_equal(my_t.test(x = my_data$bill_depth_mm, alternative = "less", mu = 20)$df,
               as.numeric(t.test(x = my_data$bill_depth_mm, alternative = "less", mu = 20)$parameter))
})

test_that("Function outputs a list", {
  expect_type(my_t.test(x = my_data$bill_depth_mm, alternative = "less", mu = 20), "list")
})

test_that("non-numeric mu input throws an error", {
  expect_error(my_t.test(x = my_data$bill_depth_mm, alternative = "less", mu = "test"))
})

test_that("incorrect alternative type input throws an error", {
  expect_error(my_t.test(x = my_data$bill_depth_mm, alternative = 20, mu = 20))
})

test_that("incorrect alternative string input throws an error", {
  expect_error(my_t.test(x = my_data$bill_depth_mm, alternative = "not an alternative", mu = 20))
})

test_that("incorrect alternative input throws an error", {
  expect_error(my_t.test(x = data.frame(c("a", "b", "c")), alternative = 20, mu = 20))
})

