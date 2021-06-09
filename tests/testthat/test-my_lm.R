my_data <- stats::na.omit(project3part1package::my_gapminder)

test_that("Function output has the correct type", {
  expect_type(my_lm(lifeExp ~ gdpPercap + continent, data = my_data), "double")
})

test_that("Function output has the correct dimension", {
  expect_equal(length(my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)[, 1]), 6)
  expect_equal(length(my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)[1, ]), 4)
})

test_that("Invalid formula throws an error", {
  expect_error(my_lm(lifeExp ~ gdpPercap + continent + notavar, data = my_data))
  expect_error(my_lm(formula = "string", data = my_data))
})

test_that("Invalid data throws an error", {
  expect_error(my_lm(lifeExp ~ gdpPercap + continent, data = "some data"))
})
