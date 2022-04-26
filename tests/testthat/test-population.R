set.seed(45)
pop_test <- generate_population(5, 4)

test_that("Cannot generate a population with negative size", {
  expect_error(generate_population(-1, 10))
  expect_error(generate_population(-5, 1))
})

test_that("Cannot evolve more individuals than population size", {
  expect_error(evolve(pop_test, 10))
})

test_that("Cannot remove more individuals than population size", {
  expect_error(remove(pop_test, 10))
})
