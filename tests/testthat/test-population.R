set.seed(45)
hh_gen <- hh_geometric(2L)
pop_test <- generate_population(5L, hh_gen)

test_that("Cannot generate a population with negative size", {
  expect_error(generate_population(-1L, hh_geometric(2L)))
  expect_error(generate_population(-5L, hh_geometric(3L)))
})

test_that("Cannot evolve more individuals than population size", {
  expect_error(evolve(pop_test, 10L))
})

test_that("Cannot remove more individuals than population size", {
  expect_error(remove(pop_test, 10L))
})
