#' Generate a new population
#'
#' @param n_ind Number of individuals
#' @param hh_gen A household generator
#'
#' @return A population
#'
#' @export
generate_population <- function(n_ind, hh_gen) {
  hh <- hh_gen(0, n_ind)
  pop <- tibble(
    ind = factor(seq_len(n_ind)),
    hh = hh$hh
  )

  new_population(
    pop = pop,
    ind_max = n_ind,
    hh_max = hh$hh_max
  )
}

#' export
print.population <- function(x, ...) {
  print(x$pop, ...)
  invisible(x)
}

#' Introduce new individuals in a population
#' 
#' @param population Population to transform
#' @param n Number of individuals to introduce
#' @param hh_gen A household generator
#' 
#' @return A population
#' @export
introduce <- function(population, n, hh_gen) {
  new_hh <- hh_gen(population$hh_max, n)
  new_pop <- tibble(
    ind = factor(population$ind_max + seq_len(n)),
    hh = new_hh$hh
  )
  pop <- population$pop |> bind_rows(new_pop)

  new_population(
    pop = pop,
    ind_max = population$ind_max + n,
    hh_max = population$hh_max + new_hh$hh_max
  )
}

#' Let the individuals of a population move around
#' 
#' @param population Population to transform
#' @param n Number of individuals to introduce
#' @param prop Proportion of individuals to remove
#' 
#' @return A population
#' @export
evolve <- function(population, n, prop) {
  check_n_prop(n, prop, nrow(population$pop))

  affectations <- population$pop |>
    slice_sample(n = n, prop = prop, replace = TRUE) |>
    pull(hh)

  changing_hh <- population$pop |>
    slice_sample(n = n, prop = prop) |>
    mutate(hh = affectations)

  pop <- population$pop |>
    left_join(changing_hh, by = "ind") |>
    mutate(hh = coalesce(hh.y, hh.x)) |>
    select(-hh.x, -hh.y)

  new_population(
    pop = pop,
    ind_max = population$ind_max,
    hh_max = population$hh_max
  )
}

#' Remove individuals from a population
#' 
#' @param population Population to transform
#' @param n Number of individuals to remove
#' @param prop Proportion of individuals to remove
#' 
#' @return A population
#' @export
remove <- function(population, n, prop) {
  check_n_prop(n, prop, nrow(population$pop))
  
  moving_out <- population$pop |>
    slice_sample(n = n, prop = prop) |>
    select(ind)

  pop <- population$pop |>
    anti_join(moving_out, by = "ind")

  new_population(
    pop = pop,
    ind_max = population$ind_max,
    hh_max = population$hh_max
  )
}


new_population <- function(pop, ind_max, hh_max) {
  structure(
    list(
      pop = pop,
      ind_max = ind_max,
      hh_max = hh_max
    ),
    class = "population"
  )
}

check_n_prop <- function(n, prop, n_ind) {
  if (!missing(n)) {
    stopifnot(
      "n must be a positive number" = n >= 0,
      "n must be less than population size" = n <= n_ind
    )
  }

  if (!missing(prop)) {
    stopifnot(
      "prop must be greater than 0" = prop >= 0,
      "prop must be less than 1" = prop <= 1
    )
  }
}
