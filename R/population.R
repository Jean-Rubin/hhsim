#' Generate a population
#'
#'
#'
#'
#'
#'
#'
#'
#' @export
generate_population <- function(n_ind, size_hh) {
  tibble(
    ind = factor(seq_len(n_ind)),
    new_hh = runif(n_ind) < (1 / size_hh),
    hh = factor(cumsum(lag(new_hh, default = TRUE)))
  ) |>
    select(-new_hh)
}

#' @export
introduce <- function(population, n, size_hh) {
  max_ind <- population |> pull(ind) |> as.integer() |> max()
  max_hh <- population |> pull(hh) |> as.integer() |> max()
  new_population <- tibble(
    ind = factor(max_ind + seq_len(n)),
    new_hh = runif(n) < 1 / size_hh,
    hh = factor(max_hh + cumsum(lag(new_hh, default = TRUE)))
  ) |>
  select(-new_hh)

  population |> bind_rows(new_population)
}

#' @export
evolve <- function(population, n) {
  n_ind <- nrow(population)
  stopifnot(
    "n must be less than population size" = n <= n_ind
  )

  affectations <- population |>
    pull(hh) |>
    sample(n, replace = TRUE)

  changing_hh <- population |>
    slice_sample(n = n) |>
    mutate(hh = affectations)

  population |>
    left_join(changing_hh, by = "ind") |>
    mutate(hh = coalesce(hh.y, hh.x)) |>
    select(-hh.x, -hh.y)
}

#' @export
exit <- function(population, n) {
  n_ind <- nrow(population)
  stopifnot(
    "n must be less than population size" = n <= n_ind
  )
  
  moving_out <- population |>
    slice_sample(n = n) |>
    select(ind)

  population |>
    anti_join(moving_out, by = "ind")
}
