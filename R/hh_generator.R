#' Construct a basic household generator
#'
#' @param mean_size Average size of households
#'
#' @return A household generator
#'
#' @export
hh_geometric <- function(mean_size) {
  function(init, n) {
    hh <- init + 1 + cumsum(runif(n) < (1 / mean_size))
    list(
      hh = factor(hh),
      hh_max = hh[length(hh)]
    )
  }
}
