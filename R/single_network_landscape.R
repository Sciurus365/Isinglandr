#' Make a 2D landscape for an Ising network
#'
#' @param Ising_network An `IsingFit` object from [IsingFit::IsingFit()].
#' @param thresholds Thresholds of the variables.
#' @param weiadj The weighted adjacency matrix.
#' @param beta The \eqn{\beta} value for calculating the Hamiltonian.
#' @param transform By default, this function considers the Ising network
#' to use `-1` and `1` for two states. Set `transform = TRUE` if the Ising
#' network uses `0` and `1` for two states.
#'
#' @return A `2d_Isingland` object that contains the result.
#' @export
make_2d_Isingland <- function(thresholds, weiadj, beta = 1, transform = FALSE) {
  Nvar <- length(thresholds)

  # transformation
  if (transform) {
    vector.one <- matrix(1, Nvar, 1)
    thresholds <- 0.5 * thresholds + 0.25 * weiadj %*% vector.one
    weiadj <- 0.25 * weiadj
  }

  ## generate all states
  d <- tibble::tibble(v = all_combination(c(-1, 1), len = Nvar))

  ## calculate their H (or U)
  d <- d %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      U = as.numeric(ham(v, thresholds, weiadj)),
      sum_state = sum(v),
      freq = exp(-beta * U),
      n_active = (sum_state + Nvar) / 2
    ) %>%
    dplyr::ungroup()

  ## summarize based on the number of symptoms
  d_sum <- d %>%
    dplyr::group_by(sum_state) %>%
    dplyr::summarize(sum_freq = sum(freq)) %>%
    dplyr::mutate(
      n_active = (sum_state + Nvar) / 2,
      U = -log(sum_freq) / beta
    )

  result <- list(dist_raw = d, dist = d_sum)
  class(result) <- c("2d_Isingland", "Isingland", "landscape")
  return(result)
}

#' @export
plot.2d_Isingland <- function(x, ...) {
  ggplot2::ggplot(data = x$dist, ggplot2::aes(x = n_active, y = U)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::xlab("Number of active nodes")
}

#' @export
print.landscape <- function(x, ...){
	print(get_dist(x))
}
