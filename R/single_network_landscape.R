#' Make a 2D landscape for an Ising network
#'
#' The potential value is calculated as the logarithm of the steady-state
#' distribution, which is, in turn, calculated from the Hamiltonian of
#' the Ising network.
#'
#' @param thresholds,weiadj The thresholds and the weighted adjacency matrix
#' of the Ising network. If you have an `IsingFit` object estimated using
#' [IsingFit::IsingFit()], you can find those two parameters in its components
#' (`<IsingFit>$thresholds` and `<IsingFit>$weiadj`).
#' @param beta The \eqn{\beta} value for calculating the Hamiltonian.
#' @param transform By default, this function considers the Ising network
#' to use `-1` and `1` for two states. Set `transform = TRUE` if the Ising
#' network uses `0` and `1` for two states, *which is often the case for the
#' Ising networks estimated using* [IsingFit::IsingFit()].
#'
#' @return A `2d_Isingland` object that contains the following components:
#' \itemize{
#'   \item `dist_raw`,`dist` Two tibbles containing the probability
#'   distribution and the potential values for different states.
#'   \item `thresholds`,`weiadj`,`beta` The parameters supplied to the function.
#'   \item `Nvar` The number of variables (nodes) in the Ising network.
#' }
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

  result <- list(
    dist_raw = d, dist = d_sum, thresholds = thresholds,
    weiadj = weiadj, beta = beta, Nvar = Nvar
  )
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
print.landscape <- function(x, ...) {
  print(get_dist(x))
}
