#' Make Ising chains from (a series of) Ising grid(s) and perform a
#' chain simulation.
#'
#' First specify what is the network parameter in each time points, then
#' perform a chain simulation based on it. An Ising chain can be generated
#' from one or more Ising grid(s) with one changing condition each.
#'
#' @param Ising_chain An	`Ising_chain` object generated from `make_Ising_chain()`.
#' @inheritParams simulate_Isingland
#' @inheritParams make_2d_Isingland_matrix
#' @return
#' `make_Ising_chain` returns an `Ising_chain` object, which is a tibble, and each row
#' represents a set of parameters for an Ising network.
#'
#' `chain_simulate_Isingland` returns a `chain_sim_Isingland` object,
#' which is a tibble containing the parameters, the landscape, and
#' the number of active nodes for each time step.
#' @export
chain_simulate_Isingland <- function(Ising_chain, transform = FALSE,
                                     initial = 0, beta2 = NULL) {
  output <- Ising_chain %>%
    dplyr::mutate(time = 1:nrow(.)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      landscape = list(make_2d_Isingland(
        thresholds_list, weiadj_list,
        beta_list, transform
      )),
      n_active = NA_integer_
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(beta2 = ifelse(is.null(beta2), unlist(beta_list), beta2))
  output$n_active[1] <- initial
  for (i in 2:nrow(output)) {
    output$n_active[i] <- simulate_next_step(output$landscape[[i]], output$n_active[i - 1], output$beta2[i])
  }

  return(
    structure(
      output,
      class = c("chain_sim_Isingland", class(output))
    )
  )
}

#' @export
#' @rdname chain_simulate_Isingland
#' @param ... Ising grid(s) created by [make_Ising_grid()].
make_Ising_chain <- function(...) {
  Igrids <- list(...)
  if (any(lapply(Igrids, methods::is, class2 = "Ising_grid") == FALSE)) {
    cli::cli_abort("All arguments should be `Ising_grid`s.")
  }
  if (any(lapply(Igrids, \(x) length(attr(x, "par_name"))) > 1)) {
    cli::cli_abort("Each `Ising_grid` should only contain one varying condition.")
  }

  Igrids <- Igrids %>% lapply(
    function(x) {
      x %>% dplyr::select(dplyr::ends_with("list"))
    }
  )

  output <- dplyr::bind_rows(Igrids)
  return(
    structure(
      output,
      class = c("Ising_chain", class(output))
    )
  )
}

#' @export
plot.chain_sim_Isingland <- function(x, ...) {
  d_sim <- x %>%
    dplyr::select(time, n_active)
  d_landscape <- x %>%
    dplyr::select(-n_active) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dist = list(get_dist(landscape))) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(dist)

  d_sim <- d_sim %>%
    dplyr::left_join(d_landscape, by = c("time", "n_active"))

  return(
    ggplot2::ggplot(d_landscape, ggplot2::aes(x = n_active, y = U)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::theme_bw() +
      ggplot2::xlab("Number of active nodes") +
      ggplot2::geom_point(ggplot2::aes(x = n_active, y = U),
        data = d_sim, size = 10, color = "black"
      ) +
      gganimate::transition_time(time) +
      ggplot2::ggtitle("Time: {frame_time}")
  )
}
