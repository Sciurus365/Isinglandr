simulate_next_step <- function(l, x, beta2) {
  probs <- purrr::map_dbl(c(x - 1, x, x + 1), ~ exp(-beta2 * U(l, .x)))
  sample(c(x - 1, x, x + 1), 1, prob = probs)
}

U <- function(l, x) {
  if (x < 0 | x >= nrow(get_dist(l))) {
    return(Inf)
  }
  return(as.numeric(get_dist(l)[x + 1, "U"]))
}

#' Simulate an 2D Ising landscape
#'
#' Perform a numeric simulation using a landscape. The simulation is
#' based on the Glauber dynamics, which means the transition possibility
#' is determined by the difference in energy.
#' Note that, the conditional transition possibility of this simulation
#' is different from that using the original Ising model. However, the
#' steady-state distribution is preserved.
#' You can choose to simulate the state of a single system
#' stochastically or simulate the distribution of the states.
#'
#' @param l An `Isingland` object constructed with [make_2d_Isingland()] or [make_2d_Isingland_matrix()].
#' @param mode One of `"single"`, `"distribution"`. Do you want to simulate
#' the state of a single system stochastically or simulate the distribution
#' of the states? `"single"` is used by default.
#' @param initial An integer indicating the initial number
#' of active nodes for the simulation. Float numbers will be
#' converted to an integer automatically.
#' @param length An integer indicating the simulation length.
#' @param beta2 The \eqn{beta} value used for simulation. By default use
#' the same value as for landscape construction. Manually setting this
#' value can make the system easier or more difficult to transition
#' to another state, but will alter the steady-state distribution as well.
#' @param ... Not in use.
#'
#' @return A `sim_Isingland` object with the following components:
#' \itemize{
#'  \item `output` A tibble of the simulation output.
#'  \item `landscape` The landscape object supplied to this function.
#'  \item `mode` A character representing the mode of the simulation.
#' }
#' @export
simulate_Isingland <- function(l, ...) {
  UseMethod("simulate_Isingland", l)
}

#' @export
#' @rdname simulate_Isingland
simulate_Isingland.2d_Isingland <- function(l, mode = "single",
                                            initial = 0, length = 100, beta2 = l$beta, ...) {
  mode <- pmatch(mode, c("single", "distribution"))
  if (is.na(mode)) abort_bad_argument("mode", "be one of 'single', 'distribution'.")
  if (!is.numeric(initial)) abort_bad_argument("initial", "be a numeric value.")
  if (mode == 1) {
    output <- tibble::tibble(
      time = 1:length,
      n_active = vector("integer", length)
    )

    if (is.numeric(initial)) output$n_active[1] <- as.integer(initial)

    for (i in 2:length) {
      output$n_active[i] <- simulate_next_step(l, output$n_active[i - 1], beta2)
    }
  } else if (mode == 2) {
    output <- tibble::tibble(time = 1:length) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(n_active = list(0:l$Nvar)) %>%
      dplyr::ungroup()

    density_list <- vector("list", length)

    if (is.numeric(initial)) {
      initial_density <- rep(0, l$Nvar + 1)
      initial_density[initial + 1] <- 1
      density_list[[1]] <- initial_density
    }

    trans_mat <- matrix(0, l$Nvar + 1, l$Nvar + 1)
    for (i in 0:(l$Nvar)) {
      if (i == 0) {
        trans_direction <- c(0, 1)
      } else if (i == l$Nvar) {
        trans_direction <- c(-1, 0)
      } else {
        trans_direction <- c(-1, 0, 1)
      }

      probs <- purrr::map_dbl(trans_direction, ~ exp(-beta2 * U(l, i + .x)))
      probs <- probs / sum(probs)
      for (j in 1:length(trans_direction)) {
        trans_mat[i + 1, i + 1 + trans_direction[j]] <- probs[j]
      }
    }
    for (i in 2:length) {
      density_list[[i]] <- t(trans_mat) %*% density_list[[i - 1]] %>% as.vector()
    }

    output$density <- density_list

    output <- tidyr::unnest(output, cols = c(n_active, density))
  }

  return(
    structure(
      list(output = output, landscape = l, mode = c("single", "distribution")[mode]),
      class = c("sim_2d_Isingland", "sim_Isingland")
    )
  )
}

#' @rdname simulate_Isingland
#' @export
simulate_Isingland.2d_Isingland_matrix <- function(l, mode = "single",
                                                   initial = 0, length = 100, beta2 = NULL, ...) {
  output <- l$dist_raw
  output <- output %>%
    dplyr::rowwise() %>%
    dplyr::mutate(beta2 = ifelse(is.null(.env$beta2), beta_list, beta2)) %>%
    dplyr::mutate(sim = list(simulate_Isingland(l = landscape, mode = mode, initial = initial, length = length, beta2 = .data$beta2))) %>%
    dplyr::mutate(sim_output = list(sim$output)) %>%
    dplyr::ungroup()

  mode <- output$sim[[1]]$mode
  output <- output %>%
    dplyr::select(-c(landscape, sim, dist)) %>%
    tidyr::unnest(sim_output)

  return(return(
    structure(
      list(output = output, landscape = l, mode = mode),
      class = c("sim_2d_Isingland_matrix", "sim_Isingland")
    )
  ))
}


#' @export
print.sim_Isingland <- function(x, ...) {
  print(x$output)
}

#' @export
plot.sim_2d_Isingland <- function(x, ...) {
  tmax <- max(x$output$time)

  d <- x$output %>%
    dplyr::left_join(x$landscape$dist, by = "n_active")

  if (x$mode == "single") {
    return(
      plot.2d_Isingland(x$landscape) +
        ggplot2::geom_point(ggplot2::aes(x = n_active, y = U),
          data = d, size = 10, color = "black"
        ) +
        gganimate::transition_time(time) +
        ggplot2::ggtitle("Time: {frame_time}")
    )
  } else if (x$mode == "distribution") {
    return(
      plot.2d_Isingland(x$landscape) +
        ggplot2::geom_point(ggplot2::aes(
          x = n_active, y = U,
          alpha = density
        ),
        data = d, color = "black", size = 10
        ) +
        ggplot2::scale_alpha_continuous(range = c(0, 1)) +
        gganimate::transition_time(time) +
        ggplot2::ggtitle("Time: {frame_time}")
    )
  }
}


#' @export
plot.sim_2d_Isingland_matrix <- function(x, ...) {
  tmax <- max(x$output$time)

  d <- x$output %>%
    dplyr::left_join(x$landscape$dist, by = c("n_active", attr(x$landscape, "par_name")))

  if (x$mode == "single") {
    return(
      plot.2d_Isingland_matrix(x$landscape) +
        ggplot2::geom_point(ggplot2::aes(x = n_active, y = U),
          data = d, size = 10, color = "black"
        ) +
        gganimate::transition_time(time) +
        ggplot2::ggtitle("Time: {frame_time}")
    )
  } else if (x$mode == "distribution") {
    return(
      plot.2d_Isingland_matrix(x$landscape) +
        ggplot2::geom_point(ggplot2::aes(
          x = n_active, y = U,
          alpha = density
        ),
        data = d, color = "black", size = 10
        ) +
        ggplot2::scale_alpha_continuous(range = c(0, 1)) +
        gganimate::transition_time(time) +
        ggplot2::ggtitle("Time: {frame_time}")
    )
  }
}
