#' Calculate the stability metrics for Ising landscapes
#'
#' The stability is calculated based on the shape of the potential landscape and the prior knowledge about the qualitatively different parts of the system. Two stability indicators are calculated separately, and their difference is used to represent a general stability of the system in favor of the first phase. Within each phase, the potential difference between the local maximum and the local minimum (if multiple minimums exist, use the one that is further from the other phase; and the local maximum should always be on the side to the other phase) is used to represent the stability of this phase.
#'
#' @inheritParams simulate_Isingland
#'
#' @return
#' \describe{
#' \item{[calculate_stability.2d_Isingland()]}{Returns a `calculate_stability.2d_Isingland` project, which contains the following elements:
#' \describe{
#' \item{dist}{The distribution tibble which is the same as in the input `l`.}
#' \item{effective_minindex1,effective_maxindex1,effective_minindex2,effective_maxindex2}{The (row)indices in `dist` that were used as the positions of the local minimums and maximums in two parts.}
#' \item{stability1,stability2,stability_diff}{The stability measures for the first (left) part, the second part (right), and their difference.}
#' }
#' }
#' \item{[calculate_stability.2d_Isingland_matrix()]}{Returns a `stability_2d_Isingland_matrix` object, which is a tibble containing columns of the varying parameters and a column `stability` of the `calculate_stability.2d_Isingland` objects for each landscape.}
#' }
#'
#' When `print()`ed, a verbal description of the stability metrics is shown. Use the `summary()` method for a tidy version of the outputs.
#' @export
calculate_stability <- function(l, ...) {
  UseMethod("calculate_stability", l)
}

#' @param split_value An integer to specify the number of active nodes used to split two stability ranges. Default is half of the number of nodes.
#' @export
#' @rdname calculate_stability
calculate_stability.2d_Isingland <- function(l, split_value = 0.5 * l$Nvar, ...) {
  d <- get_dist(l)

  # split the data into two parts
  d1 <- d %>% dplyr::filter(n_active <= split_value)
  d2 <- d %>% dplyr::filter(n_active >= split_value)

  # for the first part
  minindex1 <- local_min_index(d1$U)
  maxindex1 <- local_max_index(d1$U)
  effective_minindex1 <- minindex1[1]
  effective_maxindex1 <- maxindex1[maxindex1 > effective_minindex1] # find the first local maximum (include margin) on the right side of the leftmost local minimum
  if (length(effective_maxindex1) == 0) {
    effective_maxindex1 <- effective_minindex1
  } else {
    effective_maxindex1 <- effective_maxindex1[1]
  }
  stability1 <- d1$U[effective_maxindex1] - d1$U[effective_minindex1]

  # for the second part
  minindex2 <- local_min_index(d2$U)
  maxindex2 <- local_max_index(d2$U)
  effective_minindex2 <- minindex2[length(minindex2)]
  effective_maxindex2 <- maxindex2[maxindex2 < effective_minindex2] # find the first local maximum (include margin) on the left side of the rightmost local minimum
  if (length(effective_maxindex2) == 0) {
    effective_maxindex2 <- effective_minindex2
  } else {
    effective_maxindex2 <- effective_maxindex2[length(effective_maxindex2)]
  }
  stability2 <- d2$U[effective_maxindex2] - d2$U[effective_minindex2]

  effective_minindex2 <- effective_minindex2 + nrow(d %>% dplyr::filter(n_active < split_value))
  effective_maxindex2 <- effective_maxindex2 + nrow(d %>% dplyr::filter(n_active < split_value))

  return(
    structure(
      list(
        dist = d,
        split_value = split_value,
        effective_minindex1 = effective_minindex1,
        effective_maxindex1 = effective_maxindex1,
        effective_minindex2 = effective_minindex2,
        effective_maxindex2 = effective_maxindex2,
        stability1 = stability1,
        stability2 = stability2,
        stability_diff = stability1 - stability2
      ),
      class = c("stability_2d_Isingland", "stability_Isingland", "stability")
    )
  )
}


#' Get ggplot2 layers of stability metrics to add to the landscape plots
#'
#' Those layers can show how the stability metrics are calculated on the landscape.
#'
#' @param object A `stability` object calculated by [calculate_stability()] or [calculate_stability.2d_Isingland()].
#' @param point,line,split_value,interval,stability_value Show those elements on the layer? Default is `TRUE` for all of them.
#' @param ... Not in use.
#'
#' @export
#'
#' @inherit ggplot2::autolayer return
#' @name autolayer.stability
autolayer.stability_2d_Isingland <- function(object, point = TRUE, line = TRUE, split_value = TRUE, interval = TRUE, stability_value = TRUE, ...) {
  result <- list()
  if (point) {
    result <- append(
      result,
      ggplot2::geom_point(data = with(object, dist[c(effective_minindex1, effective_maxindex1), ]), ggplot2::aes(x = n_active, y = U), size = 2, color = "red")
    )
    result <- append(
      result,
      ggplot2::geom_point(data = with(object, dist[c(effective_minindex2, effective_maxindex2), ]), ggplot2::aes(x = n_active, y = U), size = 2, color = "blue")
    )
  }
  if (line) {
    result <- append(
      result,
      ggplot2::geom_path(data = tibble::tibble(
        x = with(object, dist[c(effective_minindex1:effective_maxindex1), "n_active"]) %>% unlist(),
        y = with(object, dist[c(effective_minindex1:effective_maxindex1), "U"]) %>% unlist()
      ), ggplot2::aes(x = x, y = y), size = 2, alpha = 0.3, color = "red")
    )

    result <- append(
      result,
      ggplot2::geom_path(data = tibble::tibble(
        x = with(object, dist[c(effective_maxindex2:effective_minindex2), "n_active"]) %>% unlist(),
        y = with(object, dist[c(effective_maxindex2:effective_minindex2), "U"]) %>% unlist()
      ), ggplot2::aes(x = x, y = y), size = 2, alpha = 0.3, color = "blue")
    )
  }
  if (split_value) {
    result <- append(
      result,
      ggplot2::geom_vline(xintercept = object$split_value, linetype = 2)
    )
  }
  if (interval) {
    result <- append(
      result,
      ggplot2::geom_errorbar(data = tibble::tribble(
        ~x, ~ymin, ~ymax,
        unlist(with(object, dist[effective_minindex1, "n_active"])), unlist(with(object, dist[effective_minindex1, "U"])), unlist(with(object, dist[effective_maxindex1, "U"]))
      ), ggplot2::aes(x = x, y = NULL, ymin = ymin, ymax = ymax), color = "red", width = 0.2)
    )
    result <- append(
      result,
      ggplot2::geom_errorbar(data = tibble::tribble(
        ~x, ~ymin, ~ymax,
        unlist(with(object, dist[effective_minindex2, "n_active"])), unlist(with(object, dist[effective_minindex2, "U"])), unlist(with(object, dist[effective_maxindex2, "U"]))
      ), ggplot2::aes(x = x, y = NULL, ymin = ymin, ymax = ymax), color = "blue", width = 0.2)
    )
  }

  if (stability_value) {
    result <- append(
      result,
      ggplot2::annotate("text",
        x = unlist(with(object, dist[effective_minindex1, "n_active"])) - 0.5,
        y = unlist(with(object, dist[effective_maxindex1, "U"])),
        label = sprintf("%.2f", object$stability1),
        color = "red"
      )
    )
    result <- append(
      result,
      ggplot2::annotate("text",
        x = unlist(with(object, dist[effective_minindex2, "n_active"])) + 0.5,
        y = unlist(with(object, dist[effective_maxindex2, "U"])),
        label = sprintf("%.2f", object$stability2),
        color = "blue"
      )
    )
  }
  return(result)
}


#' @export
#' @rdname calculate_stability
calculate_stability.2d_Isingland_matrix <- function(l, split_value = 0.5 * l$Nvar, ...) {
  d_raw <- l$dist_raw
  d_raw <- d_raw %>%
    dplyr::rowwise() %>%
    dplyr::mutate(stability = list(calculate_stability(landscape, split_value = split_value))) %>%
    dplyr::ungroup()
  d <- d_raw %>%
    dplyr::select(dplyr::all_of(attr(l, "par_name")), stability)
  return(structure(
    list(dist = d, dist_raw = d_raw, split_value = split_value),
    class = c("stability_2d_Isingland_matrix")
  ))
}


#' @rdname autolayer.stability
#' @export
autolayer.stability_2d_Isingland_matrix <- function(object, point = TRUE, line = TRUE, split_value = TRUE, interval = TRUE, stability_value = TRUE, ...) {
  d_raw <- object$dist_raw %>%
    dplyr::mutate(stability_unlisted = purrr::map(stability, function(x) {
      x$dist <- NULL
      class(x) <- "list"
      tibble::as_tibble(x)
    })) %>%
    tidyr::unnest(stability_unlisted)

  result <- list()
  if (point) {
    result <- append(
      result,
      ggplot2::geom_point(
        data = d_raw %>%
          dplyr::rowwise() %>%
          dplyr::mutate(dist = list(dist[c(effective_minindex1, effective_maxindex1), ])) %>%
          tidyr::unnest(dist) %>%
          dplyr::ungroup(),
        ggplot2::aes(x = n_active, y = U), size = 2, color = "red"
      )
    )
    result <- append(
      result,
      ggplot2::geom_point(
        data = d_raw %>%
          dplyr::rowwise() %>%
          dplyr::mutate(dist = list(dist[c(effective_minindex2, effective_maxindex2), ])) %>%
          tidyr::unnest(dist) %>%
          dplyr::ungroup(),
        ggplot2::aes(x = n_active, y = U), size = 2, color = "blue"
      )
    )
  }
  if (line) {
    result <- append(
      result,
      ggplot2::geom_path(
        data = d_raw %>%
          dplyr::rowwise() %>%
          dplyr::mutate(dist = list(dist[c(effective_maxindex1:effective_minindex1), c("n_active", "U")])) %>%
          tidyr::unnest(dist) %>%
          dplyr::ungroup(),
        ggplot2::aes(x = n_active, y = U), size = 2, alpha = 0.3, color = "red"
      )
    )

    result <- append(
      result,
      ggplot2::geom_path(
        data = d_raw %>%
          dplyr::rowwise() %>%
          dplyr::mutate(dist = list(dist[c(effective_maxindex2:effective_minindex2), c("n_active", "U")])) %>%
          tidyr::unnest(dist) %>%
          dplyr::ungroup(),
        ggplot2::aes(x = n_active, y = U), size = 2, alpha = 0.3, color = "blue"
      )
    )
  }
  if (split_value) {
    result <- append(
      result,
      ggplot2::geom_vline(xintercept = object$split_value, linetype = 2)
    )
  }
  if (interval) {
    result <- append(
      result,
      ggplot2::geom_errorbar(
        data =
          d_raw %>%
            dplyr::rowwise() %>%
            dplyr::mutate(new_dist = list(tibble::tribble(
              ~x, ~ymin, ~ymax,
              unlist(dist[effective_minindex1, "n_active"]), unlist(dist[effective_minindex1, "U"]), unlist(dist[effective_maxindex1, "U"])
            ))) %>%
            tidyr::unnest(new_dist) %>%
            dplyr::ungroup(),
        ggplot2::aes(x = x, y = NULL, ymin = ymin, ymax = ymax), color = "red", width = 0.2
      )
    )
    result <- append(
      result,
      ggplot2::geom_errorbar(
        data =
          d_raw %>%
            dplyr::rowwise() %>%
            dplyr::mutate(new_dist = list(tibble::tribble(
              ~x, ~ymin, ~ymax,
              unlist(dist[effective_minindex2, "n_active"]), unlist(dist[effective_minindex2, "U"]), unlist(dist[effective_maxindex2, "U"])
            ))) %>%
            tidyr::unnest(new_dist) %>%
            dplyr::ungroup(),
        ggplot2::aes(x = x, y = NULL, ymin = ymin, ymax = ymax), color = "blue", width = 0.2
      )
    )
  }

  if (stability_value) {
    result <- append(
      result,
      ggplot2::geom_text(
        data = d_raw %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            x = unlist(dist[effective_minindex1, "n_active"]) - 0.5,
            y = unlist(dist[effective_maxindex1, "U"])
          ) %>%
          dplyr::ungroup(),
        ggplot2::aes(x = x, y = y, label = sprintf("%.2f", stability1)), color = "red"
      )
    )


    result <- append(
      result,
      ggplot2::geom_text(
        data = d_raw %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            x = unlist(dist[effective_minindex2, "n_active"]) + 0.5,
            y = unlist(dist[effective_maxindex2, "U"])
          ) %>%
          dplyr::ungroup(),
        ggplot2::aes(x = x, y = y, label = sprintf("%.2f", stability2)), color = "blue"
      )
    )
  }
  return(result)
}


#' @export
#' @inheritParams print.barrier_2d_Isingland
print.stability_2d_Isingland <- function(x, simplify = FALSE, ...) {
  if (simplify) {
    print(glue::glue(
      "stability1 = {format(x$stability1, digits = 2)}; stability2 = {format(x$stability2, digits = 2)}; stability_diff = {format(x$stability_diff, digits = 2)}"
    ))
  } else {
    print(glue::glue(
      "
		stability1 = {format(x$stability1, digits = 2)}
		(local minimum at n_active = {as.integer(x$dist$n_active[x$effective_minindex1])}; local maximum at n_active = {as.integer(x$dist$n_active[x$effective_maxindex1])})
		stability2 = {format(x$stability2, digits = 2)}
		(local minimum at n_active = {as.integer(x$dist$n_active[x$effective_minindex2])}; local maximum at n_active = {as.integer(x$dist$n_active[x$effective_maxindex2])})
		stability_diff = {format(x$stability_diff, digits = 2)}
		"
    ))
  }
}

#' @export
print.stability_2d_Isingland_matrix <- function(x, ...) {
  x <- x$dist %>%
    dplyr::rowwise() %>%
    dplyr::mutate(stability = purrr::quietly(print)(stability, simplify = TRUE)$result) %>%
    dplyr::ungroup()
  print(x)
}


#' @export
#' @inheritParams base::summary
summary.stability_2d_Isingland <- function(object, ...) {
  c(
    stability1 = object$stability1, stability2 = object$stability2,
    stability_diff = object$stability_diff
  )
}

#' @export
#' @inheritParams base::summary
summary.stability_2d_Isingland_matrix <- function(object, ...) {
  object$dist %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      stability_measures = list(summary(stability)),
      stability1 = stability_measures["stability1"],
      stability2 = stability_measures["stability2"],
      stability_diff = stability_measures["stability_diff"]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-stability, -stability_measures)
}
