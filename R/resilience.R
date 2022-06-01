#' Calculate the resilience values for Ising landscapes
#'
#' The resilience is calculated based on the shape of the potential landscape and the prior knowledge about the qualitatively different parts of the system. Two resilience indicators are calculated separately, and their difference is used to represent a general resilience of the system in favor of the first phase. Within each phase, the potential difference between the local maximum and the local minimum (if multiple minimums exist, use the one that is further from the other phase; and the local maximum should always be on the side to the other phase) is used to represent the resilience of this phase.
#'
#' @inheritParams simulate_Isingland
#'
#' @return
#' \describe{
#' \item{[calculate_resilience.2d_Isingland()]}{Returns a `calculate_resilience.2d_Isingland` project, which contains the following elements:
#' \describe{
#' \item{dist}{The distribution tibble which is the same as in the input `l`.}
#' \item{effective_minindex1,effective_maxindex1,effective_minindex2,effective_maxindex2}{The (row)indices in `dist` that were used as the positions of the local minimums and maximums in two parts.}
#' \item{resilience1,resilience2,resilience_diff}{The resilience measures for the first (left) part, the second part (right), and their difference.}
#' }
#' }
#' \item{[calculate_resilience.2d_Isingland_matrix()]}{Returns a `resilience_2d_Isingland_matrix` object, which is a tibble containing columns of the varying parameters and a column `resilience` of the `calculate_resilience.2d_Isingland` objects for each landscape.}
#' }
#'
#' When `print()`ed, a verbal description of the resilience metrics is shown. Use the `summary()` method for a tidy version of the outputs.
#' @export
calculate_resilience <- function(l, ...) {
  UseMethod("calculate_resilience", l)
}

#' @param split_value An integer to specify the number of active nodes used to split two resilience ranges. Default is half of the number of nodes.
#' @export
#' @rdname calculate_resilience
calculate_resilience.2d_Isingland <- function(l, split_value = 0.5*l$Nvar, ...) {
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
  resilience1 <- d1$U[effective_maxindex1] - d1$U[effective_minindex1]

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
  resilience2 <- d2$U[effective_maxindex2] - d2$U[effective_minindex2]

  effective_minindex2 <- effective_minindex2 + nrow(d %>% dplyr::filter(n_active < split_value))
  effective_maxindex2 <- effective_maxindex2 + nrow(d %>% dplyr::filter(n_active < split_value))

  return(
    structure(
      list(
        dist = d,
        effective_minindex1 = effective_minindex1,
        effective_maxindex1 = effective_maxindex1,
        effective_minindex2 = effective_minindex2,
        effective_maxindex2 = effective_maxindex2,
        resilience1 = resilience1,
        resilience2 = resilience2,
        resilience_diff = resilience1 - resilience2
      ),
      class = c("resilience_2d_Isingland", "resilience_Isingland", "resilience")
    )
  )
}

#' @export
#' @rdname calculate_resilience
calculate_resilience.2d_Isingland_matrix <- function(l, split_value = 0.5*l$Nvar, ...) {
  d_raw <- l$dist_raw
  d_raw <- d_raw %>%
    dplyr::rowwise() %>%
    dplyr::mutate(resilience = list(calculate_resilience(landscape, split_value = split_value))) %>%
    dplyr::ungroup()
  d <- d_raw %>%
    dplyr::select(dplyr::all_of(attr(l, "par_name")), resilience)
  return(structure(
    d,
    class = c("resilience_2d_Isingland_matrix", class(d))
  ))
}

#' @export
#' @inheritParams print.barrier_2d_Isingland
print.resilience_2d_Isingland <- function(x, simplify = FALSE, ...) {
  if (simplify) {
    print(glue::glue(
      "resilience1 = {format(x$resilience1, digits = 2)}; resilience2 = {format(x$resilience2, digits = 2)}; resilience_diff = {format(x$resilience_diff, digits = 2)}"
    ))
  } else {
    print(glue::glue(
      "
		resilience1 = {format(x$resilience1, digits = 2)}
		(local minimum at n_active = {as.integer(x$dist$n_active[x$effective_minindex1])}; local maximum at n_active = {as.integer(x$dist$n_active[x$effective_maxindex1])})
		resilience2 = {format(x$resilience2, digits = 2)}
		(local minimum at n_active = {as.integer(x$dist$n_active[x$effective_minindex2])}; local maximum at n_active = {as.integer(x$dist$n_active[x$effective_maxindex2])})
		resilience_diff = {format(x$resilience_diff, digits = 2)}
		"
    ))
  }
}

#' @export
print.resilience_2d_Isingland_matrix <- function(x, ...) {
  x <- x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(resilience = purrr::quietly(print)(resilience, simplify = TRUE)$result) %>%
    dplyr::ungroup()
  print(x)
}


#' @export
#' @inheritParams base::summary
summary.resilience_2d_Isingland <- function(object, ...) {
  c(
    resilience1 = object$resilience1, resilience2 = object$resilience2,
    resilience_diff = object$resilience_diff
  )
}

#' @export
#' @inheritParams base::summary
summary.resilience_2d_Isingland_matrix <- function(object, ...) {
	object %>%
		dplyr::rowwise() %>%
		dplyr::mutate(resilience_measures = list(summary(resilience)),
									resilience1 = resilience_measures["resilience1"],
									resilience2 = resilience_measures["resilience2"],
									resilience_diff = resilience_measures["resilience_diff"]) %>%
		dplyr::ungroup() %>%
		dplyr::select(-resilience, -resilience_measures)
}
