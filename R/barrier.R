#' Calculate energy barrier for Ising landscapes
#'
#' @inheritParams simulate_Isingland
#' @return A `barrier_Isingland` object that contains the following components:
#' \itemize{
#'   \item `shape` A character describing the shape of the landscape.
#'   \item `local_min_start`,`local_min_end`,`saddle_point` The positions of the
#'   two local minimums and the saddle point, described each by a list containing:
#'   \itemize{
#'       \item `U` The potential value.
#'       \item `location`
#'       \itemize{
#'           \item `x_index` The row index in `get_dist(l)`.
#'           \item `x_value` The number of active nodes.
#'       }
#'   }
#'   \item `delta_U_start`,`delta_U_end` The barrier heights for both sides.
#' }
#' @name calculate_barrier.Isingland
NULL

#' @export
#' @rdname calculate_barrier.Isingland
calculate_barrier.2d_Isingland <- function(l){
	d <- get_dist(l)
	minindex <- local_min_index(d$U)
	maxindex <- local_max_index(d$U)

	for(i in 1:ncol(landscape_shapes)) {
		if(
			length(minindex) == landscape_shapes$Nmin[i] &
			length(maxindex) == landscape_shapes$Nmax[i] &
			is.unsorted(eval(landscape_shapes$order[i])) == FALSE
		){
			output <- list(
				shape = landscape_shapes$shape[i],
				local_min_start = make_point(d, eval(landscape_shapes$start[[i]])),
				local_min_end = make_point(d, eval(landscape_shapes$end[[i]])),
				saddle_point = make_point(d, eval(landscape_shapes$saddle[[i]]))
			)
			output$delta_U_start <- output$saddle_point$U - output$local_min_start$U
			output$delta_U_end <- output$saddle_point$U - output$local_min_end$U
			return(
				structure(
					output,
					class = c("barrier_2d_Isingland", "barrier_Isingland", "barrier")
				)
			)
		}
	}

	rlang::abort("The shape of the landscape is not supported for calculating barrier.")
}

#' @export
print.barrier_2d_Isingland <- function(x, ...) {
	glue::glue(
		"A landscape with shape {x$shape}
		delta_U_start = {format(x$delta_U_start, digits = 2)}
		delta_U_end = {format(x$delta_U_end, digits = 2)}
		"
	)
}

#' @export
#' @describeIn calculate_barrier.Isingland Return a vector of
#' barrier heights.
#' @inheritParams base::summary
summary.barrier_2d_Isingland <- function(object, ...){
	c(delta_U_start = object$delta_U_start, delta_U_end = object$delta_U_end)
}

