#' Title
#'
#' @inheritParams simulate_2d_Isingland
#' @return
#' @export
simulate_2d_Isingland_matrix <- function(l, mode = "single",
																				 initial = 0, length = 100, beta2 = NULL){
	output <- l$dist_raw
	output <- output %>%
		dplyr::rowwise() %>%
		dplyr::mutate(beta2 = ifelse(is.null(.env$beta2), beta_list, beta2)) %>%
		dplyr::mutate(sim = list(simulate_2d_Isingland(l = landscape, mode = mode, initial = initial, length = length, beta2 = .data$beta2))) %>%
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
plot.sim_2d_Isingland_matrix <- function(x, ...){
	tmax <- max(x$output$time)

	d <- x$output %>%
		dplyr::left_join(x$landscape$dist, by = c("n_active", attr(x$landscape, "par_name")))

	if (x$mode == "single"){
		return(
			plot.2d_Isingland_matrix(x$landscape) +
				ggplot2::geom_point(ggplot2::aes(x = n_active, y = U),
														data = d, size = 10, color = "black") +
				gganimate::transition_time(time) +
				ggplot2::ggtitle("Time: {frame_time}")
		)
	} else if (x$mode == "distribution"){
		return(
			plot.2d_Isingland_matrix(x$landscape) +
				ggplot2::geom_point(ggplot2::aes(x = n_active, y = U,
																				 alpha = density),
														data = d, color = "black", size = 10) +
				ggplot2::scale_alpha_continuous(range = c(0,1)) +
				gganimate::transition_time(time) +
				ggplot2::ggtitle("Time: {frame_time}")
		)
	}
}
