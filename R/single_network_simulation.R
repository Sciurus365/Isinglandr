simulate_next_step <- function(l, x, beta2) {
	probs <- purrr::map_dbl(c(x-1, x, x+1), ~exp(- beta2*U(l,.x)))
	sample(c(x-1, x, x+1), 1, prob = probs)
}

simulate_next_density <- function(l, dens, beta2) {

}

U <- function(l, x){
	if (x < 0 | x >= nrow(get_dist(l))) {
		return(Inf)
	}
	return(as.numeric(get_dist(l)[x+1, "U"]))
}

#' Simulate from a Landscape
#'
#' @param l A landscape from Ising network.
#' @param mode One of `"single"`, `"distribution"`.
#' @param initial An integer or one of `"random"`, `"steady-state"`.
#' Float numbers will be converted to an integer automatically.
#'
#' @return
#' @export
simulate_2d_Isingland <- function(l, mode = "single",
																	initial = 0, length = 100, beta2 = l$beta) {
	mode <- pmatch(mode, c("single", "distribution"))
	if (is.na(mode)) abort_bad_argument("mode", "be one of 'single', 'distribution'.")
	if (mode == 1) {
		output <- tibble::tibble(
			time = 1:length,
			n_active = vector("integer", length)
		)

		if (is.numeric(initial)) output$n_active[1] <- as.integer(initial)

		for (i in 2:length) {
				output$n_active[i] <- simulate_next_step(l, output$n_active[i-1], beta2)
		}
	} else if (mode == 2) {
		output <- tibble::tibble(time = 1:100) %>%
			dplyr::rowwise() %>%
			dplyr::mutate(n_active = list(0:l$Nvar)) %>%
			dplyr::ungroup()

		density_list <- list()

		if (is.numeric(initial)) {
			initial_density <- rep(0, l$Nvar)
			initial_density[initial] <- 1
			density_list <- initial_density
		}

		for (i in 2:length) {
			##! transition matrix here
			density_list[i] <- simulate_next_density(l, density_list[i-1], beta2)
		}
	}

	return(
		structure(
			list(sim = output, landscape = l, mode = c("single", "distribution")[mode]),
			class = c("sim_2d_Isingland", "sim_Isingland")
		)
	)
}

print.sim_Isingland <- function(x, ...) {
	print(x$sim)
}

plot.sim_2d_Isingland <- function(x, ...){
	tmax <- max(x$sim$time)

	d <- x$sim %>%
		dplyr::left_join(x$landscape$dist, by = c("n_active" = "n_active"))
	plot.2d_Isingland(x$landscape) +
		ggplot2::geom_point(ggplot2::aes(x = n_active, y = U),
												data = d, size = 5, color = "black", fill = "white") +
		gganimate::transition_time(time) +
		ggplot2::ggtitle("Time: {frame_time}")
}
