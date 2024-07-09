
#' Calculate the standard error and related range estimates for the stability metrics
#'
#' @param bootnet A `bootnet` object calculated by [bootnet::bootnet()], with the default `type = "IsingFit"`. See [bootnet::bootnet()] for details.
#' @inheritParams calculate_stability.2d_Isingland
#'
#' @export
calculate_stability_se <- function(bootnet, split_value = 0.5 * bootnet$boots[[1]]$nNode, ...) {
	# check if the bootnet object is calculated using IsingFit

	if (b1$boots[[1]]$default != "IsingFit") {
		rlang::abort("The bootnet object is not calculated using IsingFit. Please use `default = 'IsingFit'` in bootnet::bootnet().")
	}

	boots <- bootnet$boots
	all_s <- lapply(boots, function(b){
		l <- make_2d_Isingland(thresholds = b$intercepts, weiadj = b$graph)
		s <- calculate_stability.2d_Isingland(l, split_value = split_value, ...)
		c(s$stability1, s$stability2, s$stability_diff)
	})

	all_s <- do.call(rbind, all_s)
	colnames(all_s) <- c("stability1", "stability2", "stability_diff")

	return(all_s)
}


