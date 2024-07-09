
#' Calculate the standard error and related range estimates for the stability metrics
#'
#' @param data
#' @inheritParams calculate_stability.2d_Isingland
#' @inheritParams boot::boot
#' @param ... Parameters passed to [boot::boot()]
#'
#' @export
calculate_stability_se <- function(data, split_value = 0.5 * ncol(data), R = 1000, ...) {
	boot::boot(data = data, statistic = function(x, indicies, split_value, ...) {
		n <- IsingFit::IsingFit(x = x, progressbar = FALSE, ...)
		l <- make_2d_Isingland(thresholds = n$thresholds, weiadj = n$weiadj)
		s <- calculate_stability.2d_Isingland(l, split_value = split_value)
		c(s$stability1, s$stability2, s$stability_diff)
	}, R = R, split_value = split_value, ...)


	# if (b1$boots[[1]]$default != "IsingFit") {
	# 	rlang::abort("The bootnet object is not calculated using IsingFit. Please use `default = 'IsingFit'` in bootnet::bootnet().")
	# }
	#
	# boots <- bootnet$boots
	# all_s <- lapply(boots, function(b){
	# 	l <- make_2d_Isingland(thresholds = b$intercepts, weiadj = b$graph)
	# 	s <- calculate_stability.2d_Isingland(l, split_value = split_value, ...)
	# 	c("stability1" = s$stability1, "stability2" = s$stability2, "stability1_diff" = s$stability_diff)
	# })
	#
	# all_s <- do.call(rbind, all_s)
	# colnames(all_s) <- c("stability1", "stability2", "stability_diff")
	#
	# return(all_s)
}


