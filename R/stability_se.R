#' Calculate the standard error and related range estimates for the stability metrics
#'
#' @param data A matrix of binary data
#' @inheritParams calculate_stability.2d_Isingland
#' @inheritParams boot::boot
#' @param IsingFit_options Parameters passed to [IsingFit::IsingFit()]. Temporarily not effective.
#' @param boot.ci_options Parameters passed to [boot::boot.ci()]
#' @param boot.pval_options Parameters passed to [boot.pval::boot.pval()]
#' @param ... Parameters passed to [boot::boot()]
#'
#' @export
calculate_stability_se <- function(data, split_value = 0.5 * ncol(data), R = 1000, IsingFit_options = list(), boot.ci_options = list(type = "perc"), boot.pval_options = list(), ...) {

	if (ncol(data) >= 20) {
		rlang::warn("The number of variables is large. It may take a long time for bootstrapping.")
	}

	boot_res <- boot::boot(data = data, statistic = function(x, indices, split_value) {
		n <- do.call(IsingFit::IsingFit, append(list(), list(x = x[indices,], progressbar = FALSE), after = 0))
		l <- make_2d_Isingland(thresholds = n$thresholds, weiadj = n$weiadj)
		s <- calculate_stability.2d_Isingland(l, split_value = split_value)
		c("sability1" = s$stability1, "sability2" = s$stability2, "sability_diff" = s$stability_diff)
	}, R = R, split_value = split_value, ...)

	output <- broom::tidy(boot_res)

	output <- output %>%
		dplyr::mutate(row.index = dplyr::row_number()) %>%
		dplyr::rowwise() %>%
		dplyr::mutate(
			p.value = do.call(boot.pval::boot.pval, append(boot.pval_options, list(boot_res = boot_res, index = row.index), after = 0)),
			conf.low = do.call(boot::boot.ci, append(boot.ci_options, list(boot.out = boot_res, index = row.index), after = 0))$percent[4],
			conf.high = do.call(boot::boot.ci, append(boot.ci_options, list(boot.out = boot_res, index = row.index), after = 0))$percent[5]
		) %>%
		dplyr::mutate(p.value = ifelse(statistic == 0 & std.error == 0, NA, p.value)) %>%
		dplyr::ungroup() %>%
		dplyr::select(-row.index)

	return(output)
}
