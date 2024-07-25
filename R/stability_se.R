#' Calculate the standard error, confidence interval, and p-value for the stability metrics of an Ising landscape using bootstrapping
#'
#' Note that the BCa method is used for stability differences, and the percentile method is used for stability measures of individual phases because the stability measures of individual phases are highly zero-inflated and may crash the BCa estimation procedure. **The range estimation of the stability measures of individual phases should be interpreted with caution**.
#'
#' Use [calculate_stability_se()] for a single dataset, and use [compare_stability()] for comparing the stability metrics of two groups.
#'
#' If you encounter the error message "Error in if (any(ints)) out\[inds\[ints\]\] <- tstar\[k\[inds\[ints\]\]\] : missing value where TRUE/FALSE needed", you may need to install the patched version of the `boot.pval` package with `pak::pkg_install("Sciurus365/boot.pval@patch-1")`. See \url{https://github.com/mthulin/boot.pval/issues/4}.
#'
#' If you encounter the error message "estimated adjustment 'a' is NA", that probably means you should increase the number of bootstrap samples (`R`). See \url{https://stats.stackexchange.com/questions/37918/why-is-the-error-estimated-adjustment-a-is-na-generated-from-r-boot-package}.
#'
#' @param data A matrix of binary data
#' @inheritParams calculate_stability.2d_Isingland
#' @inheritParams boot::boot
#' @param IsingFit_options Parameters passed to [IsingFit::IsingFit()]
#' @param Isingland_options Parameters passed to [make_2d_Isingland()]
#' @param ... Parameters passed to [boot::boot()]
#'
#' @export
#' @references Puth, M.-T., Neuhäuser, M., & Ruxton, G. D. (2015). On the variety of methods for calculating confidence intervals by bootstrapping. Journal of Animal Ecology, 84(4), 892–897. https://doi.org/10.1111/1365-2656.12382
calculate_stability_se <- function(data, split_value = 0.5 * ncol(data), R = 1000, IsingFit_options = list(plot = FALSE), Isingland_options = list(), ...) {
	if (ncol(data) >= 20) {
		cli::cli_warn("The number of variables is large. It may take a long time for bootstrapping.")
	}

	boot_res <- boot::boot(data = data, statistic = function(x, indices, split_value, .IsingFit_options, .Isingland_options) {
		n <- do.call(IsingFit::IsingFit, append(.IsingFit_options, list(x = x[indices, ], progressbar = FALSE), after = 0))
		l <- do.call(make_2d_Isingland, append(.Isingland_options, list(thresholds = n$thresholds, weiadj = n$weiadj), after = 0))
		s <- calculate_stability.2d_Isingland(l, split_value = split_value)
		c("stability1" = s$stability1, "stability2" = s$stability2, "stability_diff" = s$stability_diff)
	}, R = R, split_value = split_value, .IsingFit_options = IsingFit_options, .Isingland_options = Isingland_options, ...)

	output <- broom::tidy(boot_res)

	## in the following code, if you got this error, "estimated adjustment 'a' is NA", that probably means you should increase the number of bootstrap samples (`R`).

	tryCatch({
		output <- output %>%
			dplyr::mutate(row.index = dplyr::row_number(), boot_type = c("perc", "perc", "bca")) %>%
			dplyr::rowwise() %>%
			dplyr::mutate(
				p.value = boot.pval::boot.pval(boot_res, index = row.index, type = boot_type),
				conf.low = boot::boot.ci(boot_res, index = row.index, type = boot_type)[[4]][4],
				conf.high = boot::boot.ci(boot_res, index = row.index, type = boot_type)[[4]][5]
			) %>%
			dplyr::ungroup() %>%
			dplyr::select(-row.index)
	}, error = function(e) {
		original_message <- conditionMessage(e)
		cli::cli_abort(c("An error occurred. The original error message is as follows:",
										 x = original_message,
										 i = "If you got this error, 'estimated adjustment 'a' is NA', that probably means you should increase the number of bootstrap samples (`R`)."))
	})

	return(
		structure(
			list(stats = output, boot_res = boot_res),
			class = c("stability_se", "list")
		)
	)
}


#' @export
#' @rdname calculate_stability_se
#' @param x An object of class \code{stability_se}
#'
print.stability_se <- function(x, ...) {
	print(x$stats)
	invisible(x)
}


#' @rdname calculate_stability_se
#' @param data1,data2 Two matrices of binary data
#' @export
compare_stability <- function(data1, data2, split_value = 0.5 * ncol(data), R = 1000, IsingFit_options = list(plot = FALSE), Isingland_options = list(), ...) {
	if (ncol(data1) >= 20) {
		cli::cli_warn("The number of variables is large. It may take a long time for bootstrapping.")
	}

	if (ncol(data1) != ncol(data2)) {
		cli::cli_abort("The number of variables in data1 and data2 must be the same.")
	}

	data1 <- cbind(data1, group = 1)
	data2 <- cbind(data2, group = 2)

	data <- rbind(data1, data2)

	boot_res <- boot::boot(data = data, statistic = function(x, indices, split_value, .IsingFit_options, .Isingland_options) {
		x <- x[indices,]
		x1 <- x[x[,"group"] == 1, -ncol(x)]
		x2 <- x[x[,"group"] == 2, -ncol(x)]

		n1 <- do.call(IsingFit::IsingFit, append(.IsingFit_options, list(x = x1, progressbar = FALSE), after = 0))
		n2 <- do.call(IsingFit::IsingFit, append(.IsingFit_options, list(x = x2, progressbar = FALSE), after = 0))
		l1 <- do.call(make_2d_Isingland, append(.Isingland_options, list(thresholds = n1$thresholds, weiadj = n1$weiadj), after = 0))
		l2 <- do.call(make_2d_Isingland, append(.Isingland_options, list(thresholds = n2$thresholds, weiadj = n2$weiadj), after = 0))
		s1 <- calculate_stability.2d_Isingland(l1, split_value = split_value)
		s2 <- calculate_stability.2d_Isingland(l2, split_value = split_value)

		c("group1_stability1" = s1$stability1, "group1_stability2" = s1$stability2,
			"group1_stability_diff" = s1$stability_diff, "group2_stability1" = s2$stability1,
			"group2_stability2" = s2$stability2, "group2_stability_diff" = s2$stability_diff,
			"diff_stability_diff" = s1$stability_diff - s2$stability_diff)
	}, R = R, split_value = split_value, .IsingFit_options = IsingFit_options, .Isingland_options = Isingland_options, ...)

	output <- broom::tidy(boot_res)

	tryCatch({
		output <- output %>%
			dplyr::mutate(row.index = dplyr::row_number(), boot_type = c("perc", "perc", "bca", "perc", "perc", "bca", "bca")) %>%
			dplyr::rowwise() %>%
			dplyr::mutate(
				p.value = boot.pval::boot.pval(boot_res, index = row.index, type = boot_type),
				conf.low = boot::boot.ci(boot_res, index = row.index, type = boot_type)[[4]][4],
				conf.high = boot::boot.ci(boot_res, index = row.index, type = boot_type)[[4]][5]
			) %>%
			dplyr::ungroup() %>%
			dplyr::select(-row.index)
	}, error = function(e) {
		original_message <- conditionMessage(e)
		cli::cli_abort(c("An error occurred. The original error message is as follows:",
										 x = original_message,
										 i = "If you got this error, 'estimated adjustment 'a' is NA', that probably means you should increase the number of bootstrap samples (`R`)."))
	})

	return(
		structure(
			list(stats = output, boot_res = boot_res),
			class = c("stability_se", "list")
		)
	)
}
