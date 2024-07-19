#' Calculate the standard error and related range estimates for the stability metrics
#'
#' @param data A matrix of binary data
#' @inheritParams calculate_stability.2d_Isingland
#' @inheritParams boot::boot
#' @param IsingFit_options Parameters passed to [IsingFit::IsingFit()]
#' @param Isingland_options Parameters passed to [make_2d_Isingland()]
#' @param boot.ci_options Parameters passed to [boot::boot.ci()]. `type = "basic"` is set as the default because the stability metric tends to have zero-inflated, skewed distribution, and the basic method is the most robust to such distribution. The same reason is applied to the p-value calculation.
#' @param boot.pval_options Parameters passed to [boot.pval::boot.pval()]
#' @param ... Parameters passed to [boot::boot()]
#'
#' @export
calculate_stability_se <- function(data, split_value = 0.5 * ncol(data), R = 1000, IsingFit_options = list(), Isingland_options = list(), boot.ci_options = list(type = "basic"), boot.pval_options = list(type = "basic"), ...) {
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

  output <- output %>%
    dplyr::mutate(row.index = dplyr::row_number()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      p.value = do.call(boot.pval::boot.pval, append(boot.pval_options, list(boot_res = boot_res, index = row.index), after = 0)),
      conf.low = do.call(boot::boot.ci, append(boot.ci_options, list(boot.out = boot_res, index = row.index), after = 0))[[boot.ci_options$type]][4],
      conf.high = do.call(boot::boot.ci, append(boot.ci_options, list(boot.out = boot_res, index = row.index), after = 0))[[boot.ci_options$type]][5]
    ) %>%
    dplyr::mutate(p.value = ifelse(statistic == 0 & std.error == 0, NA, p.value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-row.index)

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


#' Compare the stability metrics between two Ising landscapes
#'
#' @param stability_se_1,stability_se_2 Two objects of class \code{stability_se}
#' @inheritParams calculate_stability_se
#'
#' @export
compare_stability <- function(stability_se_1, stability_se_2, boot.ci_options = list(type = "basic"), boot.pval_options = list(type = "basic")) {
  new_boot <- stability_se_1$boot_res
  new_boot$t0 <- stability_se_1$boot_res$t0 - stability_se_2$boot_res$t0
  new_boot$t <- stability_se_1$boot_res$t - stability_se_2$boot_res$t
  new_boot$data <- NULL
  new_boot$seed <- NULL
  new_boot$strata <- NULL
  new_boot$weights <- NULL

  output <- broom::tidy(new_boot)
  boot_res <- new_boot

  output <- output %>%
    dplyr::mutate(row.index = dplyr::row_number()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      p.value = do.call(boot.pval::boot.pval, append(boot.pval_options, list(boot_res = boot_res, index = row.index), after = 0)),
      conf.low = do.call(boot::boot.ci, append(boot.ci_options, list(boot.out = boot_res, index = row.index), after = 0))[[boot.ci_options$type]][4],
      conf.high = do.call(boot::boot.ci, append(boot.ci_options, list(boot.out = boot_res, index = row.index), after = 0))[[boot.ci_options$type]][5]
    ) %>%
    dplyr::mutate(p.value = ifelse(statistic == 0 & std.error == 0, NA, p.value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-row.index)

  return(
    structure(
      list(stats = output, boot_res = new_boot),
      class = c("compare_stability_se", "stability_se", "list")
    )
  )
}
