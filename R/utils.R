#' Calculate the Hamiltonian for a specific state of a network
#'
#' @param x The state of the network represented in a vector of `0L`
#' and `1L`.
#' @param m The threshold of each node in the network represented
#' in a vector.
#' @param w The edge weights of the network represented in a matrix
#' with all zeros in the diagonal.
#'
#' @return The Hamiltonian.
#' @noRd
ham <- function(x, m, w) {
  -(((t(x) %*% w %*% x) / 2) + (t(x) %*% m))
}

# The following two functions are adapted from the answer by englealuze in
# https://stackoverflow.com/a/62006609/10397503

# the solution of length len will be the solution of shorter length appended with each element in v
all_combination <- function(v, len) {
  if (len <= 1) {
    as.list(v)
  } else {
    append_each_to_list(all_combination(v, len - 1), v)
  }
}

# function to append each element in vector v to list L and return a list
append_each_to_list <- function(L, v) {
  purrr::flatten(lapply(
    v,
    function(n) lapply(L, function(l) c(l, n))
  ))
}

# This function is adapted from
# https://adv-r.hadley.nz/conditions.html?q=abort#custom-conditions
# by Hadley Wickham.
abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- glue::glue("{msg}; not {not}.")
  }

  rlang::abort("error_bad_argument",
    message = msg,
    arg = arg,
    must = must,
    not = not
  )
}

# Undefined global
utils::globalVariables(c(".", ".data", ".env", "barrier", "density", "dist", "freq", "landscape", "n_active", "sim", "sim_output", "sum_freq", "sum_state", "thresholds_list", "time", "v", "weiadj_list", "MDDConnectivity", "MDDThresholds", "stability", "stability_measures", "sum_state_x", "sum_state_y", "p.value", "row.index", "statistic", "std.error"))
