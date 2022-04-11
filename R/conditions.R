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
