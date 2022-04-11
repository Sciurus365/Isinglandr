#' @rdname make_2d_Isingland_matrix.default
#' @export
make_2d_Isingland_matrix <- function(...) {
  UseMethod("make_2d_Isingland")
}

#' @rdname make_2d_Isingland_matrix.default
#' @export
make_2d_Isingland_matrix.IsingFit <- function(...) {
  make_2d_Isingland.default(Ising_network$thresholds, Ising_network$weiadj, ...)
}

#' @rdname make_2d_Isingland_matrix.default
#' @export
make_2d_Isingland_matrix.list <- function(...) {
  if (is(..1[[1]], "IsingFit")) {
    args <- list(...)
    do.call(make_2d_Isingland_matrix.default, args)
  } else if (is.numeric(..1[[1]])) {
    make_2d_Isingland_matrix.default(...)
  }
}

make_2d_Isingland_matrix.default <- function(thresholds, weiadj, beta = 1, mode, transform = FALSE) {

}
