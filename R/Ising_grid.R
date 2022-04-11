#' Make a Grid to Specify Multiple Ising Networks
#'
#' `make_Ising_grid()` can specify one or two varying parameters for
#' Ising networks. The output of `make_Ising_grid()` can be used to
#' make landscapes of multiple networks.
#'
#' @param type_par1,type_par2 One of `"single_threshold"`,
#' `"all_thresholds"`, `"single_wei"`, `"whole_weiadj"`,
#' or `"beta"`. `type_par2` can be left blank.
#' @param control_par1,control_par2 See [control_grid_pos_seq()].
#'
#' @return
#' @export
#'
#' @examples
make_Ising_grid <- function(type_par1, control_par1,
                            type_par2 = NULL, control_par2 = NULL,
														) {
	if (is.null(type_par2)){
		return(make_Ising_condition_list(type_par1, control_par1))
	} else {
		return(
			tidyr::expand_grid(
				make_Ising_condition_list(type_par1, control_par1),
				make_Ising_condition_list(type_par2, control_par2)
			)
		)
	}
}

make_Ising_condition_list <- function(type, control) {
	if (type == "single_threshold" | type == "single_wei"){
		if (!is(control, "ctrl_grid_pos_seq")) {
			abort_bad_argument(control, "be generated from control_grid_pos_seq()")
		}
	} else if (type == "all_thresholds" | type == "whole_weiadj" | type == "beta"){
		if (!is(control, "ctrl_grid_seq")) {
			abort_bad_argument(control, "be generated from control_grid_seq()")
		}
	} else {
		abort_bad_argument("type", "be one of 'single_threshold',
											 'all_thresholds', 'single_wei', 'whole_weiadj',
											 or 'beta'", type)
	}

  if (type == "single_threshold") {
  	return(tibble::tibble(
  		!!glue::glue("threshould_{control$pos}") := control$seq
  	))
  } else if (type == "all_thresholds") {
  	return(tibble::tibble(
  		all_threshoulds = control$seq
  	))
  } else if (type == "single_wei") {
  	return(tibble::tibble(
  		!!glue::glue("wei_{control$pos[1]}_{control$pos[2]}") := control$seq
  	))
  } else if (type == "whole_weiadj") {
  	return(tibble::tibble(
  		whole_weiadj = control$seq
  	))
  } else if (type == "beta") {
  	return(tibble::tibble(
  		beta = control$seq
  	))
  }
}

#' Control Functions to Specify the Varying Parameters for
#' an Ising Grid
#'
#' Functions to use within [make_Ising_grid()].
#' Use `control_grid_position_seq()` for types
#' `"single_threshold"` or `"single_wei"`; use `control_seq()` for
#' types `"all_thresholds"`, `"whole_weiadj"`, or `"beta"`.
#'
#' @param position The position of the single threshold or the weight
#' value that should vary across Ising networks. Should be a single
#' number for `"single_threshold"` or a numeric vector of
#' length 2 for `"single_wei"`.
#'
#' @param ... Other parameters passed to [seq()] that specify the
#' sample grid.
#'
#' @export
control_grid_pos_seq <- function(position, ...) {
  if (!is(position, "numeric")) {
    abort_bad_argument("position", "be a numeric object", class(position))
  }
  return(structure(list(pos = position, seq = seq(...)), class = "ctrl_grid_pos_seq"))
}

#' @rdname control_grid_pos_seq
#' @export
control_grid_seq <- function(...) {
  return(structure(list(seq = seq(...)), class = "ctrl_grid_seq"))
}
