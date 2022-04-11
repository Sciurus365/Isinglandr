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
#' @export
make_Ising_grid <- function(type_par1, control_par1,
                            type_par2 = NULL, control_par2 = NULL,
														thresholds, weiadj, beta = 1) {
	if (is.null(type_par2)){
		Igrid <- make_Ising_condition_list(type_par1, control_par1, thresholds, weiadj, beta)
		par_name <- attr(Igrid, "par_name")
	} else {
		t1 <- make_Ising_condition_list(type_par1, control_par1, thresholds, weiadj, beta)
		t2 <- make_Ising_condition_list(type_par2, control_par2, thresholds, weiadj, beta)
		Igrid <-
			dplyr::full_join(t1, t2, by = character()
			)
		par_name <- c(attr(t1, "par_name"), attr(t2, "par_name"))
	}

	for(i in c("thresholds", "weiadj", "beta")){
		if(!glue::glue("{i}_list") %in% colnames(Igrid)){
			Igrid <- Igrid %>%
				dplyr::rowwise() %>%
				dplyr::mutate("{i}_list" := list(eval(rlang::sym(i)))) %>%
				dplyr::ungroup()
		}
	}

	return(structure(Igrid, class = c("Ising_grid", class(Igrid)),
																		type_par1 = type_par1, control_par1 = control_par1,
																		type_par2 = type_par2, control_par2 = control_par2, par_name = par_name))
}

make_Ising_condition_list <- function(type, control, thresholds, weiadj, beta) {
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
  	par_name <- glue::glue("threshold_{control$pos}")
  	output <- tibble::tibble(
  		"threshold_{control$pos}" := control$seq
  	) %>%
  		dplyr::mutate(thresholds_list = purrr::map(
  			!!rlang::sym(par_name),
  			function(x, thresholds){
					temp <- thresholds
					temp[control$pos] <- x
					return(temp)
  			},
  			thresholds
  		))
  } else if (type == "all_thresholds") {
  	par_name <- "all_thresholds"
  	output <- tibble::tibble(
  		all_thresholds = control$seq
  	) %>%
  		dplyr::rowwise() %>%
  		dplyr::mutate(thresholds_list = list(all_thresholds * thresholds)) %>%
  		dplyr::ungroup()
  } else if (type == "single_wei") {
  	par_name <- glue::glue("wei_{control$pos[1]}_{control$pos[2]}")
  	output <- tibble::tibble(
  		"wei_{control$pos[1]}_{control$pos[2]}" := control$seq
  	) %>%
  		dplyr::mutate(weiadj_list = purrr::map(
  			!!rlang::sym(par_name),
  			function(x, weiadj){
  				temp <- weiadj
  				temp[control$pos[1], control$pos[2]] <- x
  				temp[control$pos[2], control$pos[1]] <- x # to ensure symmetricity
  				return(temp)
  			},
  			weiadj
  		))
  } else if (type == "whole_weiadj") {
  	par_name <- "whole_weiadj"
  	output <- tibble::tibble(
  		whole_weiadj = control$seq
  	) %>%
  		dplyr::rowwise() %>%
  		dplyr::mutate(weiadj_list = list(whole_weiadj * weiadj)) %>%
  		dplyr::ungroup()
  } else if (type == "beta") {
  	par_name <- "beta_list"
  	output <- tibble::tibble(
  		beta_list = control$seq
  	)
  }
	return(
		structure(output, par_name = par_name)
		)
}

#' Control Functions to Specify the Varying Parameters for
#' an Ising Grid
#'
#' Functions to use within [make_Ising_grid()].
#' Use `control_grid_pos_seq()` for types
#' `"single_threshold"` or `"single_wei"`; use `control_seq()` for
#' types `"all_thresholds"`, `"whole_weiadj"`, or `"beta"`.
#'
#' @param pos The position of the single threshold or the weight
#' value that should vary across Ising networks. Should be a single
#' number for `"single_threshold"` or a numeric vector of
#' length 2 for `"single_wei"`.
#'
#' @param seq A vector that specify the values. Can be generated
#' with [seq()].
#'
#' @export
control_grid_pos_seq <- function(pos, seq) {
  if (!is(pos, "numeric")) {
    abort_bad_argument("pos", "be a numeric object", class(pos))
  }
  return(structure(list(pos = pos, seq = seq), class = "ctrl_grid_pos_seq"))
}

#' @rdname control_grid_pos_seq
#' @export
control_grid_seq <- function(seq) {
  return(structure(list(seq = seq), class = "ctrl_grid_seq"))
}
