#' Make a Grid to Specify Multiple Ising Networks
#'
#' Specify one or two varying parameters for
#' Ising networks. The output of `make_Ising_grid()` can be used to
#' make landscapes of multiple networks.
#'
#' There are five possible ways to vary the parameters for Ising networks,
#' corresponding to five control functions:
#' \itemize{
#'   \item [single_threshold()] Vary a threshold value for a single variable.
#'   \item [all_thresholds()] Vary all threshold values together.
#'   \item [single_wei()] Vary a single weight value for a path between two
#'   variables.
#'   \item [whole_weiadj()] Vary the whole weighted adjacency matrix.
#'   \item [beta_list()] Use a list of different beta values.
#' }
#'
#' See [make_Ising_grid-control-functions] for details.
#'
#' @param par1,par2 Generated from one of [single_threshold()],
#' [all_thresholds()], [single_wei()]`, [whole_weiadj()],
#' or [beta_list()]. Use `par2 = NULL` if you only
#' want to vary one parameter.
#' @inheritParams make_2d_Isingland
#'
#' @return An `Ising_grid` object that is based on a tibble and
#' contains the information of all simulation conditions.
#'
#' @export
make_Ising_grid <- function(par1, par2 = NULL,
                            thresholds, weiadj, beta = 1) {
  if (is.null(par2)) {
    Igrid <- make_Ising_condition_list(par1, thresholds, weiadj, beta)
    par_name <- attr(Igrid, "par_name")
  } else {
    t1 <- make_Ising_condition_list(par1, thresholds, weiadj, beta)
    t2 <- make_Ising_condition_list(par2, thresholds, weiadj, beta)
    Igrid <-
      dplyr::full_join(t1, t2, by = character())
    par_name <- c(attr(t1, "par_name"), attr(t2, "par_name"))
  }

  for (i in c("thresholds", "weiadj", "beta")) {
    if (!glue::glue("{i}_list") %in% colnames(Igrid)) {
      Igrid <- Igrid %>%
        dplyr::rowwise() %>%
        dplyr::mutate("{i}_list" := list(eval(rlang::sym(i)))) %>%
        dplyr::ungroup()
    }
  }

  return(structure(Igrid,
    class = c("Ising_grid", class(Igrid)),
    par1 = par1, par2 = par2, par_name = par_name
  ))
}

make_Ising_condition_list <- function(par, thresholds, weiadj, beta) {
  if (methods::is(par, "ctrl_single_threshold")) {
    par_name <- glue::glue("threshold_{par$pos}")
    output <- tibble::tibble(
      "threshold_{par$pos}" := par$seq
    ) %>%
      dplyr::mutate(thresholds_list = purrr::map(
        !!rlang::sym(par_name),
        function(x, thresholds) {
          temp <- thresholds
          temp[par$pos] <- par$.f(x, temp[par$pos])
          return(temp)
        },
        thresholds
      ))
  } else if (methods::is(par, "ctrl_all_thresholds")) {
    par_name <- "all_thresholds"
    output <- tibble::tibble(
      all_thresholds = par$seq
    ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(thresholds_list = list(par$.f(all_thresholds, thresholds))) %>%
      dplyr::ungroup()
  } else if (methods::is(par, "ctrl_single_wei")) {
    par_name <- glue::glue("wei_{par$pos[1]}_{par$pos[2]}")
    output <- tibble::tibble(
      "wei_{par$pos[1]}_{par$pos[2]}" := par$seq
    ) %>%
      dplyr::mutate(weiadj_list = purrr::map(
        !!rlang::sym(par_name),
        function(x, weiadj) {
          temp <- weiadj
          temp[par$pos[1], par$pos[2]] <- par$.f(x, temp[par$pos[1], par$pos[2]])
          temp[par$pos[2], par$pos[1]] <- par$.f(x, temp[par$pos[2], par$pos[1]]) # to ensure symmetricity
          return(temp)
        },
        weiadj
      ))
  } else if (methods::is(par, "ctrl_whole_weiadj")) {
    par_name <- "whole_weiadj"
    output <- tibble::tibble(
      whole_weiadj = par$seq
    ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(weiadj_list = list(par$.f(whole_weiadj, weiadj))) %>%
      dplyr::ungroup()
  } else if (methods::is(par, "ctrl_beta_list")) {
    par_name <- "beta_list"
    output <- tibble::tibble(
      beta_list = par$seq
    )
  }
  return(
    structure(output, par_name = par_name)
  )
}

generator_control_pos_seq <- function(type) {
  function(pos, seq, .f = `*`) {
    if (!methods::is(pos, "numeric")) {
      abort_bad_argument("pos", "be a numeric object", class(pos))
    }
    return(structure(list(pos = pos, seq = seq, .f = .f), class = c(paste0("ctrl_", type), "ctrl_pos_seq", "ctrl")))
  }
}

generator_control_seq <- function(type) {
  function(seq, .f = `*`) {
    return(structure(list(seq = seq, .f = .f), class = c(paste0("ctrl_", type), "ctrl_seq", "ctrl")))
  }
}

#' Control Functions to Specify the Varying Parameters for
#' an Ising Grid.
#'
#' @param pos The position of the single threshold or the weight
#' value that should vary across Ising networks. Should be a single
#' number for [single_threshold()] or a numeric vector of
#' length 2 for [single_wei()].
#'
#' @param seq A vector that specify the values. Can be generated
#' with [base::seq()].
#'
#' @param .f What calculation should be done for `seq`
#' and the original threshold value(s) or the original weight(ed
#' adjacency matrix)? `*` by default, which means the values supplied
#' in `seq` will be multiplied to the original value, vector, or matrix.
#'
#' @return An `ctrl_*` object specifying the varying parameters.
#' @name make_Ising_grid-control-functions
NULL

#' @rdname make_Ising_grid-control-functions
#' @export
single_threshold <- generator_control_pos_seq("single_threshold")

#' @rdname make_Ising_grid-control-functions
#' @export
single_wei <- generator_control_pos_seq("single_wei")

#' @rdname make_Ising_grid-control-functions
#' @export
all_thresholds <- generator_control_seq("all_thresholds")

#' @rdname make_Ising_grid-control-functions
#' @export
whole_weiadj <- generator_control_seq("whole_weiadj")

#' @rdname make_Ising_grid-control-functions
#' @export
beta_list <- generator_control_seq("beta_list")
