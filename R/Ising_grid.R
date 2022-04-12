#' Make a Grid to Specify Multiple Ising Networks
#'
#' `make_Ising_grid()` can specify one or two varying parameters for
#' Ising networks. The output of `make_Ising_grid()` can be used to
#' make landscapes of multiple networks.
#'
#' @param par1,par2 Generated from one of [single_threshold()],
#' [all_thresholds()], [single_wei()]`, [whole_weiadj()],
#' or [beta_list()]. `par2` can be left blank.
#' @inheritParams make_2d_Isingland
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
  if (is(par, "ctrl_single_threshold")) {
    par_name <- glue::glue("threshold_{par$pos}")
    output <- tibble::tibble(
      "threshold_{par$pos}" := par$seq
    ) %>%
      dplyr::mutate(thresholds_list = purrr::map(
        !!rlang::sym(par_name),
        function(x, thresholds) {
          temp <- thresholds
          temp[par$pos] <- x
          return(temp)
        },
        thresholds
      ))
  } else if (is(par, "ctrl_all_thresholds")) {
    par_name <- "all_thresholds"
    output <- tibble::tibble(
      all_thresholds = par$seq
    ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(thresholds_list = list(all_thresholds * thresholds)) %>%
      dplyr::ungroup()
  } else if (is(par, "ctrl_single_wei")) {
    par_name <- glue::glue("wei_{par$pos[1]}_{par$pos[2]}")
    output <- tibble::tibble(
      "wei_{par$pos[1]}_{par$pos[2]}" := par$seq
    ) %>%
      dplyr::mutate(weiadj_list = purrr::map(
        !!rlang::sym(par_name),
        function(x, weiadj) {
          temp <- weiadj
          temp[par$pos[1], par$pos[2]] <- x
          temp[par$pos[2], par$pos[1]] <- x # to ensure symmetricity
          return(temp)
        },
        weiadj
      ))
  } else if (is(par, "ctrl_whole_weiadj")) {
    par_name <- "whole_weiadj"
    output <- tibble::tibble(
      whole_weiadj = par$seq
    ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(weiadj_list = list(whole_weiadj * weiadj)) %>%
      dplyr::ungroup()
  } else if (is(par, "ctrl_beta_list")) {
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
  function(pos, seq) {
    if (!is(pos, "numeric")) {
      abort_bad_argument("pos", "be a numeric object", class(pos))
    }
    return(structure(list(pos = pos, seq = seq), class = c(paste0("ctrl_", type), "ctrl_pos_seq", "ctrl")))
  }
}

generator_control_seq <- function(type) {
  function(seq) {
    return(structure(list(seq = seq), class = c(paste0("ctrl_", type), "ctrl_seq", "ctrl")))
  }
}

#' Control Functions to Specify the Varying Parameters for
#' an Ising Grid
#'
#' Functions to use within [make_Ising_grid()].
#'
#' @param pos The position of the single threshold or the weight
#' value that should vary across Ising networks. Should be a single
#' number for [single_threshold()] or a numeric vector of
#' length 2 for [single_wei()].
#'
#' @param seq A vector that specify the values. Can be generated
#' with [seq()].
#'
#' @export
single_threshold <- generator_control_pos_seq("single_threshold")

#' @rdname single_threshold
#' @export
single_wei <- generator_control_pos_seq("single_wei")

#' @rdname single_threshold
#' @export
all_thresholds <- generator_control_seq("all_thresholds")

#' @rdname single_threshold
#' @export
whole_weiadj <- generator_control_seq("whole_weiadj")

#' @rdname single_threshold
#' @export
beta_list <- generator_control_seq("beta_list")
