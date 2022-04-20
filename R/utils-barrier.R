local_min_index <- function(v){
	bounded_v <- c(Inf, v, Inf)
	diff1 <- diff(bounded_v) [-1]
	diff2 <- rev(diff(rev(bounded_v)) [-1])
	return(which(diff1 >= 0 & diff2 >= 0))
}

local_max_index <- function(v){
	bounded_v <- c(-Inf, v, -Inf)
	diff1 <- diff(bounded_v) [-1]
	diff2 <- rev(diff(rev(bounded_v)) [-1])
	return(which(diff1 <= 0 & diff2 <= 0))
}

landscape_shapes <- tibble::tribble(
	~shape, ~Nmin, ~Nmax, ~order, ~start, ~end, ~saddle,
	r"{\/\/}", 2, 3,  rlang::expr(c(maxindex[1], minindex[1], maxindex[2], minindex[2], maxindex[3])), rlang::expr(minindex[1]), rlang::expr(minindex[2]), rlang::expr(maxindex[2]),
	r"{/\}", 2, 1, rlang::expr(c(minindex[1], maxindex[1], minindex[2])), rlang::expr(minindex[1]), rlang::expr(minindex[2]), rlang::expr(maxindex[1]),
	r"{/\/}", 2, 2, rlang::expr(c(minindex[1], maxindex[1], minindex[2], maxindex[2])), rlang::expr(minindex[1]), rlang::expr(minindex[2]), rlang::expr(maxindex[1]),
	r"{\/\}", 2, 2, rlang::expr(c(maxindex[1], minindex[1], maxindex[2], minindex[2])), rlang::expr(minindex[1]), rlang::expr(minindex[2]), rlang::expr(maxindex[2]),
	r"{/}", 1, 1, rlang::expr(c(minindex[1], maxindex[1])), rlang::expr(minindex[1]), rlang::expr(NA), rlang::expr(maxindex[1]),
	r"{\}", 1, 1, rlang::expr(c(maxindex[1], minindex[1])), rlang::expr(NA), rlang::expr(minindex[1]), rlang::expr(maxindex[1])
)

make_point <- function(d, index) {
	if(is.na(index)) {
		U <- NA
		x_index <- NA
		x_value <- NA
	} else{
		U <- d$U[index]
		x_index <- index
		x_value <- d$n_active[index]
	}

	list(
		U = U,
		location = list(
			x_index = x_index,
			x_value = x_value
		)
	)
}
