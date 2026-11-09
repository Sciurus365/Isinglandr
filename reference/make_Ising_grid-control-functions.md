# Control Functions to Specify the Varying Parameters for an Ising Grid.

Control Functions to Specify the Varying Parameters for an Ising Grid.

## Usage

``` r
single_threshold(pos, seq, .f = `*`)

single_wei(pos, seq, .f = `*`)

all_thresholds(seq, .f = `*`)

whole_weiadj(seq, .f = `*`)

beta_list(seq, .f = `*`)
```

## Arguments

- pos:

  The position of the single threshold or the weight value that should
  vary across Ising networks. Should be a single number for
  `single_threshold()` or a numeric vector of length 2 for
  `single_wei()`.

- seq:

  A vector that specify the values. Can be generated with
  [`base::seq()`](https://rdrr.io/r/base/seq.html).

- .f:

  What calculation should be done for `seq` and the original threshold
  value(s) or the original weight(ed adjacency matrix)? `*` by default,
  which means the values supplied in `seq` will be multiplied to the
  original value, vector, or matrix.

## Value

An `ctrl_*` object specifying the varying parameters.
