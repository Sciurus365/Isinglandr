# Calculate the stability metrics for Ising landscapes

The stability is calculated based on the shape of the potential
landscape and the prior knowledge about the qualitatively different
parts of the system. Two stability indicators are calculated separately,
and their difference is used to represent a general stability of the
system in favor of the first phase. Within each phase, the potential
difference between the local maximum and the local minimum (if multiple
minimums exist, use the one that is further from the other phase; and
the local maximum should always be on the side to the other phase) is
used to represent the stability of this phase.

## Usage

``` r
calculate_stability(l, ...)

# S3 method for class '`2d_Isingland`'
calculate_stability(l, split_value = 0.5 * l$Nvar, ...)

# S3 method for class '`2d_Isingland_matrix`'
calculate_stability(l, split_value = 0.5 * l$Nvar, ...)
```

## Arguments

- l:

  An `Isingland` object constructed with
  [`make_2d_Isingland()`](https://sciurus365.github.io/Isinglandr/reference/make_2d_Isingland.md)
  or
  [`make_2d_Isingland_matrix()`](https://sciurus365.github.io/Isinglandr/reference/make_2d_Isingland_matrix.md).

- ...:

  Not in use.

- split_value:

  An integer to specify the number of active nodes used to split two
  stability ranges. Default is half of the number of nodes.

## Value

- `calculate_stability.2d_Isingland()`:

  Returns a `calculate_stability.2d_Isingland` project, which contains
  the following elements:

  dist

  :   The distribution tibble which is the same as in the input `l`.

  effective_minindex1,effective_maxindex1,effective_minindex2,effective_maxindex2

  :   The (row)indices in `dist` that were used as the positions of the
      local minimums and maximums in two parts.

  stability1,stability2,stability_diff

  :   The stability measures for the first (left) part, the second part
      (right), and their difference.

- `calculate_stability.2d_Isingland_matrix()`:

  Returns a `stability_2d_Isingland_matrix` object, which is a tibble
  containing columns of the varying parameters and a column `stability`
  of the `calculate_stability.2d_Isingland` objects for each landscape.

When [`print()`](https://rdrr.io/r/base/print.html)ed, a verbal
description of the stability metrics is shown. Use the
[`summary()`](https://rdrr.io/r/base/summary.html) method for a tidy
version of the outputs.
