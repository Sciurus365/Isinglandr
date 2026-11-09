# Calculate energy barrier for Ising landscapes

Calculate energy barrier for Ising landscapes

## Usage

``` r
# S3 method for class '`2d_Isingland`'
calculate_barrier(l, ...)

# S3 method for class '`2d_Isingland_matrix`'
calculate_barrier(l, ...)

# S3 method for class 'barrier_2d_Isingland'
print(x, simplify = FALSE, ...)

# S3 method for class 'barrier_2d_Isingland'
summary(object, ...)

# S3 method for class 'barrier_2d_Isingland_matrix'
summary(object, ...)
```

## Arguments

- l:

  An `Isingland` object constructed with
  [`make_2d_Isingland()`](https://sciurus365.github.io/Isinglandr/reference/make_2d_Isingland.md)
  or
  [`make_2d_Isingland_matrix()`](https://sciurus365.github.io/Isinglandr/reference/make_2d_Isingland_matrix.md).

- ...:

  Not in use.

- x:

  a result of the *default* method of
  [`summary()`](https://rdrr.io/r/base/summary.html).

- simplify:

  Print a simplified version of the output? Default is `FALSE`.

- object:

  an object for which a summary is desired.

## Value

A `barrier_Isingland` object that contains the following components:

- `shape` A character describing the shape of the landscape.

- `local_min_start`,`local_min_end`,`saddle_point` The positions of the
  two local minimums and the saddle point, described each by a list
  containing:

  - `U` The potential value.

  - `location`

    - `x_index` The row index in `get_dist(l)`.

    - `x_value` The number of active nodes.

- `delta_U_start`,`delta_U_end` The barrier heights for both sides.

## Functions

- `summary(barrier_2d_Isingland)`: Return a vector of barrier heights.

- `summary(barrier_2d_Isingland_matrix)`: Return a tibble of barrier
  heights and conditions.
