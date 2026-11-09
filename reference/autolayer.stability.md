# Get ggplot2 layers of stability metrics to add to the landscape plots

Those layers can show how the stability metrics are calculated on the
landscape.

## Usage

``` r
# S3 method for class 'stability_2d_Isingland'
autolayer(
  object,
  point = TRUE,
  line = TRUE,
  split_value = TRUE,
  interval = TRUE,
  stability_value = TRUE,
  ...
)

# S3 method for class 'stability_2d_Isingland_matrix'
autolayer(
  object,
  point = TRUE,
  line = TRUE,
  split_value = TRUE,
  interval = TRUE,
  stability_value = TRUE,
  ...
)
```

## Arguments

- object:

  A `stability` object calculated by
  [`calculate_stability()`](https://sciurus365.github.io/Isinglandr/reference/calculate_stability.md)
  or
  [`calculate_stability.2d_Isingland()`](https://sciurus365.github.io/Isinglandr/reference/calculate_stability.md).

- point, line, split_value, interval, stability_value:

  Show those elements on the layer? Default is `TRUE` for all of them.

- ...:

  Not in use.

## Value

a ggplot layer
