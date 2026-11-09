# Changelog

## Development version (aimed for Isinglandr 0.2.0)

- [`make_2d_Isingland()`](https://sciurus365.github.io/Isinglandr/reference/make_2d_Isingland.md)
  now have `transform = TRUE` as default. This is more suitable for the
  `Isinglandr` package, which is designed to work with networks
  estimated from psychological data. The `transform = FALSE` option is
  still available for users who want to work with Ising networks with -1
  and 1 values.
- [`calculate_stability_se()`](https://sciurus365.github.io/Isinglandr/reference/calculate_stability_se.md)
  and
  [`compare_stability()`](https://sciurus365.github.io/Isinglandr/reference/calculate_stability_se.md)
  functions now can use the `psychonetrics` package to estimate the
  networks, and `psychonetrics` is now set as the default package for
  estimating networks. The `IsingFit` package is still available for
  users who want to use it.
- Added functions
  [`calculate_stability_se()`](https://sciurus365.github.io/Isinglandr/reference/calculate_stability_se.md)
  and
  [`compare_stability()`](https://sciurus365.github.io/Isinglandr/reference/calculate_stability_se.md)
  for calculating the standard error of the stability metrics and
  comparing the stability metrics between two networks, respectively.
- Added the
  [`autolayer.stability_2d_Isingland_matrix()`](https://sciurus365.github.io/Isinglandr/reference/autolayer.stability.md)
  method. The structure of the `stability_2d_Isingland_matrix` objects
  changes slightly to accommodate the new method. Users may need to
  re-generate previously `stability_2d_Isingland_matrix` objects
- Bug-fix for
  [`make_3d_Isingland()`](https://sciurus365.github.io/Isinglandr/reference/make_3d_Isingland.md):
  - The function now allows character values for the `x` and `y`
    arguments.

## Isinglandr 0.1.1

CRAN release: 2023-07-12

- Bug-fix for the Shiny app
  ([`shiny_Isingland_MDD()`](https://sciurus365.github.io/Isinglandr/reference/shiny_Isingland_MDD.md)):
  - Load the data to the Shiny app environment;
  - Check if `gifski` is installed, which is required to render the
    animations (also added `gifski` as a suggested package).

## Isinglandr 0.1.0

CRAN release: 2023-02-10

- Initial release.
