# Make a 3D landscape for an Ising network

Similar to
[`make_2d_Isingland()`](https://sciurus365.github.io/Isinglandr/reference/make_2d_Isingland.md),
but two categories of nodes can be specified so the number of active
nodes can be calculated separately.

## Usage

``` r
make_3d_Isingland(thresholds, weiadj, x, y, beta = 1, transform = FALSE)
```

## Arguments

- thresholds, weiadj:

  The thresholds and the weighted adjacency matrix of the Ising network.
  If you have an `IsingFit` object estimated using
  [`IsingFit::IsingFit()`](https://rdrr.io/pkg/IsingFit/man/isingfit.html),
  you can find those two parameters in its components
  (`<IsingFit>$thresholds` and `<IsingFit>$weiadj`).

- x, y:

  Two vectors specifying the indices or the names of the nodes for two
  categories. If they are character vectors, the names should match the
  row names of the `thresholds` matrix.

- beta:

  The \\\beta\\ value for calculating the Hamiltonian.

- transform:

  By default, this function considers the Ising network to use `-1` and
  `1` for two states. Set `transform = TRUE` if the Ising network uses
  `0` and `1` for two states, *which is often the case for the Ising
  networks estimated using*
  [`IsingFit::IsingFit()`](https://rdrr.io/pkg/IsingFit/man/isingfit.html).

## Value

A `3d_Isingland` object that contains the following components:

- `dist_raw`,`dist` Two tibbles containing the probability distribution
  and the potential values for different states.

- `thresholds`,`weiadj`,`beta` The parameters supplied to the function.

- `Nvar` The number of variables (nodes) in the Ising network.

## See also

[`make_2d_Isingland()`](https://sciurus365.github.io/Isinglandr/reference/make_2d_Isingland.md)
for the algorithm.
