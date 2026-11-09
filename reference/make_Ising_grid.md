# Make a Grid to Specify Multiple Ising Networks

Specify one or two varying parameters for Ising networks. The output of
`make_Ising_grid()` can be used to make landscapes of multiple networks.

## Usage

``` r
make_Ising_grid(par1, par2 = NULL, thresholds, weiadj, beta = 1)
```

## Arguments

- par1, par2:

  Generated from one of
  [`single_threshold()`](https://sciurus365.github.io/Isinglandr/reference/make_Ising_grid-control-functions.md),
  [`all_thresholds()`](https://sciurus365.github.io/Isinglandr/reference/make_Ising_grid-control-functions.md),
  [`single_wei()`](https://sciurus365.github.io/Isinglandr/reference/make_Ising_grid-control-functions.md)`, [whole_weiadj()], or [beta_list()]. Use `par2
  = NULL\` if you only want to vary one parameter.

- thresholds, weiadj:

  The thresholds and the weighted adjacency matrix of the Ising network.
  If you have an `IsingFit` object estimated using
  [`IsingFit::IsingFit()`](https://rdrr.io/pkg/IsingFit/man/isingfit.html),
  you can find those two parameters in its components
  (`<IsingFit>$thresholds` and `<IsingFit>$weiadj`).

- beta:

  The \\\beta\\ value for calculating the Hamiltonian.

## Value

An `Ising_grid` object that is based on a tibble and contains the
information of all simulation conditions.

## Details

There are five possible ways to vary the parameters for Ising networks,
corresponding to five control functions:

- [`single_threshold()`](https://sciurus365.github.io/Isinglandr/reference/make_Ising_grid-control-functions.md)
  Vary a threshold value for a single variable.

- [`all_thresholds()`](https://sciurus365.github.io/Isinglandr/reference/make_Ising_grid-control-functions.md)
  Vary all threshold values together.

- [`single_wei()`](https://sciurus365.github.io/Isinglandr/reference/make_Ising_grid-control-functions.md)
  Vary a single weight value for a path between two variables.

- [`whole_weiadj()`](https://sciurus365.github.io/Isinglandr/reference/make_Ising_grid-control-functions.md)
  Vary the whole weighted adjacency matrix.

- [`beta_list()`](https://sciurus365.github.io/Isinglandr/reference/make_Ising_grid-control-functions.md)
  Use a list of different beta values.

See
[make_Ising_grid-control-functions](https://sciurus365.github.io/Isinglandr/reference/make_Ising_grid-control-functions.md)
for details.
