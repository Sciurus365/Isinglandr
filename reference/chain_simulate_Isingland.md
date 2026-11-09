# Make Ising chains from (a series of) Ising grid(s) and perform a chain simulation.

First specify what is the network parameter in each time points, then
perform a chain simulation based on it. An Ising chain can be generated
from one or more Ising grid(s) with one changing condition each.

## Usage

``` r
chain_simulate_Isingland(
  Ising_chain,
  transform = FALSE,
  initial = 0,
  beta2 = NULL
)

make_Ising_chain(...)
```

## Arguments

- Ising_chain:

  An `Ising_chain` object generated from `make_Ising_chain()`.

- transform:

  By default, this function considers the Ising network to use `-1` and
  `1` for two states. Set `transform = TRUE` if the Ising network uses
  `0` and `1` for two states, *which is often the case for the Ising
  networks estimated using*
  [`IsingFit::IsingFit()`](https://rdrr.io/pkg/IsingFit/man/isingfit.html).

- initial:

  An integer indicating the initial number of active nodes for the
  simulation. Float numbers will be converted to an integer
  automatically.

- beta2:

  The \\beta\\ value used for simulation. By default use the same value
  as for landscape construction. Manually setting this value can make
  the system easier or more difficult to transition to another state,
  but will alter the steady-state distribution as well.

- ...:

  Ising grid(s) created by
  [`make_Ising_grid()`](https://sciurus365.github.io/Isinglandr/reference/make_Ising_grid.md).

## Value

`make_Ising_chain` returns an `Ising_chain` object, which is a tibble,
and each row represents a set of parameters for an Ising network.

`chain_simulate_Isingland` returns a `chain_sim_Isingland` object, which
is a tibble containing the parameters, the landscape, and the number of
active nodes for each time step.
