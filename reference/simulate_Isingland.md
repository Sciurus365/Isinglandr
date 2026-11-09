# Simulate a 2D Ising landscape

Perform a numeric simulation using the landscape. The simulation is
performed using a similar algorithm as Glauber dynamics, that the
transition possibility is determined by the difference in the potential
function, and the steady-state distribution is the same as the Boltzmann
distribution (if not setting `beta2`). Note that, the conditional
transition possibility of this simulation may be different from Glauber
dynamics or other simulation methods.

## Usage

``` r
simulate_Isingland(l, ...)

# S3 method for class '`2d_Isingland`'
simulate_Isingland(
  l,
  mode = "single",
  initial = 0,
  length = 100,
  beta2 = l$beta,
  ...
)

# S3 method for class '`2d_Isingland_matrix`'
simulate_Isingland(
  l,
  mode = "single",
  initial = 0,
  length = 100,
  beta2 = NULL,
  ...
)
```

## Arguments

- l:

  An `Isingland` object constructed with
  [`make_2d_Isingland()`](https://sciurus365.github.io/Isinglandr/reference/make_2d_Isingland.md)
  or
  [`make_2d_Isingland_matrix()`](https://sciurus365.github.io/Isinglandr/reference/make_2d_Isingland_matrix.md).

- ...:

  Not in use.

- mode:

  One of `"single"`, `"distribution"`. Do you want to simulate the state
  of a single system stochastically or simulate the distribution of the
  states? `"single"` is used by default.

- initial:

  An integer indicating the initial number of active nodes for the
  simulation. Float numbers will be converted to an integer
  automatically.

- length:

  An integer indicating the simulation length.

- beta2:

  The \\beta\\ value used for simulation. By default use the same value
  as for landscape construction. Manually setting this value can make
  the system easier or more difficult to transition to another state,
  but will alter the steady-state distribution as well.

## Value

A `sim_Isingland` object with the following components:

- `output` A tibble of the simulation output.

- `landscape` The landscape object supplied to this function.

- `mode` A character representing the mode of the simulation.

## Details

In each simulation step, the system can have one more active node, one
less active node, or the same number of active nodes (if possible; so if
all nodes are already active then it is not possible to have one more
active node). The possibility of the three cases is determined by their
potential function:

\$\$P(n\_{t}=b\|n\_{t-1}=a) = \frac{e^{-\beta U(b)}}{\sum\_{i \in
\\a-1,a,a+1\\,0\leq i\leq N}e^{-\beta U(i)}}, \\ \mathrm{if} \\
b\in\\a-1,a,a+1\\\\ \\\\ 0\leq i\leq N; 0, \mathrm{otherwise},\$\$

where \\n\_{t}\\ is the number of active nodes at the time \\t\\, and
\\U(n)\\ is the potential function.

## Examples

``` r
if (FALSE) { # interactive()
Nvar <- 10
m <- rep(0, Nvar)
w <- matrix(0.1, Nvar, Nvar)
diag(w) <- 0
result1 <- make_2d_Isingland(m, w)
plot(result1)

set.seed(1614)
sim1 <- simulate_Isingland(result1, initial = 5)
plot(sim1)
}
```
