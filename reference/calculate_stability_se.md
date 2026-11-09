# Calculate the standard error, confidence interval, and p-value for the stability metrics of an Ising landscape using bootstrapping

Note that the BCa method is used for stability differences, and the
percentile method is used for stability measures of individual phases
because the stability measures of individual phases are highly
zero-inflated and may crash the BCa estimation procedure. **The range
estimation of the stability measures of individual phases should be
interpreted with caution**.

## Usage

``` r
calculate_stability_se(
  data,
  split_value = 0.5 * ncol(data),
  R = 1000,
  estimator = c("psychonetrics", "IsingFit"),
  IsingFit_options = list(plot = FALSE),
  psychonetrics_options = list(),
  Isingland_options = list(),
  ...
)

# S3 method for class 'stability_se'
print(x, ...)

compare_stability(
  data1,
  data2,
  split_value = 0.5 * ncol(data1),
  R = 1000,
  estimator = c("psychonetrics", "IsingFit"),
  IsingFit_options = list(plot = FALSE),
  psychonetrics_options = list(),
  Isingland_options = list(),
  ...
)
```

## Arguments

- data:

  A matrix of binary data

- split_value:

  An integer to specify the number of active nodes used to split two
  stability ranges. Default is half of the number of nodes.

- R:

  The number of bootstrap replicates. Usually this will be a single
  positive integer. For importance resampling, some resamples may use
  one set of weights and others use a different set of weights. In this
  case `R` would be a vector of integers where each component gives the
  number of resamples from each of the rows of weights.

- estimator:

  Which package to use for network estimation: `"psychonetrics"`
  (default) or `"IsingFit"`.

- IsingFit_options:

  Parameters passed to
  [`IsingFit::IsingFit()`](https://rdrr.io/pkg/IsingFit/man/isingfit.html)

- psychonetrics_options:

  Parameters passed to
  [`psychonetrics::Ising()`](https://rdrr.io/pkg/psychonetrics/man/Ising.html)

- Isingland_options:

  Parameters passed to
  [`make_2d_Isingland()`](https://sciurus365.github.io/Isinglandr/reference/make_2d_Isingland.md)

- ...:

  Additional arguments passed to
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html)

- x:

  An object of class `stability_se`

- data1, data2:

  Two matrices of binary data

## Details

Use `calculate_stability_se()` for a single dataset, and use
`compare_stability()` for comparing the stability metrics of two groups.

If you encounter the error message "Error in if (any(ints))
out\[inds\[ints\]\] \<- tstar\[k\[inds\[ints\]\]\] : missing value where
TRUE/FALSE needed", you may need to install the patched version of the
`boot.pval` package with
`pak::pkg_install("Sciurus365/boot.pval@patch-1")`. See
<https://github.com/mthulin/boot.pval/issues/4>.

If a BCa adjustment ('a' or 'w') is NA or infinite, try increasing the
number of bootstrap samples (`R`). This may help but is not guaranteed
to resolve the error. See
<https://stats.stackexchange.com/questions/37918/why-is-the-error-estimated-adjustment-a-is-na-generated-from-r-boot-package>.

## References

Puth, M.-T., Neuhäuser, M., & Ruxton, G. D. (2015). On the variety of
methods for calculating confidence intervals by bootstrapping. Journal
of Animal Ecology, 84(4), 892–897.
https://doi.org/10.1111/1365-2656.12382
