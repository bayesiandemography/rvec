# Calculate Weighted Summaries

Calculate weighted

- means

- medians

- MADs (mean absolute deviations)

- variances

- standard deviations.

These functions all work with ordinary vectors and with
[rvecs](https://bayesiandemography.github.io/rvec/reference/rvec.md).

## Usage

``` r
weighted_mean(x, wt = NULL, na_rm = FALSE)

# Default S3 method
weighted_mean(x, wt = NULL, na_rm = FALSE)

# S3 method for class 'rvec'
weighted_mean(x, wt = NULL, na_rm = FALSE)

weighted_mad(x, wt = NULL, na_rm = FALSE)

# Default S3 method
weighted_mad(x, wt = NULL, na_rm = FALSE)

# S3 method for class 'rvec'
weighted_mad(x, wt = NULL, na_rm = FALSE)

weighted_median(x, wt = NULL, na_rm = FALSE)

# Default S3 method
weighted_median(x, wt = NULL, na_rm = FALSE)

# S3 method for class 'rvec'
weighted_median(x, wt = NULL, na_rm = FALSE)

weighted_sd(x, wt = NULL, na_rm = FALSE)

# Default S3 method
weighted_sd(x, wt = NULL, na_rm = FALSE)

# S3 method for class 'rvec'
weighted_sd(x, wt = NULL, na_rm = FALSE)

weighted_var(x, wt = NULL, na_rm = FALSE)

# Default S3 method
weighted_var(x, wt = NULL, na_rm = FALSE)

# S3 method for class 'rvec'
weighted_var(x, wt = NULL, na_rm = FALSE)
```

## Arguments

- x:

  Quantity being summarised. An ordinary vector or an
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md).

- wt:

  Weights. An ordinary vector, an
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md),
  or `NULL` (the default.) If `NULL`, an unweighted summary is returned.

- na_rm:

  Whether to remove `NA`s in `x` or `wt` before calculating. Default is
  `FALSE`. See
  [`matrixStats::weightedMean()`](https://rdrr.io/pkg/matrixStats/man/weightedMean.html)
  for a description of the algorithm used.

## Value

If `x` or `wt` or is
[rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md),
then an rvec of length 1. Otherwise, a scalar.

## Details

`x` and `wt` must have the same length.

Internally the calculations are done by
[matrixStats](https://CRAN.R-project.org/package=matrixStats) functions
such as
[`matrixStats::weightedMean()`](https://rdrr.io/pkg/matrixStats/man/weightedMean.html)
and
[`matrixStats::colWeightedMeans()`](https://rdrr.io/pkg/matrixStats/man/rowWeightedMeans.html).

## See also

- Functions [`mean()`](https://rdrr.io/r/base/mean.html),
  [`median()`](https://rdrr.io/r/stats/median.html),
  [`mad()`](https://rdrr.io/r/stats/mad.html),
  [`var()`](https://bayesiandemography.github.io/rvec/reference/var.md),
  [`sd()`](https://bayesiandemography.github.io/rvec/reference/sd.md)
  for unweighted data all have methods for rvecs

- The original
  [matrixStats](https://CRAN.R-project.org/package=matrixStats) weighted
  summary functions have additional options not implemented in the
  functions here.

- [`weighted.mean()`](https://rdrr.io/r/stats/weighted.mean.html) is a
  base R function for weighted data

- For numeric summaries of draws in an rvec, use
  [`draws_median()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md),
  [draws_mean](https://bayesiandemography.github.io/rvec/reference/draws_median.md),
  [`draws_quantile()`](https://bayesiandemography.github.io/rvec/reference/draws_quantile.md),
  [`draws_fun()`](https://bayesiandemography.github.io/rvec/reference/draws_fun.md).

## Examples

``` r
## 'x' is rvec, 'wt' is ordinary vector
v <- rvec(list(c(1, 11),
               c(2, 12),
               c(7, 17)))
weights <- c(40, 80, 72)
weighted_mean(v, wt = weights)
#> <rvec_dbl<2>[1]>
#> [1] 3.667,13.67

## 'x' is ordinary vector, 'wt' is rvec
y <- c(1, 2, 3)
w <- rvec(list(c(100, 200),
               c(210, 889),
               c(200, 200)))
weighted_mean(y, wt = w)
#> <rvec_dbl<2>[1]>
#> [1] 2.196,2
weighted_mean(y, wt = w, na_rm = TRUE)
#> <rvec_dbl<2>[1]>
#> [1] 2.196,2
```
