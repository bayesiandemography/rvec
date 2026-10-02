# Parallel Minima and Maxima with Rvecs

Compare corresponding elements independently within each draw. Unlike
[`min()`](https://rdrr.io/r/base/Extremes.html) and
[`max()`](https://rdrr.io/r/base/Extremes.html), these functions do not
summarise across elements.

## Usage

``` r
pmin(..., na.rm = FALSE)

pmax(..., na.rm = FALSE)
```

## Arguments

- ...:

  Rvecs or ordinary vectors. With an rvec argument, ordinary inputs must
  be unclassed logical, integer, double, or character vectors (or
  `NULL`). An rvec may occur in any argument position.

- na.rm:

  Whether to ignore missing values. If all corresponding values are
  missing, the result is still missing.

## Value

An rvec if any argument is an rvec, otherwise the result of
[`base::pmin()`](https://rdrr.io/r/base/Extremes.html) or
[`base::pmax()`](https://rdrr.io/r/base/Extremes.html). Names come from
the first argument.

## Details

With rvec arguments, element lengths must agree or be one. Draw counts
must also agree or be one. Ordinary vectors are repeated across draws.
This uses rvec's usual size rules rather than base R's fractional
recycling. Missing values, infinities, and type promotion follow the
base calculation on each draw. Matrices and other classed objects are
not supported alongside rvecs. Calls without rvecs are passed unchanged
to the base functions.

These wrappers mask the base functions when rvec is attached. Explicit
calls to [`base::pmin()`](https://rdrr.io/r/base/Extremes.html) or
[`base::pmax()`](https://rdrr.io/r/base/Extremes.html) do not use the
wrappers.

## Examples

``` r
x <- rvec(rbind(a = c(-2, 3), b = c(4, -1)))
pmax(x, 0)
#> <rvec_dbl<2>[2]>
#>   a   b 
#> 0,3 4,0 
pmax(0, x)
#> <rvec_dbl<2>[2]>
#> [1] 0,3 4,0
pmin(pmax(x, 0), 1)
#> <rvec_dbl<2>[2]>
#>   a   b 
#> 0,1 1,0 
```
