# Find the Minimum or Maximum Position Within Each Draw

For an rvec, find the position of the first minimum or maximum among its
elements independently within each draw. Ordinary inputs retain the
behavior of [`base::which.min()`](https://rdrr.io/r/base/which.min.html)
and [`base::which.max()`](https://rdrr.io/r/base/which.min.html).

## Usage

``` r
which.min(x)

# Default S3 method
which.min(x)

# S3 method for class 'rvec'
which.min(x)

which.max(x)

# Default S3 method
which.max(x)

# S3 method for class 'rvec'
which.max(x)
```

## Arguments

- x:

  An ordinary vector or an
  [`rvec()`](https://bayesiandemography.github.io/rvec/reference/rvec.md).

## Value

An rvec of indices with the same number of draws as `x` if `x` is an
rvec; otherwise the corresponding base R result. A nonempty rvec returns
one position per draw. An empty rvec returns a length-zero rvec.
Positions refer to the original elements, starting at one. The rvec
result has no name. Indices are normally integers; very long inputs may
require double indices, as in base R.

## Details

Missing values are ignored. For a nonempty rvec, a draw with no valid
value returns `NA_integer_`, with one summary warning giving the number
of affected draws. Base R instead returns `integer(0)` for an entirely
missing ordinary vector. An empty rvec returns a length-zero result
without a warning, preserving its number of draws. Ties select the first
position. Character values follow base R's numeric coercion, including
its warnings when coercion introduces missing values.

The positions can differ between draws, so they cannot generally be used
as a single ordinary subscript. These wrappers mask the base functions
when rvec is attached. Explicit
[`base::which.min()`](https://rdrr.io/r/base/which.min.html) and
[`base::which.max()`](https://rdrr.io/r/base/which.min.html) calls
bypass them.

## Examples

``` r
x <- rvec(rbind(north = c(3, 1), south = c(1, 4), west = c(2, 2)))
as.matrix(which.min(x))
#>      [,1] [,2]
#> [1,]    2    1
as.matrix(which.max(x))
#>      [,1] [,2]
#> [1,]    1    2
```
