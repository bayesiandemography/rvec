# Calculate Probabilities from Random Draws

Convert an rvec of logical values (an
[rvec_lgl](https://bayesiandemography.github.io/rvec/reference/rvec.md))
into a vector of probabilities.

## Usage

``` r
prob(x, na_rm = FALSE)

# S3 method for class 'rvec_lgl'
prob(x, na_rm = FALSE)

# S3 method for class 'logical'
prob(x, na_rm = FALSE)
```

## Arguments

- x:

  An object of class
  [rvec_lgl](https://bayesiandemography.github.io/rvec/reference/rvec.md).

- na_rm:

  Whether to remove NAs before calculating summaries. Default is
  `FALSE`.

## Value

A logical vector with the same length as `x`.

## Details

`prob()` is essentially just
[`draws_mean()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)
with a different name. The proportion of draws that are `TRUE` is used
as an estimate of the underlying probability. The different name can
make the intent of the code clearer.

## See also

- [`draws_mean()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)
  Means across draws. Gives the same result as `prob` when applied to
  logical rvecs.

## Examples

``` r
m <- rbind(c(FALSE,  TRUE),
           c(TRUE,   TRUE),
           c(FALSE,  FALSE))
x <- rvec(m)
x
#> <rvec_lgl<2>[3]>
#> [1] F,T T,T F,F
prob(x)
#> [1] 0.5 1.0 0.0

## logical rvec created on the fly
## through operations such as '>'
m <- rbind(c(-1,  1.3, 2),
           c(2, 0.1, -1),
           c(Inf, 0, -0.5))
y <- rvec(m)
y
#> <rvec_dbl<3>[3]>
#> [1] -1,1.3,2   2,0.1,-1   Inf,0,-0.5
prob(y > 0)
#> [1] 0.6666667 0.6666667 0.3333333
prob(y >= 0)
#> [1] 0.6666667 0.6666667 0.6666667
prob(y^2 > 0)
#> [1] 1.0000000 1.0000000 0.6666667
```
