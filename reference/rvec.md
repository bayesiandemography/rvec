# Create an Rvec from Data

Create an object of class `"rvec"`, based on input data.

## Usage

``` r
rvec(x)

rvec_chr(x = NULL)

rvec_dbl(x = NULL)

rvec_int(x = NULL)

rvec_lgl(x = NULL)
```

## Arguments

- x:

  A matrix, a list of vectors, an atomic vector, or an rvec.

## Value

An rvec with the following class:

- `rvec_dbl()`: `"rvec_dbl"`

- `rvec_int()`: `"rvec_int"`

- `rvec_lgl()`: `"rvec_lgl"`

- `rvec_chr()`: `"rvec_chr"`

- `rvec()`: `"rvec_chr"`, `"rvec_dbl"` `"rvec_int"`, or `"rvec_lgl"`

## Details

Class `"rvec"` has four subclasses, each dealing with a diffent type:

- `"rvec_dbl"` doubles

- `"rvec_int"` integers

- `"rvec_lgl"` logical

- `"rvec_chr"` character

These subclasses are analogous to
[`double()`](https://rdrr.io/r/base/double.html),
[`integer()`](https://rdrr.io/r/base/integer.html),
[`logical()`](https://rdrr.io/r/base/logical.html), and
[`character()`](https://rdrr.io/r/base/character.html) vectors.

Function `rvec()` chooses the subclass, based on `x`. Functions
`rvec_dbl()`, `rvec_int()`, `rvec_lgl()`, and `rvec_chr()` each create
objects of a particular subclass.

`x` can be

- a matrix, where each row is a set of draws for an unknown quantity;

- a list, where each element is a set of draws;

- an atomic vector, which is treated as a single-column matrix; or

- an rvec.

## See also

- [`new_rvec()`](https://bayesiandemography.github.io/rvec/reference/new_rvec.md)
  Create a blank rvec.

- [`collapse_to_rvec()`](https://bayesiandemography.github.io/rvec/reference/collapse_to_rvec.md)
  Create rvecs within a data frame.

- [`rnorm_rvec()`](https://bayesiandemography.github.io/rvec/reference/dnorm_rvec.md),
  [`rbinom_rvec()`](https://bayesiandemography.github.io/rvec/reference/dbinom_rvec.md),
  etc. Create rvecs representing probability distributions.

## Examples

``` r
m <- rbind(c(-1.5, 2, 0.2),
           c(-2.3, 3, 1.2))
rvec_dbl(m)
#> <rvec_dbl<3>[2]>
#> [1] -1.5,2,0.2 -2.3,3,1.2

l <- list(rpois(100, lambda = 10.2),
          rpois(100, lambda = 5.5))
rvec(l)
#> <rvec_int<100>[2]>
#> [1] 10 (6, 17) 6 (2, 11) 

rvec(letters[1:5])
#> <rvec_chr<1>[5]>
#> [1] "a" "b" "c" "d" "e"

l <- list(a = c(TRUE, FALSE),
          b = c(FALSE, TRUE))
rvec(l)
#> <rvec_lgl<2>[2]>
#>   a   b 
#> T,F F,T 
```
