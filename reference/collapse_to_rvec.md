# Convert a Data Frame Between 'Draws-and-Values' Format and 'Rvec' Format

`collapse_to_rvec()` converts a data frame from a 'draws-and-values'
format to an 'rvec' format. `expand_from_rvec()`, does the opposite,
converting a data frame from an rvecs format to a draws-and-values
format.

## Usage

``` r
collapse_to_rvec(data, draw = draw, values = value, by = NULL, type = NULL)

# S3 method for class 'data.frame'
collapse_to_rvec(data, draw = draw, values = value, by = NULL, type = NULL)

# S3 method for class 'grouped_df'
collapse_to_rvec(data, draw = draw, values = value, by = NULL, type = NULL)

expand_from_rvec(data, draw = "draw")

# S3 method for class 'data.frame'
expand_from_rvec(data, draw = "draw")

# S3 method for class 'grouped_df'
expand_from_rvec(data, draw = "draw")
```

## Arguments

- data:

  A data frame, possibly
  [grouped](https://dplyr.tidyverse.org/reference/group_data.html).

- draw:

  \<[`tidyselect`](https://tidyselect.r-lib.org/reference/language.html)\>
  The variable that uniquely identifies random draws within each
  combination of values for the 'by' variables. Must be quoted for
  `expand_from_rvec()`.

- values:

  \<[`tidyselect`](https://tidyselect.r-lib.org/reference/language.html)\>
  One or more variables in `data` that hold measurements.

- by:

  \<[`tidyselect`](https://tidyselect.r-lib.org/reference/language.html)\>
  Variables used to stratify or cross-classify the data. See Details.

- type:

  String specifying the class of rvec to use for each variable.
  Optional. See Details.

## Value

A data frame.

- `collapse_to_rvec()` **reduces** the number of rows by a factor of
  [`n_draw()`](https://bayesiandemography.github.io/rvec/reference/n_draw.md).

- `expand_from_rvec()` **increases** the number of rows by a factor of
  [`n_draw()`](https://bayesiandemography.github.io/rvec/reference/n_draw.md).

- `collapse_to_rvec()` silently drops all variables that are not draw,
  value, or grouping variables if `data` is a
  [grouped](https://dplyr.tidyverse.org/reference/group_data.html) data
  frame.

## Details

In a draws-and-values format, each row represents one random draw. The
data frame contains a 'draw' variable that distinguishes different draws
within the same combination of 'by' variables. In rvec format, each row
represents one combination of 'by' variables, and multiple draws are
stored in an
[rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md). See
below for examples.

## `by` argument

The `by` argument is used to specify stratifying variables. For instance
if `by` includes `sex` and `age`, then data frame produced by
`collapse_to_rvec()` has separate rows for each combination of `sex` and
`age`.

If `data` is a
[grouped](https://dplyr.tidyverse.org/reference/group_data.html) data
frame, then the grouping variables take precedence over `by`.

If no value for `by` is provided, and `data` is not a grouped data
frame, then `collapse_to_rvec()` assumes that all variables in `data`
that are not included in `value` and `draw` should be included in `by`.

## `type` argument

By default, `collapse_to_rvec()` calls function
[`rvec()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
on each values variable in `data`.
[`rvec()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
chooses the class of the output (ie `rvec_chr`, `rvec_dbl`, `rvec_int`,
or `rvec_lgl`) depending on the input. Types can instead be specified in
advance, using the `type` argument. `type` is a string, each character
of which specifies the class of the corresponding values variable. The
characters have the following meanings:

- `"c"`: `rvec_chr`

- `"d"`: `rvec_dbl`

- `"i"`: `rvec_int`

- `"l"`: `rvec_lgl`

- `"?"`: Depends on inputs.

The codes for `type` are modified from ones used by the
[readr](https://readr.tidyverse.org) package.

## See also

- [`rvec()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
  Construct a single rvec

- [`as_list_col()`](https://bayesiandemography.github.io/rvec/reference/as_list_col.md)
  Convert an rvec to a list variable

- [dplyr::group_vars()](https://dplyr.tidyverse.org/reference/group_data.html)
  Names of grouping variables

`collapse_to_rvec()` and `expand_from_rvec()` are analogous to
[tidyr::nest()](https://tidyr.tidyverse.org/reference/nest.html) and
[tidyr::unnest()](https://tidyr.tidyverse.org/reference/unnest.html)
though `collapse_to_rvec()` and `expand_from_rvec()` move values into
and out of rvecs, while
[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html) and
[`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html)
move them in and out of data frames.
([`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html) and
[`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html)
are also a lot more flexible.)

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
data_db <- tribble(
  ~occupation,    ~sim, ~pay,
  "Statistician", 1,    100,
  "Statistician", 2,    80,
  "Statistician", 3,    105,
  "Banker",       1,    400,
  "Banker",       2,    350,
  "Banker",       3,    420
)

## draws-and-values format to rvec format
data_rv <- data_db |>
  collapse_to_rvec(draw = sim,
                   values = pay)
data_rv
#> # A tibble: 2 × 2
#>   occupation           pay
#>   <chr>          <rdbl<3>>
#> 1 Statistician  100,80,105
#> 2 Banker       400,350,420

## rvec format to draws-and-values format
data_rv |>
  expand_from_rvec()
#> # A tibble: 6 × 3
#>   occupation    draw   pay
#>   <chr>        <int> <dbl>
#> 1 Statistician     1   100
#> 2 Statistician     2    80
#> 3 Statistician     3   105
#> 4 Banker           1   400
#> 5 Banker           2   350
#> 6 Banker           3   420

## provide a name for the draw variable
data_rv |>
  expand_from_rvec(draw = "sim")
#> # A tibble: 6 × 3
#>   occupation     sim   pay
#>   <chr>        <int> <dbl>
#> 1 Statistician     1   100
#> 2 Statistician     2    80
#> 3 Statistician     3   105
#> 4 Banker           1   400
#> 5 Banker           2   350
#> 6 Banker           3   420

## specify that rvec variable
## must be rvec_int
data_rv <- data_db |>
  collapse_to_rvec(draw = sim,
                   values = pay,
                   type = "i")

## specify stratifying variable explicitly,
## using 'by' argument
data_db |>
  collapse_to_rvec(draw = sim,
                   values = pay,
                   by = occupation)
#> # A tibble: 2 × 2
#>   occupation           pay
#>   <chr>          <rdbl<3>>
#> 1 Statistician  100,80,105
#> 2 Banker       400,350,420

## specify stratifying variable explicitly,
## using 'group_by'
library(dplyr)
data_db |>
  group_by(occupation) |>
  collapse_to_rvec(draw = sim,
                   values = pay)
#> # A tibble: 2 × 2
#> # Groups:   occupation [2]
#>   occupation           pay
#>   <chr>          <rdbl<3>>
#> 1 Statistician  100,80,105
#> 2 Banker       400,350,420
```
