# Divorce Rates in New Zealand

Posterior sample from a model of divorce rates in New Zealand.

## Usage

``` r
divorce
```

## Format

A tibble with 30,000 rows and the following variables:

- `age`: Age, in 5-year age groups, 15-19 to 65+.

- `sex`: `"Female"` or `"Male"`.

- `draw`: Index for random draw.

- `rate`: Divorce rate, per 1000.

## Source

Derived from data in tables "Age at divorces by sex (marriages and civil
unions) (Annual-Dec)" and "Estimated Resident Population by Age and Sex
(1991+) (Annual-Dec)" in the online database Infoshare on the Statistics
New Zealand website, downloaded on 22 March 2023.
