# Posterior Sample from Linear Regression

Posterior sample for parameters from a linear regression model.

## Usage

``` r
reg_post
```

## Format

A matrix with 200 columns and the following rows:

- `alpha`: Intercept parameter

- `beta`: Slope parameter

- `sigma`: Standard deviation of error term

## Source

`reg_post` contains values from the second half of the `line` dataset in
package [coda](https://CRAN.R-project.org/package=coda). The line
dataset draws on the BUGS manual: Spiegelhalter, D.J., Thomas, A., Best,
N.G. and Gilks, W.R. (1995) BUGS: Bayesian inference using Gibbs
Sampling, Version 0.5, MRC Biostatistics Unit, Cambridge.
