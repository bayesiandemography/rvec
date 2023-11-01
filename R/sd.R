
## based on
## https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Adding-new-generics

#' @export
sd <- function(x, na.rm = FALSE) {
    UseMethod("sd")
}

## HAS_TESTS
#' @export
sd.default <- function(x, na.rm = FALSE) {
    stats::sd(x, na.rm = na.rm)
}

## HAS_TESTS
#' @export
sd.rvec <- function(x, na.rm = FALSE) {
    m <- 1 * field(x, "data")
    data <- matrixStats::colSds(m, na.rm = na.rm)
    data <- matrix(data, nrow = 1L)
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
sd.rvec_chr <- function(x, na.rm = FALSE) {
    cli::cli_abort("Standard deviation not defined for character vectors.")
}    
