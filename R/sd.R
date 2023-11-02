
## based on
## https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Adding-new-generics


#' Standard Deviation, Including Rvecs
#'
#' Calculate standard deviation of `x`, where `x` can be
#' an rvec. If `x` is an rvec, separate standard deviations
#' are calculated for each draw.
#'
#' To enable different behavior for rvecs and for ordinary vectors,
#' the base R function [stats::sd()] is turned into a generic,
#' with [stats::sd()] as the default.
#'
#' For details on the calculations, see the documentation
#' for [stats::sd()].
#'
#' @param x A numeric vector or R object, including an [rvec::rvec()].
#' @param na.rm Whether to remove `NA`s before calculating standard deviations.
#'
#' @returns An rvec, if `x` is an rvec. Otherwise
#' typically a numeric vector.
#'
#' @seealso [rvec::var()]
#'
#' @examples
#' x <- rvec(cbind(rnorm(10), rnorm(10, sd = 20)))
#' x
#' sd(x)
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
