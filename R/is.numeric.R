

## HAS_TESTS

#' @export
#' @method is.numeric rvec
is.numeric.rvec <- function(x) FALSE

#' @export
#' @method is.numeric rvec_dbl
is.numeric.rvec_dbl <- function(x) TRUE

#' @export
#' @method is.numeric rvec_int
is.numeric.rvec_int <- function(x) TRUE

