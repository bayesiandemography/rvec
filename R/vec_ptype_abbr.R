
## HAS_TESTS
#' @export
vec_ptype_abbr.rvec_chr <- function(x, ...) {
    n <- n_draw(x)
    sprintf("rchr<%d>", n)
}

## HAS_TESTS
#' @export
vec_ptype_abbr.rvec_dbl <- function(x, ...) {
    n <- n_draw(x)
    sprintf("rdbl<%d>", n)
}

## HAS_TESTS
#' @export
vec_ptype_abbr.rvec_int <- function(x, ...) {
    n <- n_draw(x)
    sprintf("rint<%d>", n)
}

## HAS_TESTS
#' @export
vec_ptype_abbr.rvec_lgl <- function(x, ...) {
    n <- n_draw(x)
    sprintf("rlgl<%d>", n)
}
