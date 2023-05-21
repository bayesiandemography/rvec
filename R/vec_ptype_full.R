
## HAS_TESTS
#' @export
vec_ptype_full.rvec_chr <- function(x, ...) {
    n <- n_draw(x)
    sprintf("rvec_chr<%d>", n)
}

## HAS_TESTS
#' @export
vec_ptype_full.rvec_dbl <- function(x, ...) {
    n <- n_draw(x)
    sprintf("rvec_dbl<%d>", n)
}

## HAS_TESTS
#' @export
vec_ptype_full.rvec_int <- function(x, ...) {
    n <- n_draw(x)
    sprintf("rvec_int<%d>", n)
}

## HAS_TESTS
#' @export
vec_ptype_full.rvec_lgl <- function(x, ...) {
    n <- n_draw(x)
    sprintf("rvec_lgl<%d>", n)
}
