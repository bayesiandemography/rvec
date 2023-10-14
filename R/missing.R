
#' Missing, Finite, and Infinite Values
#' in Rvecs
#'
#' @description
#' rvec methods for `is.na()`, `is.nan()`,
#' `is.finite()`, and `is.infinite()` all
#' return rvecs, with separate values for
#' every draw.
#'
#' The rvec method for `anyNA()` returns
#' `TRUE` if any value for any draw is
#' `NA` or `NaN`, and returns `FALSE` otherwise.
#'
#' Functions `na.omit()` and `na.exclude()`
#' return versions of `x` in which observations
#' with `NA` in any draws are omitted.
#' 
#'
#' @details
#' The behavior of the rvec methods
#' for `is.na()`, `is.nan()`,
#' `is.finite()`, and `is.infinite()`
#' differs from the standard
#' [vctrs](https://vctrs.r-lib.org)
#' behavior, which is to return a logical
#' vector with length equal to `length(x)`.
#' With rvecs, the standard **vctrs** behavior
#' would entail summarising across draws,
#' which is the job of the [draws_*][draws_all()]
#' functions.
#'
#' @param x,object An [rvec][rvec()].
#' @param recursive Whether `anyNA()` should be
#' applied recursively to lists. Ignored when
#' `x` is an rvec.
#' @param ... Currently ignored.
#'
#' @seealso
#' - [if_else_rvec()] for modifying individual
#'   values within draws.
#' - Base R functions [is.na()], [is.nan()],
#'   [is.finite()], [is.infinite()],
#'   [anyNA()], [na.omit()], [na.exclude()]
#' - [vctrs::vec_detect_missing()] to test whether
#'   all draws for an observation are missing.
#' - [vctrs::vec_detect_complete()] to test whether
#'   any draws for an observation are missing.
#' 
#' 
#' @examples
#' x <- rvec(list(c(1.2, NA),
#'                c(Inf, 3),
#'                c(-1, NaN)))
#'
#' ## return a logical rvec
#' is.na(x)
#' is.nan(x)
#' is.finite(x)
#' is.infinite(x)
#'
#' ## return a logical scalar
#' anyNA(x)
#'
#' ## return an NA-free version of 'x'
#' na.omit(x)
#' na.exclude(x)
#'
#' ## use 'if_else_rvec' to modify values
#' ## within rvec
#' if_else_rvec(is.na(x), 999, x)
#'
#' ## vctrs functions
#' library(vctrs, warn.conflicts = FALSE)
#' ## all draws missing
#' vec_detect_missing(x)
#' ## any draws missing
#' vec_detect_complete(x)
#' @name missing
NULL

## HAS_TESTS
#' @rdname missing
#' @export
anyNA.rvec <- function(x, recursive = FALSE) {
    m <- field(x, "data")
    anyNA(m)
}

## HAS_TESTS
#' @rdname missing
#' @export
is.na.rvec <- function(x) {
    m <- field(x, "data")
    data <- is.na(m)
    rvec_lgl(data)
}

## HAS_TESTS
## modified from na.exclude.vctrs_vctr
#' @rdname missing
#' @importFrom stats na.exclude
#' @export
na.exclude.rvec <- function(object, ...) {
  na_remove_rvec(object, class_loc = "exclude")
}

## HAS_TESTS
## modified from na.fail.vctrs_vctr
#' @importFrom stats na.fail
#' @export
na.fail.rvec <- function(object, ...) {
    m <- field(object, "data")
    if (anyNA(m)) {
        ## return the same error as `na.fail.default()`
        stop("missing values in object")
    }
    object
}

## HAS_TESTS
## modified from na.omit.vctrs_vctr
#' @rdname missing
#' @importFrom stats na.omit
#' @export
na.omit.rvec <- function(object, ...) {
  na_remove_rvec(object, class_loc = "omit")
}


## 'is.nan()', 'is.finite()', and 'is.infinite()'
## implemented as part of 'vec_math()'


## Helper functions -----------------------------------------------------------

## HAS_TESTS
#' Modified from vctrs::na_remove
#' @noRd
na_remove_rvec <- function(x, class_loc) {
    m <- field(x, "data")
    if (!anyNA(m)) {
        return(x)
    }
    is_any_missing <- matrixStats::rowAnyNAs(m)
    loc <- which(is_any_missing)
    attr(loc, "class") <- class_loc
    ans <- m[!is_any_missing, , drop = FALSE]
    ans <- rvec(ans)
    attr(ans, "na.action") <- loc
    ans
}
