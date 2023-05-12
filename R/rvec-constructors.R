
# for compatibility with the S4 system
methods::setOldClass(c("rvec_chr", "rvec", "vctrs_vctr"))
methods::setOldClass(c("rvec_dbl", "rvec", "vctrs_vctr"))
methods::setOldClass(c("rvec_int", "rvec", "vctrs_vctr"))
methods::setOldClass(c("rvec_lgl", "rvec", "vctrs_vctr"))

## User-visible constructors --------------------------------------------------

## HAS_TESTS
#' Create an rvec
#'
#' Create an object of class `"rvec"`.
#'
#' Class `"rvec"` has four subclasses, each dealing with
#' a diffent type:
#' - `"rvec_dbl"` doubles
#' - `"rvec_int"` integers
#' - `"rvec_lgl"` logical
#' - `"rvec_chr"` character
#'
#' These subclasses are analogous to [double()],
#' [integer()], [logical()], and [character()]
#' vectors.
#'
#' Function `rvec()` chooses the subclass, based on
#' `x`. Functions `rvec_dbl()`, `rvec_int()`,
#' `rvec_lgl()`, and `rvec_chr()` each create
#' objects of a particular subclass.
#' 
#' `x` is typically a matrix, where each row is
#' a set of draws for a particular unknown quantity.
#' However, `x` can also be a vector, which is
#' treated as a single-row matrix. `x` can be
#' doubles, integers, logical, or character.
#'
#' @param x A matrix or vector.
#'
#' @returns
#' An rvec with the following class:
#' - `rvec_dbl()`: `"rvec_dbl"`
#' - `rvec_int()`: `"rvec_int"`
#' - `rvec_lgl()`: `"rvec_lgl"`
#' - `rvec_chr()`: `"rvec_chr"`
#' - `rvec()`: `"rvec_chr"`, `"rvec_dbl"`
#' `"rvec_int"`, or `"rvec_lgl"`, depending on
#' `typeof(x)`.
#'
#' @examples
#' birdweight
#' rvec_dbl(birdweight)
#' rvec(birdweight)
#'
#' dicerolls
#' rvec(dicerolls)
#' @export
rvec <- function(x) {
    if (is.matrix(x)) {
        check_x_has_at_least_one_col(x)
    }
    else if (is.vector(x)) {
        check_x_length_at_least_one(x)
    }
    else
        cli::cli_abort(c("{.arg x} must be a matrix or a vector",
                         "i" = "{.arg x} has class {.cls {class(x)}}"))
    ptype <- vector(mode = typeof(x), length = 0L)
    rvec_inner(x, ptype = ptype)
}

## HAS_TESTS
#' @export
#' @rdname rvec
rvec_chr <- function(x = NULL) {
    rvec_inner(x = x, ptype = character())
}

## HAS_TESTS
#' @export
#' @rdname rvec
rvec_dbl <- function(x = NULL) {
    rvec_inner(x = x, ptype = double())
}

## HAS_TESTS
#' @export
#' @rdname rvec
rvec_int <- function(x = NULL) {
    rvec_inner(x = x, ptype = integer())
}

## HAS_TESTS
#' @export
#' @rdname rvec
rvec_lgl <- function(x = NULL) {
    rvec_inner(x = x, ptype = logical())
}


## Internal constructors ------------------------------------------------------

## HAS_TESTS
new_rvec <- function(data) {
    fun <- get_new_rvec_fun(data)
    fun(data)
}
           
## HAS_TESTS
new_rvec_chr <- function(data) {
    new_rcrd(fields = list(data = data),
             class = c("rvec_chr", "rvec"))
}

## HAS_TESTS
new_rvec_dbl <- function(data) {
    new_rcrd(fields = list(data = data),
             class = c("rvec_dbl", "rvec"))
}

## HAS_TESTS
new_rvec_int <- function(data) {
    new_rcrd(fields = list(data = data),
             class = c("rvec_int", "rvec"))
}

## HAS_TESTS
new_rvec_lgl <- function(data) {
    new_rcrd(fields = list(data = data),
             class = c("rvec_lgl", "rvec"))
}


## 'rvec_inner' ---------------------------------------------------------------

#' Workhorse for 'rvec'
#'
#' @param x A matrix, a vector, or NULL
#'
#' @returns Object of class "rvec"
#'
#' @noRd
rvec_inner <- function(x, ptype) {
    if (is.null(x))
        data <- matrix(ptype, nrow = 0L, ncol = 1L)
    else if (is.matrix(x)) {
        check_x_has_at_least_one_col(x)
        ptype <- matrix(ptype, nrow = 0L, ncol = ncol(x))
        data <- vec_cast(x, to = ptype)
    }
    else if (is.character(x)
             || is.double(x)
             || is.integer(x)
             || is.logical(x)) {
        check_x_length_at_least_one(x)
        data <- vec_cast(x, to = ptype)
        data <- matrix(data, nrow = 1L)
    }
    else
        cli::cli_abort(c(paste("{.arg x} must be a matrix, a character, double,",
                               "integer, or logical vector, or NULL}"),
                         "i" = "{.arg x} has class {.cls {class(x)}}"))
    fun <- get_new_rvec_fun(ptype)
    fun(data)
}
