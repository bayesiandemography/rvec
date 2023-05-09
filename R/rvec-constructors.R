
## User-visible constructors --------------------------------------------------

#' Create a random vector
#'
#' @description
#' Create a vector of class `"rvec"`. Class
#' `"rvec"` has four subclasses, each dealing with
#' a diffent type:
#' - `"rvec_dbl"` doubles
#' - `"rvec_int"` integers
#' - `"rvec_lgl"` logical
#' - `"rvec_chr"` character
#'
#' Argument `x` is a matrix in which each
#' row represents a set of draws for a particular
#' quantity.
#'
#' @param x A matrix.
#'
#' @returns
#' A random vector with the following class:
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
        check_x_at_least_length_one(x)
    }
    else
        cli::cli_abort(c("{.arg x} must be a matrix or a vector",
                         "i" = "{.arg x} has class {.cls {class(x)}}"))
    new_rvec(x)
}


#' @export
#' @rdname rvec
rvec_chr <- function(x = NULL) {
    rvec_inner(x = x, ptype = character())
}

#' @export
#' @rdname rvec
rvec_dbl <- function(x = NULL) {
    rvec_inner(x = x, ptype = double())
}

#' @export
#' @rdname rvec
rvec_int <- function(x = NULL) {
    rvec_inner(x = x, ptype = integer())
}

#' @export
#' @rdname rvec
rvec_lgl <- function(x = NULL) {
    rvec_inner(x = x, ptype = logical())
}





## Internal constructors ------------------------------------------------------

new_rvec <- function(data) {
    fun <- get_new_rvec_fun(data)
    fun(data)
}
           
new_rvec_chr <- function(data) {
    new_rcrd(fields = list(data = data),
             class = c("rvec_chr", "rvec"))
}

new_rvec_dbl <- function(data) {
    new_rcrd(fields = list(data = data),
             class = c("rvec_dbl", "rvec"))
}

new_rvec_int <- function(data) {
    new_rcrd(fields = list(data = data),
             class = c("rvec_int", "rvec"))
}

new_rvec_lgl <- function(data) {
    new_rcrd(fields = list(data = data),
             class = c("rvec_lgl", "rvec"))
}


# for compatibility with the S4 system
methods::setOldClass(c("rvec_chr", "rvec", "vctrs_vctr"))
methods::setOldClass(c("rvec_dbl", "rvec", "vctrs_vctr"))
methods::setOldClass(c("rvec_int", "rvec", "vctrs_vctr"))
methods::setOldClass(c("rvec_lgl", "rvec", "vctrs_vctr"))
             
