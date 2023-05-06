
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
    data <- matrix_to_list_of(x, .ptype = NULL)
    new_rvec(data)
}


#' @export
#' @rdname rvec
rvec_chr <- function(x = matrix(character())) {
    data <- matrix_to_list_of(x, .ptype = character())
    new_rvec_chr(data)
}

#' @export
#' @rdname rvec
rvec_dbl <- function(x = matrix(double())) {
    data <- matrix_to_list_of(x, .ptype = double())
    new_rvec_dbl(data)
}

#' @export
#' @rdname rvec
rvec_int <- function(x = matrix(integer())) {
    data <- matrix_to_list_of(x, .ptype = integer())
    new_rvec_int(data)
}

#' @export
#' @rdname rvec
rvec_lgl <- function(x = matrix(logical())) {
    data <- matrix_to_list_of(x, .ptype = logical())
    new_rvec_lgl(data)
}


## Internal constructors ------------------------------------------------------

## Low-level constructors

#' Create new 'rvec' object with class
#' dependent on inputs
#'
#' @param data Object of class "list_of".
#'
#' @returns Object belonging to a subclass of `"rvec"`
#'
#' @noRd
new_rvec <- function(data) {
    ptype <- attr(data, "ptype")
    type <- typeof(ptype)
    fun <- switch(type,
                  character = new_rvec_chr,
                  double = new_rvec_dbl,
                  integer = new_rvec_int,
                  logical = new_rvec_lgl,
                  cli::cli_abort("Internal error: Can't handle type {.class type}"))
    fun(data)
}
           

#' Create new "rvec_chr" object
#'
#' Create a new object of class `"rvec_chr"`.
#'
#' @param data Object of class "list_of",
#' with a character prototype.
#'
#' @return An object of class `"rvec_chr"`.
#'
#' @noRd
new_rvec_chr <- function(data = list_of(.ptype = character())) {
    new_rcrd(fields = list(data = data),
             class = c("rvec_chr", "rvec"))
}


#' Create new "rvec_dbl" object
#'
#' Create a new object of class `"rvec_dbl"`.
#'
#' @param data Object of class "list_of",
#' with a double prototype.
#' 
#' @return An object of class `"rvec_dbl"`.
#'
#' @noRd
new_rvec_dbl <- function(data = list_of(.ptype = double())) {
    new_rcrd(fields = list(data = data),
             class = c("rvec_dbl", "rvec"))
}


#' Create new "rvec_int" object
#'
#' Create a new object of class `"rvec_int"`.
#'
#' @param data Object of class "list_of",
#' with an integer prototype.
#'
#' @return An object of class `"rvec_int"`.
#'
#' @noRd
new_rvec_int <- function(data = list_of(.ptype = integer())) {
    new_rcrd(fields = list(data = data),
             class = c("rvec_int", "rvec"))
}


#' Create new "rvec_lgl" object
#'
#' Create a new object of class `"rvec_lgl"`.
#'
#' @param data Object of class "list_of",
#' with a logical prototype.
#'
#' @return An object of class `"rvec_lgl"`.
#'
#' @noRd
new_rvec_lgl <- function(data = list_of(.ptype = logical())) {
    new_rcrd(fields = list(data = data),
             class = c("rvec_lgl", "rvec"))
}


# for compatibility with the S4 system
methods::setOldClass(c("rvec_chr", "rvec", "vctrs_vctr"))
methods::setOldClass(c("rvec_dbl", "rvec", "vctrs_vctr"))
methods::setOldClass(c("rvec_int", "rvec", "vctrs_vctr"))
methods::setOldClass(c("rvec_lgl", "rvec", "vctrs_vctr"))
             
