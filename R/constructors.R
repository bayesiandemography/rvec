
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
#' However, `x` can also be a list of vectors,
#' all of the same length.
#'
#' @param x A matrix or a list of vectors.
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
    if (is.matrix(x))
        check_x_has_at_least_one_col(x)
    else if (is.list(x)) {
        if (length(x) == 0L)
            cli::cli_abort("If {.arg x} is a list, it must have at least one element.")
        check_lengths_nonzero(x)
        check_lengths_equal(x)
        x <- do.call(rbind, args = x)
    }        
    else
        cli::cli_abort(c("{.arg x} must be a matrix or a list",
                         i = "{.arg x} has class {.cls {class(x)}}"))
    colnames(x) <- NULL
    if (is.character(x))
        new_rvec_chr(x)
    else if (is.double(x))
        new_rvec_dbl(x)
    else if (is.integer(x))
        new_rvec_int(x)
    else if (is.logical(x))
        new_rvec_lgl(x)
    else
        cli::cli_abort(c("{.arg x} must be double, integer, logical, or character",
                         i = "{.arg x} has type {typeof(x)}"))

}

## HAS_TESTS
#' @export
#' @rdname rvec
rvec_chr <- function(x = NULL) {
    if (is.null(x))
        data <- matrix(character(), nrow = 0, ncol = 1L)
    else if (is.matrix(x)) {
        check_x_has_at_least_one_col(x)
        data_vec <- as.character(x)
        data <- matrix(data_vec,
                       nrow = nrow(x),
                       ncol = ncol(x))
        rownames(data) <- rownames(x)
    }
    else if (is.list(x)) {
        if (length(x) > 0L) {
            check_lengths_nonzero(x)
            check_lengths_equal(x)
            data_vec <- unlist(x)
            data_vec <- as.character(data_vec)
            data <- matrix(data_vec,
                           nrow = length(x),
                           ncol = length(x[[1L]]))
            rownames(data) <- names(x)
        }
        else
            data <- matrix(character(), nrow = 0, ncol = 1L)
    }
    
    else
        cli::cli_abort(c("{.arg x} must be a matrix, a list, or NULL}",
                         i = "{.arg x} has class {.cls {class(x)}}"))
    new_rvec_chr(data)
}

## HAS_TESTS
#' @export
#' @rdname rvec
rvec_dbl <- function(x = NULL) {
    if (is.null(x))
        data <- matrix(double(), nrow = 0, ncol = 1L)
    else if (is.matrix(x)) {
        check_x_has_at_least_one_col(x)
        data_vec <- as.vector(x)
        data_vec <- vec_cast(data_vec, double())
        data <- matrix(data_vec,
                       nrow = nrow(x),
                       ncol = ncol(x))
        rownames(data) <- rownames(x)
    }
    else if (is.list(x)) {
        if (length(x) > 0L) {
            check_lengths_nonzero(x)
            check_lengths_equal(x)
            data_vec <- unlist(x)
            data_vec <- vec_cast(data_vec, double())
            data <- matrix(data_vec,
                           nrow = length(x),
                           ncol = length(x[[1L]]))
            rownames(data) <- names(x)
        }
        else
            data <- matrix(double(), nrow = 0, ncol = 1L)
    }
    else
        cli::cli_abort(c("{.arg x} must be a matrix, a list, or NULL}",
                         i = "{.arg x} has class {.cls {class(x)}}"))
    new_rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @rdname rvec
rvec_int <- function(x = NULL) {
    if (is.null(x))
        data <- matrix(integer(), nrow = 0, ncol = 1L)
    else if (is.matrix(x)) {
        check_x_has_at_least_one_col(x)
        data_vec <- as.vector(x)
        data_vec <- vec_cast(data_vec, integer())
        data <- matrix(data_vec,
                       nrow = nrow(x),
                       ncol = ncol(x))
        rownames(data) <- rownames(x)
    }
    else if (is.list(x)) {
        if (length(x) > 0L) {
            check_lengths_nonzero(x)
            check_lengths_equal(x)
            data_vec <- unlist(x)
            data_vec <- vec_cast(data_vec, integer())
            data <- matrix(data_vec,
                           nrow = length(x),
                           ncol = length(x[[1L]]))
            rownames(data) <- names(x)
        }
        else
            data <- matrix(integer(), nrow = 0, ncol = 1L)
    }
    else
        cli::cli_abort(c("{.arg x} must be a matrix, a list, or NULL}",
                         i = "{.arg x} has class {.cls {class(x)}}"))
    new_rvec_int(data)
}

## HAS_TESTS
#' @export
#' @rdname rvec
rvec_lgl <- function(x = NULL) {
    if (is.null(x))
        data <- matrix(logical(), nrow = 0, ncol = 1L)
    else if (is.matrix(x)) {
        check_x_has_at_least_one_col(x)
        data_vec <- as.vector(x)
        data_vec <- vec_cast(data_vec, logical())
        data <- matrix(data_vec,
                       nrow = nrow(x),
                       ncol = ncol(x))
        rownames(data) <- rownames(x)
    }
    else if (is.list(x)) {
        if (length(x) > 0L) {
            check_lengths_nonzero(x)
            check_lengths_equal(x)
            data_vec <- unlist(x)
            data_vec <- vec_cast(data_vec, logical())
            data <- matrix(data_vec,
                           nrow = length(x),
                           ncol = length(x[[1L]]))
            rownames(data) <- names(x)
        }
        else
            data <- matrix(logical(), nrow = 0, ncol = 1L)
    }
    else
        cli::cli_abort(c("{.arg x} must be a matrix, a list, or NULL}",
                         i = "{.arg x} has class {.cls {class(x)}}"))
    new_rvec_lgl(data)
}


## Internal constructors ------------------------------------------------------

## HAS_TESTS
new_rvec <- function(data) {
    if (is.character(data))
        new_rvec_chr(data)
    else if (is.double(data))
        new_rvec_dbl(data)
    else if (is.integer(data))
        new_rvec_int(data)
    else if (is.logical(data))
        new_rvec_lgl(data)
    else
        cli::cli_abort("Internal error: {.arg data} has type {typeof(data)}.")
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
