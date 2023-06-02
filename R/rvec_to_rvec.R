
## Non-exported helper functions for casting between rvecs
##
## Needed because 'vec_cast' does not have the behaviour
## needed for matrices.
##
## @param x An rvec
## @param n_draw Number of draws
##
## @returns An rvec


## rvec_to_rvec_chr -----------------------------------------------------------

rvec_to_rvec_chr <- function(x, n_draw) {
    UseMethod("rvec_to_rvec_chr")
}

## HAS_TESTS
## conversion to character always allowed
#' @export
rvec_to_rvec_chr.rvec <- function(x, n_draw) {
    data_old <- field(x, "data")
    data_new_vec <- as.character(data_old)
    data_new <- data_matrix(data_new_vec = data_new_vec,
                            data_old = data_old,
                            n_draw = n_draw)
    new_rvec_chr(data_new)
}


## rvec_to_rvec_dbl -----------------------------------------------------------

rvec_to_rvec_dbl <- function(x, n_draw) {
    UseMethod("rvec_to_rvec_dbl")
}

## no method for rvec_chr


## HAS_TESTS
## conversion to double always allowed
#' @export
rvec_to_rvec_dbl.rvec_dbl <- function(x, n_draw) {
    data_old <- field(x, "data")
    data_new_vec <- as.double(data_old)
    data_new <- data_matrix(data_new_vec = data_new_vec,
                            data_old = data_old,
                            n_draw = n_draw)
    new_rvec_dbl(data_new)
}

## HAS_TESTS
## conversion to double always allowed
#' @export
rvec_to_rvec_dbl.rvec_int <- function(x, n_draw) {
    data_old <- field(x, "data")
    data_new_vec <- as.double(data_old)
    data_new <- data_matrix(data_new_vec = data_new_vec,
                            data_old = data_old,
                            n_draw = n_draw)
    new_rvec_dbl(data_new)
}

## HAS_TESTS
## conversion to double always allowed
#' @export
rvec_to_rvec_dbl.rvec_lgl <- function(x, n_draw) {
    data_old <- field(x, "data")
    data_new_vec <- as.double(data_old)
    data_new <- data_matrix(data_new_vec = data_new_vec,
                            data_old = data_old,
                            n_draw = n_draw)
    new_rvec_dbl(data_new)
}



## rvec_to_rvec_int -----------------------------------------------------------

rvec_to_rvec_int <- function(x, n_draw) {
    UseMethod("rvec_to_rvec_int")
}

## HAS_TESTS
## conversion to integer only allowed if information preserved
#' @export
rvec_to_rvec_int.rvec_dbl <- function(x, n_draw) {
    data_old <- field(x, "data")
    data_old_vec <- as.double(data_old)
    data_new_vec <- vec_cast(x = data_old_vec, to = integer())
    data_new <- data_matrix(data_new_vec = data_new_vec,
                            data_old = data_old,
                            n_draw = n_draw)
    new_rvec_int(data_new)
}

## HAS_TESTS
## conversion to integer only allowed if information preserved
#' @export
rvec_to_rvec_int.rvec_int <- function(x, n_draw) {
    data_old <- field(x, "data")
    data_new_vec <- as.integer(data_old)
    data_new <- data_matrix(data_new_vec = data_new_vec,
                            data_old = data_old,
                            n_draw = n_draw)
    new_rvec_int(data_new)
}

## no method for rvec_int

## HAS_TESTS
## conversion to integer always allowed
#' @export
rvec_to_rvec_int.rvec_lgl <- function(x, n_draw) {
    data_old <- field(x, "data")
    data_new_vec <- as.integer(data_old)
    data_new <- data_matrix(data_new_vec = data_new_vec,
                            data_old = data_old,
                            n_draw = n_draw)
    new_rvec_int(data_new)
}

## rvec_to_rvec_lgl -----------------------------------------------------------

rvec_to_rvec_lgl <- function(x, n_draw) {
    UseMethod("rvec_to_rvec_lgl")
}

## no method for rvec_chr

## HAS_TESTS
## conversion to logical only allowed if information preserved
#' @export
rvec_to_rvec_lgl.rvec_dbl <- function(x, n_draw) {
    data_old <- field(x, "data")
    data_old_vec <- as.double(data_old)
    data_new_vec <- vec_cast(x = data_old_vec, to = logical())
    data_new <- data_matrix(data_new_vec = data_new_vec,
                            data_old = data_old,
                            n_draw = n_draw)
    new_rvec_lgl(data_new)
}

## HAS_TESTS
## conversion to logical only allowed if information preserved
#' @export
rvec_to_rvec_lgl.rvec_int <- function(x, n_draw) {
    data_old <- field(x, "data")
    data_old_vec <- as.integer(data_old)
    data_new_vec <- vec_cast(data_old_vec, logical())
    data_new <- data_matrix(data_new_vec = data_new_vec,
                            data_old = data_old,
                            n_draw = n_draw)
    new_rvec_lgl(data_new)
}

## HAS_TESTS
#' @export
rvec_to_rvec_lgl.rvec_lgl <- function(x, n_draw) {
    data_old <- field(x, "data")
    data_new_vec <- as.logical(data_old)
    data_new <- data_matrix(data_new_vec = data_new_vec,
                            data_old = data_old,
                            n_draw = n_draw)
    new_rvec_lgl(data_new)
}

## Helper functions -----------------------------------------------------------

## HAS_TESTS
#' Construct data to be used in new rvec
#'
#' @param data_new_vec Vector with data,
#' which may have been coerced to new mode.
#' May be recycled.
#' @param data_old Old data matrix.
#' @param n_draw Target number of draws.
#' 
#' @returns A matrix
#'
#' @noRd
data_matrix <- function(data_new_vec, data_old, n_draw) {
    nrow <- nrow(data_old)
    ans <- matrix(data_new_vec,
                  nrow = nrow,
                  ncol = n_draw)
    rownames(ans) <- rownames(data_old)
    ans
}
