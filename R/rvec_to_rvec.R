
## Non-exported helper functions for casting between rvecs
##
## Needed because 'vec_cast' does not have the behaviour
## needed for matrices.
##
## @param x An rvec
##
## @returns An rvec


## rvec_to_rvec_chr -----------------------------------------------------------

rvec_to_rvec_chr <- function(x) {
    UseMethod("rvec_to_rvec_chr")
}

## conversion to double always allowed
#' @export
rvec_to_rvec_chr.rvec <- function(x) {
    data_old <- field(x, "data")
    data_new_vec <- as.character(data_old)
    data_new <- array(data_new_vec,
                      dim = dim(data_old),
                      dimnames = dimnames(data_old))
    new_rvec_chr(data_new)
}


## rvec_to_rvec_dbl -----------------------------------------------------------

rvec_to_rvec_dbl <- function(x) {
    UseMethod("rvec_to_rvec_dbl")
}

## no method for rvec_chr

## no method for rvec_dbl

## conversion to double always allowed
#' @export
rvec_to_rvec_dbl.rvec_int <- function(x) {
    data_old <- field(x, "data")
    data_new_vec <- as.double(data_old)
    data_new <- array(data_new_vec,
                      dim = dim(data_old),
                      dimnames = dimnames(data_old))
    new_rvec_dbl(data_new)
}

## conversion to double always allowed
#' @export
rvec_to_rvec_dbl.rvec_lgl <- function(x) {
    data_old <- field(x, "data")
    data_new_vec <- as.double(data_old)
    data_new <- array(data_new_vec,
                      dim = dim(data_old),
                      dimnames = dimnames(data_old))
    new_rvec_dbl(data_new)
}



## rvec_to_rvec_int -----------------------------------------------------------

rvec_to_rvec_int <- function(x) {
    UseMethod("rvec_to_rvec_int")
}

## no method for rvec_chr

## conversion to integer only allowed if information preserved
#' @export
rvec_to_rvec_int.rvec_dbl <- function(x) {
    data_old <- field(x, "data")
    data_old_vec <- as.double(data_old)
    data_new_vec <- vec_cast(x = data_old_vec, to = integer())
    data_new <- array(data_new_vec,
                      dim = dim(data_old),
                      dimnames = dimnames(data_old))
    new_rvec_int(data_new)
}

## no method for rvec_int

## conversion to integer always allowed
#' @export
rvec_to_rvec_int.rvec_lgl <- function(x) {
    data_old <- field(x, "data")
    data_new_vec <- as.integer(data_old)
    data_new <- array(data_new_vec,
                      dim = dim(data_old),
                      dimnames = dimnames(data_old))
    new_rvec_int(data_new)
}


## rvec_to_rvec_lgl -----------------------------------------------------------

rvec_to_rvec_lgl <- function(x) {
    UseMethod("rvec_to_rvec_lgl")
}

## no method for rvec_chr

## conversion to logical only allowed if information preserved
#' @export
rvec_to_rvec_lgl.rvec_dbl <- function(x) {
    data_old <- field(x, "data")
    data_old_vec <- as.double(data_old)
    data_new_vec <- vec_cast(x = data_old_vec, to = logical())
    data_new <- array(data_new_vec,
                      dim = dim(data_old),
                      dimnames = dimnames(data_old))
    new_rvec_lgl(data_new)
}

## conversion to logical only allowed if information preserved
#' @export
rvec_to_rvec_lgl.rvec_int <- function(x) {
    data_old <- field(x, "data")
    data_old_vec <- as.integer(data_old)
    data_new_vec <- vec_cast(data_old_vec, logical())
    data_new <- array(data_new_vec,
                      dim = dim(data_old),
                      dimnames = dimnames(data_old))
    new_rvec_lgl(data_new)
}

## no method for rvec_lgl




