
## Non-exported helper functions for casting
## from atomic vectors to rvecs
##
## Needed because 'vec_cast' does not have the behaviour
## needed for 'n_draw'.
##
## @param x A character, double, integer, or logical vector.
##
## @returns An rvec


## atomic_to_rvec_chr ---------------------------------------------------------

atomic_to_rvec_chr <- function(x, n_draw) {
    UseMethod("atomic_to_rvec_chr")
}


## HAS_TESTS
#' @export
atomic_to_rvec_chr.character <- function(x, n_draw) {
    data <- matrix(x,
                   nrow = length(x),
                   ncol = n_draw)
    rownames(data) <- names(x)
    new_rvec_chr(data)
}

## conversion to character always allowed
#' @export
atomic_to_rvec_chr.double <- function(x, n_draw) {
    data <- matrix(as.character(x),
                   nrow = length(x),
                   ncol = n_draw)
    rownames(data) <- names(x)
    new_rvec_chr(data)
}

## conversion to character always allowed
#' @export
atomic_to_rvec_chr.integer <- function(x, n_draw) {
    data <- matrix(as.character(x),
                   nrow = length(x),
                   ncol = n_draw)
    rownames(data) <- names(x)
    new_rvec_chr(data)
}

## conversion to character always allowed
#' @export
atomic_to_rvec_chr.logical <- function(x, n_draw) {
    data <- matrix(as.character(x),
                   nrow = length(x),
                   ncol = n_draw)
    rownames(data) <- names(x)
    new_rvec_chr(data)
}

## atomic_to_rvec_dbl ---------------------------------------------------------

atomic_to_rvec_dbl <- function(x, n_draw) {
    UseMethod("atomic_to_rvec_dbl")
}

## no method for character

#' @export
atomic_to_rvec_dbl.double <- function(x, n_draw) {
    data <- matrix(x,
                   nrow = length(x),
                   ncol = n_draw)
    rownames(data) <- names(x)
    new_rvec_dbl(data)
}

## conversion to double always allowed
#' @export
atomic_to_rvec_dbl.integer <- function(x, n_draw) {
    data <- matrix(as.double(x),
                   nrow = length(x),
                   ncol = n_draw)
    rownames(data) <- names(x)
    new_rvec_dbl(data)
}

## conversion to double always allowed
#' @export
atomic_to_rvec_dbl.logical <- function(x, n_draw) {
    data <- matrix(as.double(x),
                   nrow = length(x),
                   ncol = n_draw)
    rownames(data) <- names(x)
    new_rvec_dbl(data)
}


## atomic_to_rvec_int ----------------------------------------------------------------

atomic_to_rvec_int <- function(x, n_draw) {
    UseMethod("atomic_to_rvec_int")
}

## conversion to integer only allowed if information preserved
#' @export
atomic_to_rvec_int.double <- function(x, n_draw) {
    data <- vec_cast(x, to = integer())
    data <- matrix(data,
                   nrow = length(x),
                   ncol = n_draw)
    rownames(data) <- names(x)
    new_rvec_int(data)
}

#' @export
atomic_to_rvec_int.integer <- function(x, n_draw) {
    data <- matrix(x,
                   nrow = length(x),
                   ncol = n_draw)
    rownames(data) <- names(x)
    new_rvec_int(data)
}

## conversion to integer always allowed
#' @export
atomic_to_rvec_int.logical <- function(x, n_draw) {
    data <- matrix(as.integer(x),
                   nrow = length(x),
                   ncol = n_draw)
    rownames(data) <- names(x)
    new_rvec_int(data)
}


## atomic_to_rvec_lgl ---------------------------------------------------------

atomic_to_rvec_lgl <- function(x, n_draw) {
    UseMethod("atomic_to_rvec_lgl")
}

## no method for rvec_chr

## conversion to logical only allowed if information preserved
#' @export
atomic_to_rvec_lgl.double <- function(x, n_draw) {
    data <- vec_cast(x, to = logical())
    data <- matrix(data,
                   nrow = length(x),
                   ncol = n_draw)
    rownames(data) <- names(x)
    new_rvec_lgl(data)
}

## conversion to logical only allowed if information preserved
#' @export
atomic_to_rvec_lgl.integer <- function(x, n_draw) {
    data <- vec_cast(x, to = logical())
    data <- matrix(data,
                   nrow = length(x),
                   ncol = n_draw)
    rownames(data) <- names(x)
    new_rvec_lgl(data)
}

#' @export
atomic_to_rvec_lgl.logical <- function(x, n_draw) {
    data <- matrix(x,
                   nrow = length(x),
                   ncol = n_draw)
    rownames(data) <- names(x)
    new_rvec_lgl(data)
}




