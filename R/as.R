
## as.character ---------------------------------------------------------------

## HAS_TESTS
#' @export
as.character.rvec <- function(x, ...) {
    m <- field(x, "data")
    as.character(m, ...)
}


## as.double ------------------------------------------------------------------

## HAS_TESTS
#' @export
as.double.rvec <- function(x, ...) {
    m <- field(x, "data")
    as.double(m, ...)
}


## as.integer -----------------------------------------------------------------

## HAS_TESTS
#' @export
as.integer.rvec <- function(x, ...) {
    m <- field(x, "data")
    as.integer(m, ...)
}


## as.logical -----------------------------------------------------------------

## HAS_TESTS
#' @export
as.logical.rvec <- function(x, ...) {
    m <- field(x, "data")
    as.logical(m, ...)
}


## as.numeric -----------------------------------------------------------------

## HAS_TESTS
#' @export
as.numeric.rvec <- function(x, ...) {
    m <- field(x, "data")
    as.numeric(m, ...)
}


## as.matrix ------------------------------------------------------------------

## HAS_TESTS
#' @export
as.matrix.rvec <- function(x, ...) {
    field(x, "data")
}
