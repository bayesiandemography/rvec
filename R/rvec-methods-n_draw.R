
## HAS_TESTS
#' Number of draws in a random sample
#'
#' @param An object holding a random sample.
#'
#' @returns A count, or `NULL`, if the
#' vector being sampled as no elements.
#'
#' @examples
#' m <- matrix(1:40, nrow = 4, ncol = 10)
#' x <- rvec(m)
#' n_draw(x)
#' @export
n_draw <- function(x) {
    UseMethod("n_draw")
}

#' @rdname n_draw
#' @export
n_draw.rvec <- function(x) {
    data <- field(x, "data")
    ncol(data)
}
