
## HAS_TESTS
#' Query Number of Draws in a Random Sample
#'
#' @param x An object holding a random sample.
#'
#' @returns A count, or, if the
#' vector being sampled as no elements, `NULL`.
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
