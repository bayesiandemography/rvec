
## HAS_TESTS
#' Query Number of Draws
#'
#' Get a count of the random draws
#' held by `x`. If `x`
#' does not hold random draws, then `n_draw()`
#' throws an error.
#'
#' @param x An object that holds random draws,
#' eg an [rvec][rvec()].
#'
#' @returns A positive integer scalar. An rvec always has at least one draw,
#' including when it has zero elements.
#'
#' @seealso
#' - [extract_draws()] to select draws using indices.
#' - [thin_draws()] to randomly reduce the number of draws.
#' - [is_rvec()] to test if an object is an rvec.
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
n_draw.default <- function(x) {
    cli::cli_abort(c("{.arg x} does not appear to hold random draws.",
                     i = "{.arg x} has class {.cls {class(x)}}."))
}

#' @rdname n_draw
#' @export
n_draw.rvec <- function(x) {
    data <- field(x, "data")
    ncol(data)
}
