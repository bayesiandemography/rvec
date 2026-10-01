## HAS_TESTS
#' Extract Draws From an Rvec
#'
#' Select a set of draws, specified using indices.
#'
#' Index `i` must be a numeric vector, consisting of whole numbers
#' between 1 and `n_draw(x)`. Duplicates are allowed. `NA`s are not.
#' Draws are returned in the order specified by `i`.
#' 
#' @param x An [rvec][rvec()].
#' @param i An index vector.
#' @returns An rvec with the same type, length, and element names as `x`,
#' and `length(i)` draws. Selecting one draw still returns an rvec.
#'
#' @seealso
#' - [extract_draw()] Extract one draw as an ordinary vector.
#' - [thin_draws()] Randomly select draws without replacement.
#' - [n_draw()] Number of draws.
#'
#' @examples
#' x <- rvec(matrix(1:12, nrow = 4))
#' x
#' extract_draws(x, c(3, 1, 3))
#'
#' ## sample with replacement
#' set.seed(1)
#' i <- sample.int(n_draw(x), size = 10, replace = TRUE)
#' extract_draws(x, i)
#'
#' ## shared indices preserve alignment
#' y <- 2 * x
#' extract_draws(x, i)
#' extract_draws(y, i)
#' @export
extract_draws <- function(x, i) {
    if (!is_rvec(x))
        cli::cli_abort(c("{.arg x} is not an rvec.",
                        i = "{.arg x} has class {.cls {class(x)}}."))
    if (missing(i))
        cli::cli_abort("{.arg i} must be supplied.")
    if (!is.numeric(i) || !is.null(dim(i)) || length(i) == 0L ||
        anyNA(i) || any(!is.finite(i)) || any(i != trunc(i)) ||
        any(i < 1 | i > n_draw(x)))
        cli::cli_abort("{.arg i} must be a nonempty numeric vector of whole numbers between 1 and {n_draw(x)}, with no missing values.")
    m <- field(x, "data")
    rvec(m[, i, drop = FALSE])
}


## HAS_TESTS
#' Thin Draws in an Rvec
#'
#' Randomly select draws without replacement, retaining their original
#' order. The same draws are selected for every element of `x`.
#'
#' Selection uses R's random-number state; use [set.seed()] for
#' reproducibility. When `n_draw_new` equals `n_draw(x)`, `x` is returned
#' unchanged and no random numbers are used.
#'
#' Independently thinning related rvecs can lose draw alignment. To retain
#' alignment, select shared indices and use [extract_draws()] instead.
#'
#' @param x An [rvec][rvec()].
#' @param n_draw_new Number of draws to retain. A single whole number
#' between 1 and `n_draw(x)`. Must be supplied.
#'
#' @returns An rvec with the same type, length, and element names as `x`,
#' and `n_draw_new` draws.
#'
#' @seealso
#' - [extract_draws()] Select draws using explicit indices.
#' - [extract_draw()] Extract one draw as an ordinary vector.
#' - [n_draw()] Number of draws.
#'
#' @examples
#' x <- rvec(matrix(1:40, nrow = 2))
#' set.seed(1)
#' thin_draws(x, n_draw_new = 5)
#' @export
thin_draws <- function(x, n_draw_new) {
    if (!is_rvec(x))
        cli::cli_abort(c("{.arg x} is not an rvec.",
                        i = "{.arg x} has class {.cls {class(x)}}."))
    if (missing(n_draw_new))
        cli::cli_abort("{.arg n_draw_new} must be supplied.")
    n_old <- n_draw(x)
    if (!is.numeric(n_draw_new) || !is.null(dim(n_draw_new)) ||
        length(n_draw_new) != 1L || is.na(n_draw_new) ||
        !is.finite(n_draw_new) || n_draw_new != trunc(n_draw_new) ||
        n_draw_new < 1 || n_draw_new > n_old)
        cli::cli_abort("{.arg n_draw_new} must be a single whole number between 1 and {n_old}.")
    if (n_draw_new == n_old)
        return(x)
    i <- sort(sample.int(n_old, size = n_draw_new, replace = FALSE))
    extract_draws(x, i)
}
