#' Minima, Maxima, and Ranges Within Draws
#'
#' Calculate minima, maxima, or ranges across elements independently within
#' each draw. In contrast, [draws_min()] and [draws_max()] summarise across
#' draws separately for each element.
#'
#' @param ... Rvecs or ordinary vectors. Arguments are combined using rvec's
#'   concatenation rules: one-draw rvecs and ordinary vectors are repeated
#'   across draws, and other draw counts must agree. Element lengths need not
#'   agree. Ordinary vectors are treated as additional elements, not as draws.
#' @param na.rm Whether to remove missing values before calculating summaries.
#' @param finite For `range()`, whether to exclude all nonfinite numeric values.
#'   When `TRUE`, this also removes missing values.
#'
#' @returns An rvec of length one for `min()` and `max()`, or length two for
#'   `range()` (minimum followed by maximum), with the common number of draws.
#'   Element names are dropped. The result uses a common type across draws.
#'
#' @details
#' The first argument must be an rvec for base R to dispatch to these methods.
#' Subsequent arguments may be rvecs or ordinary vectors.
#'
#' The calculations use the corresponding base R function on each draw,
#' including its handling of `NA`, `NaN`, infinities, and character values.
#' Empty numeric draws (including those emptied by removing missing or
#' nonfinite values) give `Inf` for the minimum and `-Inf` for the maximum,
#' with base R warnings. Warnings are emitted separately for each draw.
#'
#' These operations do not require a common ordering of the elements across
#' draws. The restrictions on [sort()], [order()], and [xtfrm()] are unchanged.
#'
#' @seealso [base::min()], [base::max()], [base::range()],
#'   [draws_min()], [draws_max()]
#'
#' @examples
#' x <- rvec(rbind(a = c(1, 10), b = c(3, 5), c = c(2, 8)))
#' min(x)       # one element: draws 1 and 5
#' max(x)       # one element: draws 3 and 10
#' range(x)     # two elements: minima and maxima
#' draws_min(x) # three values: one for each original element
#' min(x, 0)    # include 0 in every draw
#' range(x, rvec(c(4, 6)))
#'
#' y <- rvec(rbind(c(NA, Inf), c(2, 3)))
#' min(y, na.rm = TRUE)
#' range(y, finite = TRUE)
#' @name extrema
NULL

## HAS_TESTS
#' @rdname extrema
#' @export
min.rvec <- function(..., na.rm = FALSE) {
    extrema_rvec(list(...), base::min, na.rm)
}

## HAS_TESTS
#' @rdname extrema
#' @export
max.rvec <- function(..., na.rm = FALSE) {
    extrema_rvec(list(...), base::max, na.rm)
}

## HAS_TESTS
#' @rdname extrema
#' @export
range.rvec <- function(..., na.rm = FALSE, finite = FALSE) {
    extrema_rvec(list(...), base::range, na.rm, finite)
}

## HAS_TESTS
#' Apply base extrema summaries independently to each draw
#' @noRd
extrema_rvec <- function(args, fun, na_rm, finite = NULL) {
    # A single rvec already has the required layout; avoid copying it via vec_c.
    x <- if (length(args) == 1L && is_rvec(args[[1L]])) args[[1L]]
         else do.call(vec_c, unname(args))
    m <- field(x, "data")
    results <- lapply(seq_len(ncol(m)), function(j) {
        if (!identical(fun, base::range)) fun(m[, j], na.rm = na_rm)
        else fun(m[, j], na.rm = na_rm, finite = finite)
    })
    # Some draws may produce doubles (e.g. empty integer draws give Inf).
    # Combining the results selects one common storage type without truncation.
    rvec(matrix(unlist(results, use.names = FALSE), ncol = ncol(m)))
}
