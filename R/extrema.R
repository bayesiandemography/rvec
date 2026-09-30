## HAS_TESTS
#' @noRd
#' @export
min.rvec <- function(..., na.rm = FALSE) {
    extrema_rvec(list(...), base::min, na.rm)
}

## HAS_TESTS
#' @noRd
#' @export
max.rvec <- function(..., na.rm = FALSE) {
    extrema_rvec(list(...), base::max, na.rm)
}

## HAS_TESTS
#' @noRd
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
