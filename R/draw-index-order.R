#' Find the Minimum or Maximum Position Within Each Draw
#'
#' For an rvec, find the position of the first minimum or maximum among its
#' elements independently within each draw. Ordinary inputs retain the
#' behavior of [base::which.min()] and [base::which.max()].
#'
#' @param x An ordinary vector or an [rvec::rvec()].
#'
#' @returns An rvec of indices with the same number of draws as `x` if `x`
#' is an rvec; otherwise the corresponding base R result. A nonempty rvec
#' returns one position per draw. An empty rvec returns a length-zero rvec.
#' Positions refer to the original elements, starting at one. The rvec result
#' has no name. Indices are normally integers; very long inputs may require
#' double indices, as in base R.
#'
#' @details
#' Missing values are ignored. For a nonempty rvec, a draw with no valid value
#' returns `NA_integer_`, with one summary warning giving the number of
#' affected draws. Base R instead returns `integer(0)` for an entirely missing
#' ordinary vector. An empty rvec returns a length-zero result without a
#' warning, preserving its number of draws. Ties select the first position.
#' Character values follow base R's numeric coercion, including its warnings
#' when coercion introduces missing values.
#'
#' The positions can differ between draws, so they cannot generally be used
#' as a single ordinary subscript. These wrappers mask the base functions
#' when rvec is attached. Explicit `base::which.min()` and
#' `base::which.max()` calls bypass them.
#'
#' @examples
#' x <- rvec(rbind(north = c(3, 1), south = c(1, 4), west = c(2, 2)))
#' as.matrix(which.min(x))
#' as.matrix(which.max(x))
#' @export
which.min <- function(x) {
    UseMethod("which.min")
}

#' @rdname which.min
#' @export
which.min.default <- function(x) {
    base::which.min(x)
}

#' @rdname which.min
#' @export
which.min.rvec <- function(x) {
    draw_extreme_index(x, base::which.min)
}

#' @rdname which.min
#' @export
which.max <- function(x) {
    UseMethod("which.max")
}

#' @rdname which.min
#' @export
which.max.default <- function(x) {
    base::which.max(x)
}

#' @rdname which.min
#' @export
which.max.rvec <- function(x) {
    draw_extreme_index(x, base::which.max)
}

## HAS_TESTS
#' @noRd
draw_extreme_index <- function(x, fun) {
    m <- vctrs::field(x, "data")
    if (nrow(m) == 0L)
        return(rvec(matrix(integer(), nrow = 0L, ncol = ncol(m))))
    indices <- lapply(seq_len(ncol(m)), function(j) fun(m[, j]))
    missing <- lengths(indices) == 0L
    n_missing <- sum(missing)
    if (n_missing > 0L) {
        indices[missing] <- rep(list(NA_integer_), n_missing)
        warning(sprintf("No valid value in %d of %d draws; returning NA indices.",
                        n_missing, ncol(m)), call. = FALSE)
    }
    rvec(matrix(unlist(indices, use.names = FALSE), nrow = 1L))
}
