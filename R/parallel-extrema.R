#' Parallel Minima and Maxima with Rvecs
#'
#' Compare corresponding elements independently within each draw. Unlike
#' [min()] and [max()], these functions do not summarise across elements.
#'
#' @param ... Rvecs or ordinary vectors. With an rvec argument, ordinary
#'   inputs must be unclassed logical, integer, double, or character vectors
#'   (or `NULL`). An rvec may occur in any argument position.
#' @param na.rm Whether to ignore missing values. If all corresponding
#'   values are missing, the result is still missing.
#'
#' @returns An rvec if any argument is an rvec, otherwise the result of
#'   [base::pmin()] or [base::pmax()]. Names come from the first argument.
#'
#' @details
#' With rvec arguments, element lengths must agree or be one. Draw counts
#' must also agree or be one. Ordinary vectors are repeated across draws.
#' This uses rvec's usual size rules rather than base R's fractional recycling.
#' Missing values, infinities, and type promotion follow the base calculation
#' on each draw. Matrices and other classed objects are not supported alongside
#' rvecs. Calls without rvecs are passed unchanged to the base functions.
#'
#' These wrappers mask the base functions when rvec is attached. Explicit
#' calls to `base::pmin()` or `base::pmax()` do not use the wrappers.
#'
#' @examples
#' x <- rvec(rbind(a = c(-2, 3), b = c(4, -1)))
#' pmax(x, 0)
#' pmax(0, x)
#' pmin(pmax(x, 0), 1)
#' @export
pmin <- function(..., na.rm = FALSE) {
    parallel_extrema(list(...), base::pmin, na.rm)
}

#' @rdname pmin
#' @export
pmax <- function(..., na.rm = FALSE) {
    parallel_extrema(list(...), base::pmax, na.rm)
}

## HAS_TESTS
#' Calculate parallel extrema without expanding shared draws
#' @noRd
parallel_extrema <- function(args, fun, na_rm) {
    is_rv <- vapply(args, is_rvec, logical(1))
    if (!any(is_rv))
        return(do.call(fun, c(args, list(na.rm = na_rm))))
    ordinary <- args[!is_rv]
    valid <- vapply(ordinary, function(x)
        is.null(x) || (!is.object(x) && is.null(dim(x)) &&
                      typeof(x) %in% c("logical", "integer", "double", "character")),
        logical(1))
    if (!all(valid))
        cli::cli_abort("Ordinary inputs alongside rvecs must be unclassed logical, integer, double, or character vectors.")
    rvs <- args[is_rv]
    # Comparing every rvec with the largest draw count catches incompatible
    # counts even when the first rvec has only one draw.
    nds <- vapply(rvs, n_draw, integer(1))
    reference <- rvs[[which.max(nds)]]
    for (x in rvs)
        n_draw_common(reference, x, x_arg = "reference", y_arg = "input")
    nd <- max(nds)
    data <- lapply(seq_along(args), function(i)
        if (is_rv[i]) field(args[[i]], "data")
        else if (is.null(args[[i]])) logical() else args[[i]])
    size <- do.call(vec_size_common, unname(data))
    calculate <- function(j) {
        columns <- lapply(seq_along(data), function(i) {
            if (is_rv[i]) data[[i]][, if (ncol(data[[i]]) == 1L) 1L else j]
            else data[[i]]
        })
        do.call(fun, c(columns, list(na.rm = na_rm)))
    }
    first <- calculate(1L)
    result <- matrix(vector(typeof(first), size * nd), nrow = size, ncol = nd)
    result[, 1L] <- first
    if (nd > 1L)
        for (j in 2:nd) result[, j] <- calculate(j)
    rownames(result) <- names(first)
    rvec(result)
}
