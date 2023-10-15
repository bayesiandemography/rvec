
## 'draws_all', 'draws_any' ---------------------------------------------------

#' Logical Operations Across Random Draws
#'
#' Apply `all` or `any` logical summaries
#' across random draws.
#'
#' @param x An object of class [rvec][rvec()].
#' @param na_rm Whether to remove NAs before
#' calculating summaries. Default is `FALSE`.
#'
#' @returns A vector.
#'
#' @seealso
#' Apply pre-specified functions across draws:
#' - [draws_median()]
#' - [draws_mean()]
#' - [draws_mode()]
#' - [draws_ci()]
#' - [draws_quantile()]
#'
#' Apply arbitrary function across draws:
#' - [draws_fun()] to apply abritrary functions
#'
#' For additional functions for summarising random draws, see
#' [tidybayes](https://CRAN.R-project.org/package=tidybayes)
#' and [ggdist](https://CRAN.R-project.org/package=ggdist).
#' Function [as_list_col()] converts rvecs into a
#' format that `tidybayes` and `ggdist` can work with.
#'
#' @examples
#' m <- rbind(a = c(TRUE,  FALSE,  TRUE),
#'            b = c(TRUE,  TRUE,   TRUE),
#'            c = c(FALSE, FALSE,  FALSE))
#' x <- rvec(m)
#' x
#' draws_all(x)
#' draws_any(x)
#' @export
draws_all <- function(x, na_rm = FALSE) {
    UseMethod("draws_all")
}

## HAS_TESTS
#' @rdname draws_all
#' @export
draws_all.rvec_chr <- function(x, na_rm = FALSE) {
    cli::cli_abort("{.fun all} not defined for character.")
}

## HAS_TESTS
#' @rdname draws_all
#' @export
draws_all.rvec <- function(x, na_rm = FALSE) {
    check_flag(na_rm)
    m <- field(x, "data")
    if (nrow(m) == 0L)
        TRUE ## base::all returns TRUE with zero-length 'x'
    else {
        if (!is.logical(m))
            cli::cli_warn("Coercing from type {.val {typeof(m)}} to type {.val logical}.")
        ans <- matrixStats::rowAlls(m, na.rm = na_rm)
        names(ans) <- rownames(m)
        ans
    }
}

#' @rdname draws_all
#' @export
draws_any <- function(x, na_rm = FALSE) {
    UseMethod("draws_any")
}

## HAS_TESTS
#' @rdname draws_all
#' @export
draws_any.rvec_chr <- function(x, na_rm = FALSE) {
    cli::cli_abort("{.fun any} not defined for character.")
}

## HAS_TESTS
#' @rdname draws_all
#' @export
draws_any.rvec <- function(x, na_rm = FALSE) {
    check_flag(na_rm)
    m <- field(x, "data")
    if (nrow(m) == 0L)
        FALSE ## base::any returns FALSE with zero-length 'x'
    else {
        if (!is.logical(m))
            cli::cli_warn("Coercing from type {.val {typeof(m)}} to type {.val logical}.")
        ans <- matrixStats::rowAnys(m, na.rm = na_rm)
        names(ans) <- rownames(m)
        ans
    }
}


## 'draws_ci' -----------------------------------------------------------------

#' Credible Intervals from Random Draws
#'
#' Summarise the distribution of random draws
#' in an `rvec`, using a simple credible interval.
#'
#' @section Warning:
#'
#' It is tempting to assign the results
#' of a call to `draws_ci()` to a
#' column in a base R data frame or a tibble,
#' as in
#'
#' `my_df$ci <- draws_ci(my_rvec)`
#'
#' However, creating columns in
#' this way currently corrupts data
#' frames and tibbles. For safer options,
#' see the examples below.
#'
#' @inheritParams draws_all
#' @param width A number, where `0 < width <= 1`.
#' Default is `0.975`.
#' @param prefix String to be added to the
#' names of columns in the result.
#' Defaults to name of `x`.
#'
#' @returns A [tibble][tibble::tibble()]
#' with three columns.
#'
#' @seealso
#' [draws_quantile()] gives more options
#' for forming quantiles.
#'
#' Other ways of applying pre-specified functions
#' across draws are:
#' - [draws_all()]
#' - [draws_any]
#' - [draws_median()]
#' - [draws_mean()]
#' - [draws_mode()]
#' - [draws_quantile()]
#'
#' Apply arbitrary function across draws:
#' - [draws_fun()] to apply abritrary functions
#'
#' For additional functions for summarising random draws, see
#' [tidybayes](https://CRAN.R-project.org/package=tidybayes)
#' and [ggdist](https://CRAN.R-project.org/package=ggdist).
#' Function [as_list_col()] converts rvecs into a
#' format that `tidybayes` and `ggdist` can work with.
#'
#' @examples
#' set.seed(0)
#' m <- rbind(a = rnorm(100, mean = 5, sd = 2),
#'            b = rnorm(100, mean = -3, sd = 3),
#'            c = rnorm(100, mean = 0, sd = 20))
#' x <- rvec(m)
#' x
#' draws_ci(x)
#'
#' ## results from 'draws_ci'
#' ## assigned to a data frame
#' library(dplyr)
#' df <- data.frame(x)
#'
#' ## base R approach
#' cbind(df, draws_ci(x))
#'
#' ## a tidyverse alternative:
#' ## mutate with no '='
#' df |> mutate(draws_ci(x))
#' @export
draws_ci <- function(x,
                     width = 0.95,
                     prefix = NULL,
                     na_rm = FALSE) {
    UseMethod("draws_ci")
}

## HAS_TESTS
#' @rdname draws_ci
#' @export
draws_ci.rvec <- function(x,
                          width = 0.95,
                          prefix = NULL,
                          na_rm = FALSE) {
    x_str <- deparse1(substitute(x))
    check_width(width)
    has_prefix <- !is.null(prefix)
    if (has_prefix)
        check_str(prefix, x_arg = "prefix")
    check_flag(na_rm)
    probs <- make_probs(width)
    m <- field(x, "data")
    if (nrow(m) == 0L)
        ans <- stats::quantile(double(), probs = probs)
    else {
        ans <- matrixStats::rowQuantiles(m,
                                         probs = probs,
                                         na.rm = na_rm,
                                         drop = FALSE)
        ans <- matrix_to_list_of_cols(ans)
    }
    nms <- c(".lower", ".mid", ".upper")
    if (has_prefix)
        nms <- paste0(prefix, nms)
    else
        nms <- paste0(x_str, nms)
    names(ans) <- nms
    ans <- tibble::tibble(!!!ans)
    ans
}

## HAS_TESTS
#' @rdname draws_ci
#' @export
draws_ci.rvec_chr <- function(x,
                              width = 0.95,
                              prefix = NULL,
                              na_rm = FALSE) {
    cli::cli_abort("Credible intervals not defined for character.")
}


## 'draws_median', 'draws_mean', 'draws_mode' ---------------------------------

#' Medians, Means, and Modes Across Random Draws
#'
#' Summarise the distribution of random draws
#' in an `rvec`, using means, medians, or modes.
#'
#'
#' When `method` is `"mode"`, `reduce_rvec()`
#' returns the most common value for each
#' observation. When there is a tie, it returns
#' `NA`.
#'
#' @inheritParams draws_all
#'
#' @returns A vector.
#'
#' @seealso
#' Apply pre-specified functions across draws:
#' - [draws_all()]
#' - [draws_any()]
#' - [draws_ci()]
#' - [draws_quantile()]
#'
#' Apply arbitrary function across draws:
#' - [draws_fun()] to apply abritrary functions
#'
#' For additional functions for summarising random draws, see
#' [tidybayes](https://CRAN.R-project.org/package=tidybayes)
#' and [ggdist](https://CRAN.R-project.org/package=ggdist).
#' Function [as_list_col()] converts rvecs into a
#' format that `tidybayes` and `ggdist` can work with.
#'
#' @examples
#' m <- rbind(a = c(1, 1, 1, 2, 3),
#'            b = c(2, 4, 0, 2, 3),
#'            c = c(0, 0, 1, 0, 100))
#' x <- rvec(m)
#' x
#' draws_median(x)
#' draws_mean(x)
#' draws_mode(x)
#' @export
draws_median <- function(x, na_rm = FALSE) {
    UseMethod("draws_median")
}

## HAS_TESTS
#' @rdname draws_median
#' @export
draws_median.rvec_chr <- function(x, na_rm = FALSE) {
    cli::cli_abort("Median not defined for character.")
}

## HAS_TESTS
#' @rdname draws_median
#' @export
draws_median.rvec <- function(x, na_rm = FALSE) {
    check_flag(na_rm)
    m <- field(x, "data")
    if (nrow(m) == 0L)
        NA_real_ ## base::median returns NA with zero-length 'x'
    else {
        m <- 1 * m
        ans <- matrixStats::rowMedians(m, na.rm = na_rm)
        names(ans) <- rownames(m)
        ans
    }
}

#' @rdname draws_median
#' @export
draws_mean <- function(x, na_rm = FALSE) {
    UseMethod("draws_mean")
}

## HAS_TESTS
#' @rdname draws_median
#' @export
draws_mean.rvec <- function(x, na_rm = FALSE) {
    check_flag(na_rm)
    m <- field(x, "data")
    if (nrow(m) == 0L)
        NaN ## base::mean returns NaN with zero-length 'x'
    else {
        m <- 1 * m
        ans <- matrixStats::rowMeans2(m, na.rm = na_rm)
        names(ans) <- rownames(m)
        ans
    }
}

## HAS_TESTS
#' @rdname draws_median
#' @export
draws_mean.rvec_chr <- function(x, na_rm = FALSE) {
    cli::cli_abort("Mean not defined for character.")
}

#' @rdname draws_median
#' @export
draws_mode <- function(x, na_rm = FALSE) {
    UseMethod("draws_mode")
}

## HAS_TESTS
#' @rdname draws_median
#' @export
draws_mode.rvec <- function(x, na_rm = FALSE) {
    check_flag(na_rm)
    m <- field(x, "data")
    storage_mode <- storage.mode(m)
    if (nrow(m) == 0L) {
        ans <- NA
    }
    else {
        useNA <- if (na_rm) "no" else "ifany"
        tabs <- apply(m, 1L, table, useNA = useNA, simplify = FALSE)
        nms_tabs <- lapply(tabs, names)
        i_max <- lapply(tabs, function(x) which(x == max(x))) # allows multiple
        has_unique_mode <- vapply(i_max, length, 1L) == 1L
        ans <- rep(NA, times = nrow(m))
        modes <- .mapply(function(x, i) x[[i]],
                         dots = list(nms_tabs[has_unique_mode],
                                     i_max[has_unique_mode]),
                         MoreArgs = list())
        modes <- unlist(modes, use.names = FALSE)
        ans[has_unique_mode] <- modes
        names(ans) <- rownames(m)
    }
    storage.mode(ans) <- storage_mode
    ans
}


## 'draws_quantile ------------------------------------------------------------

#' Quantiles Across Random Draws
#'
#' Summarise the distribution of random draws
#' in an `rvec`, using quantiles.
#'
#' The `probs` argument defaults to
#' `c(0.025, 0.25, 0.5, 0.75, 0.975)`,
#' the values needed for a median,
#' a 50% credible intervals, and a
#' 95% credible interval.
#'
#' @section Warning:
#'
#' It is tempting to assign the results
#' of a call to `draws_quantile()` to a
#' column in a base R data frame or a tibble,
#' as in
#'
#' `my_df$quantile <- draws_quantile(my_rvec)`
#'
#' However, creating data frame columns in
#' this way currently seems to corrupt both data
#' frames and tibbles. For safer options,
#' see the examples below.
#'
#' @inheritParams draws_all
#' @param probs Vector of probabilities.
#'
#' @returns A [tibble][tibble::tibble()].
#'
#' @seealso
#' [draws_ci()] creates simple credible intervals.
#'
#' Other functions for applying pre-specified
#' functions across draws are:
#' - [draws_all()]
#' - [draws_any()]
#' - [draws_ci()]
#' - [draws_median()]
#' - [draws_mean()]
#' - [draws_mode()]
#'
#' Apply arbitrary function across draws:
#' - [draws_fun()] to apply arbitrary functions
#'
#' For additional functions for summarising random draws, see
#' [tidybayes](https://CRAN.R-project.org/package=tidybayes)
#' and [ggdist](https://CRAN.R-project.org/package=ggdist).
#' Function [as_list_col()] converts rvecs into a
#' format that `tidybayes` and `ggdist` can work with.
#'
#' @examples
#' set.seed(0)
#' m <- rbind(a = rnorm(100, mean = 5, sd = 2),
#'            b = rnorm(100, mean = -3, sd = 3),
#'            c = rnorm(100, mean = 0, sd = 20))
#' x <- rvec(m)
#' x
#' draws_quantile(x)
#'
#' ## results from 'draws_quantile'
#' ## assigned to a data frame
#' library(dplyr)
#' df <- data.frame(x)
#'
#' ## base R approach
#' cbind(df, draws_quantile(x))
#'
#' ## a tidyverse alternative:
#' ## mutate with no '='
#' df |>
#'   mutate(draws_quantile(x))
#' @export
draws_quantile <- function(x,
                           probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                           na_rm = FALSE) {
    UseMethod("draws_quantile")
}

## HAS_TESTS
#' @rdname draws_quantile
#' @export
draws_quantile.rvec <- function(x,
                                probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                                na_rm = FALSE) {
    x_str <- deparse1(substitute(x))
    check_probs(probs)
    check_flag(na_rm)
    m <- field(x, "data")
    if (nrow(m) == 0L)
        ans <- stats::quantile(double(), probs = probs)
    else {
        ans <- matrixStats::rowQuantiles(m,
                                         probs = probs,
                                         na.rm = na_rm,
                                         drop = FALSE)
        ans <- matrix_to_list_of_cols(ans)
    }
    nms <- names(ans)
    nms <- sub("%$", "", nms)
    nms <- paste(x_str, nms, sep = "_")
    names(ans) <- nms
    ans <- tibble::tibble(!!!ans)
    ans
}

## HAS_TESTS
#' @rdname draws_quantile
#' @export
draws_quantile.rvec_chr <- function(x,
                                    probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                                    na_rm = FALSE) {
    cli::cli_abort("Quantiles not defined for character.")
}


## 'draws_fun' ----------------------------------------------------------------

#' Apply Summary Function Across Random Draws
#'
#' Summarise the distribution of random draws
#' in an `rvec`, using a function.
#'
#' @inheritParams draws_all
#' @param fun A function.
#' @param ... Additional arguments passed to `fun`.
#'
#' @returns The results from calls to `fun`,
#' combined using [vctrs::vec_c()].
#'
#' @seealso
#' Apply pre-specified functions across draws:
#' - [draws_all()]
#' - [draws_any()]
#' - [draws_ci()]
#' - [draws_median()]
#' - [draws_mean()]
#' - [draws_mode()]
#' - [draws_quantile()]
#'
#' @examples
#' set.seed(0)
#' m <- rbind(a = rnorm(100, mean = 5, sd = 2),
#'            b = rnorm(100, mean = -3, sd = 3),
#'            c = rnorm(100, mean = 0, sd = 20))
#' x <- rvec(m)
#' x
#' draws_fun(x, fun = mad)
#' draws_fun(x, fun = range)
#' draws_fun(x, weighted.mean, wt = runif(100))
#' draws_fun(x, function(x) sd(x) / mean(x))
#' @export
draws_fun <- function(x, fun, ...) {
    UseMethod("draws_fun")
}

## HAS_TESTS
#' @rdname draws_fun
#' @export
draws_fun.rvec <- function(x, fun, ...) {
    m <- field(x, "data")
    if (nrow(m) == 0L)
        return(list())
    fun <- match.fun(fun)
    l <- matrix_to_list_of_rows(m)
    for (i in seq_along(l))
        l[[i]] <- fun(l[[i]], ...)
    is_atomic <- vapply(l, is.atomic, TRUE)
    lengths <- lengths(l)
    if (all(is_atomic) && all(lengths == 1L))
        vec_c(!!!l)
    else
        l
}









