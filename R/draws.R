
#' Medians, means, and modes for random draws
#'
#' Summarise the distribution of random draws
#' in an `rvec`, using means, medians, or modes.
#'
#' An `rvec`
#'
#' |              | Draw 1        | Draw 2        | \eqn{\dots}  | Draw \eqn{n}  |
#' |:-------------|:-------------:|:-------------:|:------------:|:-------------:|
#' | Obs 1        | \eqn{x_{1,1}} | \eqn{x_{1,2}} | \eqn{\dots}  | \eqn{x_{1,n}} |
#' | Obs 2        | \eqn{x_{2,1}} | \eqn{x_{2,2}} | \eqn{\dots}  | \eqn{x_{2,n}} |
#' | \eqn{\vdots} | \eqn{\vdots}  | \eqn{\vdots}  | \eqn{\ddots} | \eqn{\vdots}  |
#' | Obs \eqn{m}  | \eqn{x_{m,1}} | \eqn{x_{m,2}} | \eqn{\dots}  | \eqn{x_{m,n}} |
#'
#'
#' When `method` is `"mode"`, `reduce_rvec()`
#' returns the most common value for each
#' observation. When there is a tie, it returns
#' `NA`.
#'
#' @param x An object of class [rvec][rvec()].
#' @param na_rm Whether to remove NAs before
#' calculating summaries. Default is `FALSE`.
#'
#' @returns A vector.
#'
#' @seealso
#' - [draws_quantile()] to calculate quantiles
#' - [draws_fun()] to apply abritrary functions
#'
#' For a wide range of tools for summarising random draws, see
#' [tidybayes](https://CRAN.R-project.org/package=tidybayes)
#' and [ggdist](https://CRAN.R-project.org/package=ggdist).
#' Function [as_list_col()] converts `rvec`s into a
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
    check_na_rm(na_rm)
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
    check_na_rm(na_rm)
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
    check_na_rm(na_rm)
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




#' Quantiles for random draws
#'
#' Summarise the distribution of random draws
#' in an `rvec`, using quantiles.
#'
#' The `probs` argument defaults to
#' `c(0.025, 0.5, 0.975)`, the values needed
#' for a median and 95% credible interval.
#'
#' Regular [data frames][base::data.frame()]
#' and [tibbles][tibble::tibble()] both allow
#' data frame columns, so the results from
#' `draws_quantile()` can be inserted into a
#' data frame. See below for an example.
#'
#' To expand a data frame column, use
#' [tidyr::unnest_wider()](https://tidyr.tidyverse.org/reference/hoist.html)
#'
#' @inheritParams draws_median
#' @param probs Vector of probabilities.
#'
#' @returns A [tibble][tibble::tibble()].
#'
#' @seealso
#' - [draws_median()], [draws_mean], [draws_mode()]
#' describe the location of a distribution.
#' - [draws_fun()] applies a function to draws.
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
#' ## results from 'draw_quantile'
#' ## assigned to a data frame
#' library(dplyr)
#' df <- tibble(x)
#' df %>%
#'   mutate(draws_quantile(x))
#' @export
draws_quantile <- function(x,
                           probs = c(0.025, 0.5, 0.975),
                           na_rm = FALSE) {
    UseMethod("draws_quantile")
}

## HAS_TESTS
#' @rdname draws_quantile
#' @export
draws_quantile.rvec <- function(x,
                                probs = c(0.025, 0.5, 0.975),
                                na_rm = FALSE) {
    x_str <- deparse1(substitute(x))
    ## x_expr <- rlang::expr(x)
    ## x_str <- rlang::expr_deparse(x_expr)
    check_probs(probs)
    check_na_rm(na_rm)
    m <- field(x, "data")
    if (nrow(m) == 0L)
        ans <- stats::quantile(double(), probs = probs)
    else {
        ans <- matrixStats::rowQuantiles(m, probs = probs, na.rm = na_rm)
        ans <- matrix_to_list_of_cols(ans)
    }
    names(ans) <- paste(x_str, names(ans), sep = "_")
    ans <- tibble::tibble(!!!ans)
    ans
}

## HAS_TESTS
#' @rdname draws_quantile
#' @export
draws_quantile.rvec_chr <- function(x,
                                     probs = c(0.025, 0.5, 0.975),
                                     na_rm = FALSE) {
    cli::cli_abort("Quantiles not defined for character.")
}


#' Apply summary function to random draws
#'
#' Summarise the distribution of random draws
#' in an `rvec`, using a function.
#'
#' @inheritParams draws_median
#' @param fun A function.
#' @param ... Additional arguments passed to `fun`.
#'
#' @returns The results from calls to `fun`,
#' combined using [vctrs::vec_c()].
#'
#' @seealso
#' Summaries of draws based on pre-specified functions:
#' - [draws_median()]
#' - [draws_mean]
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

    
    






