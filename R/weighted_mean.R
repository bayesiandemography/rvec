
#' Calculate Weighted Summaries
#'
#' @description
#' 
#' Calculate weighted
#' - means
#' - medians
#' - MADs (mean absolute deviations)
#' - variances
#' - standard deviations.
#'
#' These functions all work with ordinary vectors
#' and with [rvecs][rvec()].
#'
#' @details
#'
#' `x` and `wt` must have the same length.
#'
#' Internally the calculations are done by
#' [matrixStats](https://CRAN.R-project.org/package=matrixStats)
#' functions such as [matrixStats::weightedMean()]
#' and [matrixStats::colWeightedMeans()].
#'
#' @param x Quantity being summarised.
#' An ordinary vector or an [rvec][rvec()].
#' @param wt Weights. An ordinary vector,
#' an [rvec][rvec()], or `NULL` (the default.)
#' If `NULL`, an unweighted summary is returned.
#' @param na_rm Whether to remove `NA`s
#' in `x` or `wt` before calculating.
#' Default is `FALSE`.
#'  See
#' [matrixStats::weightedMean()] for a description
#' of the algorithm used.
#' 
#' @returns
#' If `x` or `wt` or is [rvec][rvec()],
#' then an rvec of length 1. Otherwise, a scalar.
#'
#' @seealso
#' - Functions [mean()], [median()],
#'   [mad()], [var()], [sd()] for unweighted data
#'   all have methods for rvecs
#' - The original
#'   [matrixStats](https://CRAN.R-project.org/package=matrixStats)
#'   weighted summary functions have additional options
#'   not implemented in the functions here.
#' - [weighted.mean()] is a base R function for weighted data
#' - For numeric summaries of draws in an rvec,
#'   use [draws_median()], [draws_mean], [draws_quantile()],
#'   [draws_fun()].
#'
#' @examples
#' ## 'x' is rvec, 'wt' is ordinary vector
#' v <- rvec(list(c(1, 11),
#'                c(2, 12),
#'                c(7, 17)))
#' weights <- c(40, 80, 72)
#' weighted_mean(v, wt = weights)
#'
#' ## 'x' is ordinary vector, 'wt' is rvec
#' y <- c(1, 2, 3)
#' w <- rvec(list(c(100, 200),
#'                c(210, 889),
#'                c(200, 200)))
#' weighted_mean(y, wt = w)
#' weighted_mean(y, wt = w, na_rm = TRUE)
#' @export
weighted_mean <- function(x,
                          wt = NULL,
                          na_rm = FALSE) {
    UseMethod("weighted_mean")
}

## HAS_TESTS
#' @rdname weighted_mean
#' @export
weighted_mean.default <- function(x,
                                  wt = NULL,
                                  na_rm = FALSE) {
    if (is_rvec(wt))
        weighted_fun_has_rvec(x = x,
                              wt = wt,
                              na_rm = na_rm,
                              fun_vec = matrixStats::weightedMean,
                              fun_mat = matrixStats::colWeightedMeans)
    else
        weighted_fun_no_rvec(x = x,
                             wt = wt,
                             na_rm = na_rm,
                             fun = matrixStats::weightedMean)
}

## HAS_TESTS
#' @rdname weighted_mean
#' @export
weighted_mean.rvec <- function(x,
                               wt = NULL,
                               na_rm = FALSE) {
    weighted_fun_has_rvec(x = x,
                          wt = wt,
                          na_rm = na_rm,
                          fun_vec = matrixStats::weightedMean,
                          fun_mat = matrixStats::colWeightedMeans)
}


## 'weighted_mad' ----------------------------------------------------------

#' @rdname weighted_mean
#' @export
weighted_mad <- function(x,
                         wt = NULL,
                         na_rm = FALSE) {
    UseMethod("weighted_mad")
}

## HAS_TESTS
#' @rdname weighted_mean
#' @export
weighted_mad.default <- function(x,
                                 wt = NULL,
                                 na_rm = FALSE) {
    if (is_rvec(wt))
        weighted_fun_has_rvec(x = x,
                              wt = wt,
                              na_rm = na_rm,
                              fun_vec = matrixStats::weightedMad,
                              fun_mat = matrixStats::colWeightedMads)
    else
        weighted_fun_no_rvec(x = x,
                             wt = wt,
                             na_rm = na_rm,
                             fun = matrixStats::weightedMad)
}

## HAS_TESTS
#' @rdname weighted_mean
#' @export
weighted_mad.rvec <- function(x,
                              wt = NULL,
                              na_rm = FALSE) {
    weighted_fun_has_rvec(x = x,
                          wt = wt,
                          na_rm = na_rm,
                          fun_vec = matrixStats::weightedMad,
                          fun_mat = matrixStats::colWeightedMads)
}


## 'weighted_median' ----------------------------------------------------------

#' @rdname weighted_mean
#' @export
weighted_median <- function(x,
                            wt = NULL,
                            na_rm = FALSE) {
    UseMethod("weighted_median")
}

## HAS_TESTS
#' @rdname weighted_mean
#' @export
weighted_median.default <- function(x,
                                    wt = NULL,
                                    na_rm = FALSE) {
    if (is_rvec(wt))
        weighted_fun_has_rvec(x = x,
                              wt = wt,
                              na_rm = na_rm,
                              fun_vec = matrixStats::weightedMedian,
                              fun_mat = matrixStats::colWeightedMedians)
    else
        weighted_fun_no_rvec(x = x,
                             wt = wt,
                             na_rm = na_rm,
                             fun = matrixStats::weightedMedian)
}

## HAS_TESTS
#' @rdname weighted_mean
#' @export
weighted_median.rvec <- function(x,
                                 wt = NULL,
                                 na_rm = FALSE) {
    weighted_fun_has_rvec(x = x,
                          wt = wt,
                          na_rm = na_rm,
                          fun_vec = matrixStats::weightedMedian,
                          fun_mat = matrixStats::colWeightedMedians)
}


## 'weighted_sd' --------------------------------------------------------------

#' @rdname weighted_mean
#' @export
weighted_sd <- function(x,
                        wt = NULL,
                        na_rm = FALSE) {
    UseMethod("weighted_sd")
}

## HAS_TESTS
#' @rdname weighted_mean
#' @export
weighted_sd.default <- function(x,
                                wt = NULL,
                                na_rm = FALSE) {
    if (is_rvec(wt))
        weighted_fun_has_rvec(x = x,
                              wt = wt,
                              na_rm = na_rm,
                              fun_vec = matrixStats::weightedSd,
                              fun_mat = matrixStats::colWeightedSds)
    else
        weighted_fun_no_rvec(x = x,
                             wt = wt,
                             na_rm = na_rm,
                             fun = matrixStats::weightedSd)
}

## HAS_TESTS
#' @rdname weighted_mean
#' @export
weighted_sd.rvec <- function(x,
                             wt = NULL,
                             na_rm = FALSE) {
    weighted_fun_has_rvec(x = x,
                          wt = wt,
                          na_rm = na_rm,
                          fun_vec = matrixStats::weightedSd,
                          fun_mat = matrixStats::colWeightedSds)
}


## 'weighted_var' -------------------------------------------------------------

#' @rdname weighted_mean
#' @export
weighted_var <- function(x,
                        wt = NULL,
                        na_rm = FALSE) {
    UseMethod("weighted_var")
}

## HAS_TESTS
#' @rdname weighted_mean
#' @export
weighted_var.default <- function(x,
                                wt = NULL,
                                na_rm = FALSE) {
    if (is_rvec(wt))
        weighted_fun_has_rvec(x = x,
                              wt = wt,
                              na_rm = na_rm,
                              fun_vec = matrixStats::weightedVar,
                              fun_mat = matrixStats::colWeightedVars)
    else
        weighted_fun_no_rvec(x = x,
                             wt = wt,
                             na_rm = na_rm,
                             fun = matrixStats::weightedVar)
}

## HAS_TESTS
#' @rdname weighted_mean
#' @export
weighted_var.rvec <- function(x,
                             wt = NULL,
                             na_rm = FALSE) {
    weighted_fun_has_rvec(x = x,
                          wt = wt,
                          na_rm = na_rm,
                          fun_vec = matrixStats::weightedVar,
                          fun_mat = matrixStats::colWeightedVars)
}


## Helper functions -----------------------------------------------------------

## HAS_TESTS
#' Wrapper for weighted* functions from matrixStats
#'
#' Used when neither 'x' nor 'wt' is rvec.
#'
#' @param x Vector
#' @param wt Vector
#' @param na_rm Flag
#' @param fun weighted* function
#'
#' @returns A scalar
#'
#' @noRd
weighted_fun_no_rvec <- function(x, wt, na_rm, fun) {
    if (is.null(wt))
        wt <- rep(1, times = length(x))
    else
        check_same_length(x = x,
                          y = wt,
                          x_arg = "x",
                          y_arg = "wt")
    if (length(x) == 0L)
        return(NaN)
    check_flag(na_rm)
    fun(x = x,
        w = wt,
        na.rm = na_rm)
}


#' Wrapper for weighted* and
#' colWeighted* functions from matrixStats
#'
#' Used when when 'x' or 'wt' is rvec.
#'
#' @param x Vector or rvec
#' @param wt Vector or rvec
#' @param na_rm Flag
#' @param fun_vec weighted* function
#' @param fun_mat colWeighted* function
#'
#' @returns A length-1 rvec
#'
#' @noRd
weighted_fun_has_rvec <- function(x,
                                  wt,
                                  na_rm,
                                  fun_vec,
                                  fun_mat) {
    n_obs <- length(x)
    if (is.null(wt))
        wt <- rep(1, times = n_obs)
    else
        check_same_length(x = x,
                          y = wt,
                          x_arg = "x",
                          y_arg = "wt")
    check_flag(na_rm)
    is_rv_x <- is_rvec(x)
    is_rv_w <- is_rvec(wt)
    if (is_rv_x && is_rv_w)
        n_draw <- n_draw_common(x = x,
                                y = wt,
                                x_arg = "x",
                                y_arg = "wt")
    else if (is_rv_x && !is_rv_w)
        n_draw <- n_draw(x)
    else
        n_draw <- n_draw(wt)
    if (n_obs == 0L)
        data <- NaN
    else {
        if (is_rv_x)
            m_x <- field(x, "data")
        else
            m_x <- matrix(x, nrow = n_obs, ncol = n_draw)
        if (is_rv_w) {
            m_w <- field(wt, "data")
            data <- double(length = n_draw)
            for (i in seq_len(n_draw))
                data[[i]] <- fun_vec(x = m_x[, i],
                                     w = m_w[, i],
                                     na.rm = na_rm)
        }
        else 
            data <- fun_mat(x = m_x,
                            w = wt,
                            na.rm = na_rm)
    }
    data <- matrix(data, nrow = 1L, ncol = n_draw)
    rvec(data)
}
