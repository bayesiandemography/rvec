
## 'draws_all', 'draws_any' ---------------------------------------------------

#' Logical Operations Across Random Draws
#'
#' Apply `all` or `any` logical summaries
#' across random draws.
#'
#' @param x An [rvec][rvec()].
#' @param na_rm Whether to remove NAs before
#' calculating summaries. Default is `FALSE`.
#'
#' @returns A vector.
#'
#' @seealso
#' Apply pre-specified functions across draws:
#' - [draws_min()]
#' - [draws_max()]
#' - [draws_median()]
#' - [draws_mean()]
#' - [draws_mode()]
#' - [draws_sd()]
#' - [draws_var()]
#' - [draws_cv()]
#' - [draws_ci()]
#' - [draws_quantile()]
#'
#' Apply arbitrary function across draws:
#' - [draws_fun()]
#'
#' Check for missing or finite values:
#' - [draws_any_na()]
#' - [draws_all_na()]
#' - [draws_any_infinite()]
#' - [draws_all_infinite()]
#' - [draws_any_finite()]
#' - [draws_all_finite()]
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


## Missingness and finiteness across draws ----------------------------------

#' Missing and Finite Values Across Draws
#'
#' Test whether draws are missing, infinite, or finite.
#' Tests can apply to all draws or any draws.
#'
#' Missing values include `NA` and `NaN`; infinite values are `Inf` and `-Inf`;
#' finite values exclude all four. Missingness checks accept all rvec types,
#' but finiteness checks reject character rvecs. Results never contain `NA`.
#'
#' @param x An [rvec][rvec()].
#'
#' @returns A logical vector of length `length(x)`.
#'
#'
#' @seealso
#' Apply pre-specified functions across draws:
#' - [draws_all()]
#' - [draws_any()]
#' - [draws_median()]
#' - [draws_mean()]
#' - [draws_mode()]
#' - [draws_sd()]
#' - [draws_var()]
#' - [draws_cv()]
#' - [draws_ci()]
#' - [draws_quantile()]
#'
#' Apply arbitrary function across draws:
#' - [draws_fun()]
#'
#' Test for missing or finite values separately
#' across each element and draw:
#' - [is.na()]
#' - [is.finite()]
#' - [is.infinite()]
#'
#' @examples
#' x <- rvec(rbind(complete1 = c(1, 2, 3),
#'                 missing = c(1, NA, NaN),
#'                 unbounded = c(Inf, -Inf, 2),
#'                 complete2 = c(11, 12, 13)))
#' x
#'
#' draws_any_na(x)
#' draws_all_na(x)
#' draws_any_infinite(x)
#' draws_all_infinite(x)
#' draws_any_finite(x)
#' draws_all_finite(x)
#'
#' # Keep elements whose draws are all finite
#' x[draws_all_finite(x)]
#'
#' # Filter rows of a data frame using the same condition
#' df <- tibble::tibble(id = seq_along(x), value = x)
#' df[draws_all_finite(df$value), ]
#'
#' # draws_any_na() vs anyNA()
#' draws_any_na(x) # aggregates within elements, across draws
#' anyNA(x)        # aggregates across elements, within draws
#' @name draws_any_na
NULL

#' @rdname draws_any_na
#' @export
draws_any_na <- function(x) {
    UseMethod("draws_any_na")
}

## HAS_TESTS
#' @rdname draws_any_na
#' @export
draws_any_na.rvec <- function(x) {
    m <- field(x, "data")
    ans <- if (nrow(m) == 0L) logical() else matrixStats::rowAnyNAs(m)
    names(ans) <- rownames(m)
    ans
}

#' @rdname draws_any_na
#' @export
draws_all_na <- function(x) {
    UseMethod("draws_all_na")
}

## HAS_TESTS
#' @rdname draws_any_na
#' @export
draws_all_na.rvec <- function(x) {
    m <- field(x, "data")
    ans <- if (nrow(m) == 0L) logical() else matrixStats::rowAlls(m, value = NA)
    names(ans) <- rownames(m)
    ans
}

#' @rdname draws_any_na
#' @export
draws_any_infinite <- function(x) {
    UseMethod("draws_any_infinite")
}

## HAS_TESTS
#' @rdname draws_any_na
#' @export
draws_any_infinite.rvec <- function(x) {
    draws_predicate(x, is.infinite, all = FALSE)
}

## HAS_TESTS
#' @rdname draws_any_na
#' @export
draws_any_infinite.rvec_chr <- function(x) {
    cli::cli_abort("Finiteness checks are not defined for character rvecs.")
}

#' @rdname draws_any_na
#' @export
draws_all_infinite <- function(x) {
    UseMethod("draws_all_infinite")
}

## HAS_TESTS
#' @rdname draws_any_na
#' @export
draws_all_infinite.rvec <- function(x) {
    draws_predicate(x, is.infinite, all = TRUE)
}

## HAS_TESTS
#' @rdname draws_any_na
#' @export
draws_all_infinite.rvec_chr <- function(x) {
    cli::cli_abort("Finiteness checks are not defined for character rvecs.")
}

#' @rdname draws_any_na
#' @export
draws_any_finite <- function(x) {
    UseMethod("draws_any_finite")
}

## HAS_TESTS
#' @rdname draws_any_na
#' @export
draws_any_finite.rvec <- function(x) {
    draws_predicate(x, is.finite, all = FALSE)
}

## HAS_TESTS
#' @rdname draws_any_na
#' @export
draws_any_finite.rvec_chr <- function(x) {
    cli::cli_abort("Finiteness checks are not defined for character rvecs.")
}

#' @rdname draws_any_na
#' @export
draws_all_finite <- function(x) {
    UseMethod("draws_all_finite")
}

## HAS_TESTS
#' @rdname draws_any_na
#' @export
draws_all_finite.rvec <- function(x) {
    draws_predicate(x, is.finite, all = TRUE)
}

## HAS_TESTS
#' @rdname draws_any_na
#' @export
draws_all_finite.rvec_chr <- function(x) {
    cli::cli_abort("Finiteness checks are not defined for character rvecs.")
}

## HAS_TESTS
#' Summarise a predicate across draws without constructing an intermediate rvec
#' @noRd
draws_predicate <- function(x, predicate, all) {
    m <- field(x, "data")
    ans <- if (nrow(m) == 0L) logical()
           else if (all) matrixStats::rowAlls(predicate(m))
           else matrixStats::rowAnys(predicate(m))
    names(ans) <- rownames(m)
    ans
}


## 'draws_ci' -----------------------------------------------------------------

#' Credible Intervals and Point Estimates from Random Draws
#'
#' Summarise the distribution of random draws
#' in an rvec, using credible intervals and point estimates.
#'
#' The credible intervals extend from the quantiles at
#' `0.5 - 0.5 * width` to the quantiles `0.5 + 0.5 * width`.
#'
#' When `point` is `"mean"`,  point estimates
#' can lie outside the associated credible intervals,
#' though this only happens in unusual cases, such as when
#' distributions are highly skewed.
#'
#' @section Warning:
#'
#' It is tempting to assign the results
#' of a call to `draws_ci()` to a
#' column in a data frame,
#' as in
#'
#' `my_df$ci <- draws_ci(my_rvec)`
#'
#' However, creating columns in
#' this way can corrupt an ordinary data frame.
#' For safer options,
#' see the examples below.
#'
#' @inheritParams draws_all
#' @param width Width(s) of credible interval(s).
#' One or more numbers greater than 0
#' and less than or equal to 1.
#' Default is `0.95`.
#' @param prefix String to be added to the
#' names of columns in the result.
#' Defaults to name of `x`.
#' @param point Summary measure used for point estimate:
#'  `"median"` (the default) or `"mean"`.
#'
#' @returns A [tibble][tibble::tibble()]
#' with `2 * length(width) + 1` columns.
#' When `width` has length 1, the columns are
#'
#' - `<name>.lower` Lower limit of credible interval
#' - `<name>.mid` Point estimate
#' - `<name>.upper` Upper limit of the credible interval
#' 
#' @seealso
#' [draws_quantile()] gives more options
#' for forming quantiles.
#'
#' Other ways of applying pre-specified functions
#' across draws are:
#' - [draws_all()]
#' - [draws_any]
#' - [draws_min()]
#' - [draws_max()]
#' - [draws_median()]
#' - [draws_mean()]
#' - [draws_mode()]
#' - [draws_sd()]
#' - [draws_var()]
#' - [draws_cv()]
#' - [draws_quantile()]
#'
#' Apply arbitrary function across draws:
#' - [draws_fun()]
#'
#' Check for missing or finite values:
#' - [draws_any_na()]
#' - [draws_all_na()]
#' - [draws_any_infinite()]
#' - [draws_all_infinite()]
#' - [draws_any_finite()]
#' - [draws_all_finite()]
#'
#' @examples
#' set.seed(0)
#' m <- rbind(a = rnorm(100, mean = 5, sd = 2),
#'            b = rnorm(100, mean = -3, sd = 3),
#'            c = rnorm(100, mean = 0, sd = 20))
#' x <- rvec(m)
#' x
#' draws_ci(x)
#' draws_ci(x, width = c(0.5, 0.99))
#' draws_ci(x, point = "mean")
#' draws_ci(x, prefix = "results")
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
                     na_rm = FALSE,
                     point = c("median", "mean")) {
  UseMethod("draws_ci")
}

## HAS_TESTS
#' @rdname draws_ci
#' @export
draws_ci.rvec <- function(x,
                          width = 0.95,
                          prefix = NULL,
                          na_rm = FALSE,
                          point = c("median", "mean")) {
  x_str <- deparse1(substitute(x))
  point <- match.arg(point)
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
  n <- length(width)
  if (point == "mean")
    ans[[n + 1L]] <- draws_mean(x, na_rm = na_rm)
  lower <- rep(".lower", times = n)
  upper <- rep(".upper", times = n)
  if (n > 1L) {
    s <- seq_len(n - 1L)
    lower[-1L] <- paste0(lower[-1L], s)
    upper[-n] <- paste0(upper[-n], rev(s))
  }
  nms <- c(lower, ".mid", upper)
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
                              na_rm = FALSE,
                              point = c("median", "mean")) {
  cli::cli_abort("Credible intervals not defined for character.")
}


## 'draws_median', 'draws_mean', 'draws_mode' ---------------------------------

#' Medians, Means, and Modes Across Random Draws
#'
#' Use means, medians, or modes to
#' summarise the distribution of random draws
#' in an rvec.
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
#' - [draws_min()]
#' - [draws_max()]
#' - [draws_sd()]
#' - [draws_var()]
#' - [draws_cv()]
#' - [draws_ci()]
#' - [draws_quantile()]
#'
#' Apply arbitrary function across draws:
#' - [draws_fun()]
#'
#' Check for missing or finite values:
#' - [draws_any_na()]
#' - [draws_all_na()]
#' - [draws_any_infinite()]
#' - [draws_all_infinite()]
#' - [draws_any_finite()]
#' - [draws_all_finite()]
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
    if (!is.double(m))
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
    if (!is.double(m))
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
    ans <- rep(NA, times = nrow(m))
    for (i in seq_len(nrow(m))) {
      tab <- table(m[i, ], useNA = useNA)
      i_max <- which(tab == max(tab)) # allows multiple
      if (length(i_max) == 1L)
        ans[i] <- names(tab)[i_max]
    }
    names(ans) <- rownames(m)
  }
  storage.mode(ans) <- storage_mode
  ans
}


## 'draws_min', 'draws_max' ---------------------------------------------------

#' Minima and Maxima Across Random Draws
#'
#' Apply `min` or `max` across random draws. To summarise elements within
#' each draw instead, use [base::min()] or [base::max()].
#'
#' @param x An [rvec][rvec()].
#' @param na_rm Whether to remove NAs before
#' calculating minima and maxima. Default is `FALSE`.
#'
#' @returns A vector.
#'
#' @seealso
#' Apply pre-specified functions across draws:
#' - [draws_all()]
#' - [draws_any()]
#' - [draws_median()] 
#' - [draws_mean()]
#' - [draws_mode()]
#' - [draws_sd()]
#' - [draws_var()]
#' - [draws_cv()]
#' - [draws_ci()]
#' - [draws_quantile()]
#'
#' Apply arbitrary function across draws:
#' - [draws_fun()]
#'
#' Check for missing or finite values:
#' - [draws_any_na()]
#' - [draws_all_na()]
#' - [draws_any_infinite()]
#' - [draws_all_infinite()]
#' - [draws_any_finite()]
#' - [draws_all_finite()]
#'
#' @examples
#' m <- rbind(a = c(1,  -3,  2),
#'            b = c(Inf,  0,   -Inf),
#'            c = c(0.2, 0.3,  0.1))
#' x <- rvec(m)
#' x
#' draws_min(x)
#' draws_max(x)
#' @export
draws_min <- function(x, na_rm = FALSE) {
  UseMethod("draws_min")
}

#' @rdname draws_min
#' @export
draws_max <- function(x, na_rm = FALSE) {
  UseMethod("draws_max")
}

## HAS_TESTS
#' @rdname draws_min
#' @export
draws_min.rvec_chr <- function(x, na_rm = FALSE) {
  cli::cli_abort("Minimum not defined for character.")
}

## HAS_TESTS
#' @rdname draws_min
#' @export
draws_min.rvec <- function(x, na_rm = FALSE) {
  check_flag(na_rm)
  m <- field(x, "data")
  if (nrow(m) == 0L) {
    cli::cli_warn("{.var n_draw} is 0: returning {.val Inf}.")
    Inf ## base::min returns Inf, with warning, with zero-length 'x'
  }
  else {
    if (is.logical(m))
      m <- 1L * m
    ans <- matrixStats::rowMins(m, na.rm = na_rm)
    names(ans) <- rownames(m)
    ans
  }
}

## HAS_TESTS
#' @rdname draws_min
#' @export
draws_max.rvec_chr <- function(x, na_rm = FALSE) {
  cli::cli_abort("Maximum not defined for character.")
}

## HAS_TESTS
#' @rdname draws_min
#' @export
draws_max.rvec <- function(x, na_rm = FALSE) {
  check_flag(na_rm)
  m <- field(x, "data")
  if (nrow(m) == 0L) {
    cli::cli_warn("{.var n_draw} is 0: returning {.val -Inf}.")
    -Inf ## base::max returns -Inf, with warning, with zero-length 'x'
  }
  else {
    if (is.logical(m))
      m <- 1L * m
    ans <- matrixStats::rowMaxs(m, na.rm = na_rm)
    names(ans) <- rownames(m)
    ans
  }
}


## 'draws_quantile ------------------------------------------------------------

#' Quantiles Across Random Draws
#'
#' Summarise the distribution of random draws
#' in an rvec, using quantiles. To calculate quantiles across elements
#' within each draw instead, use [stats::quantile()].
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
#' column in a data frame,
#' as in
#'
#' `my_df$quantile <- draws_quantile(my_rvec)`
#'
#' However, creating data frame columns in
#' this way can corrupt data
#' frames. For safer options,
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
#' - [draws_min()]
#' - [draws_max()]
#' - [draws_median()]
#' - [draws_mean()]
#' - [draws_mode()]
#' - [draws_sd()]
#' - [draws_var()]
#' - [draws_cv()]
#' - [draws_ci()]
#'
#' Apply arbitrary function across draws:
#' - [draws_fun()]
#'
#' Check for missing or finite values:
#' - [draws_any_na()]
#' - [draws_all_na()]
#' - [draws_any_infinite()]
#' - [draws_all_infinite()]
#' - [draws_any_finite()]
#' - [draws_all_finite()]
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

## 'draws_sd' --------------------------------------------------------

#' Standard Deviations, Variances, and Coefficients
#' of Variation Across Random Draws
#'
#' Use standard deviations, variances, or
#' coefficients of variation to
#' summarise the distribution of random draws
#' in an rvec.
#'
#' The coefficient of variation is the standard
#' deviation divided by the mean.
#'
#' @inheritParams draws_all
#'
#' @returns A vector.
#'
#' @seealso
#' Apply pre-specified functions across draws:
#' - [draws_all()]
#' - [draws_any()]
#' - [draws_mean()]
#' - [draws_median()]
#' - [draws_mode()]
#' - [draws_min()]
#' - [draws_max()]
#' - [draws_ci()]
#' - [draws_quantile()]
#'
#' Apply arbitrary function across draws:
#' - [draws_fun()]
#'
#' Check for missing or finite values:
#' - [draws_any_na()]
#' - [draws_all_na()]
#' - [draws_any_infinite()]
#' - [draws_all_infinite()]
#' - [draws_any_finite()]
#' - [draws_all_finite()]
#'
#' @examples
#' m <- rbind(a = c(1, 1, 1, 2, 3),
#'            b = c(2, 4, 0, 2, 3),
#'            c = c(0, 0, 1, 0, 100))
#' x <- rvec(m)
#' x
#' draws_sd(x)
#' draws_var(x)
#' draws_cv(x)
#' @export
draws_sd <- function(x, na_rm = FALSE) {
  UseMethod("draws_sd")
}

## HAS_TESTS
#' @rdname draws_sd
#' @export
draws_sd.rvec_chr <- function(x, na_rm = FALSE) {
  cli::cli_abort("Standard deviation not defined for character.")
}

## HAS_TESTS
#' @rdname draws_sd
#' @export
draws_sd.rvec <- function(x, na_rm = FALSE) {
  check_flag(na_rm)
  m <- field(x, "data")
  if (nrow(m) == 0L)
    NA_real_ ## stats::sd returns NA with zero-length 'x'
  else {
    if (!is.double(m))
      m <- 1 * m
    ans <- matrixStats::rowSds(m, na.rm = na_rm)
    names(ans) <- rownames(m)
    ans
  }
}

#' @rdname draws_sd
#' @export
draws_var <- function(x, na_rm = FALSE) {
  UseMethod("draws_var")
}

## HAS_TESTS
#' @rdname draws_sd
#' @export
draws_var.rvec_chr <- function(x, na_rm = FALSE) {
  cli::cli_abort("Variance not defined for character.")
}

## HAS_TESTS
#' @rdname draws_sd
#' @export
draws_var.rvec <- function(x, na_rm = FALSE) {
  check_flag(na_rm)
  m <- field(x, "data")
  if (nrow(m) == 0L)
    NA_real_ ## stats::var returns NA with zero-length 'x'
  else {
    if (!is.double(m))
      m <- 1 * m
    ans <- matrixStats::rowVars(m, na.rm = na_rm)
    names(ans) <- rownames(m)
    ans
  }
}

#' @rdname draws_sd
#' @export
draws_cv <- function(x, na_rm = FALSE) {
  UseMethod("draws_cv")
}

## HAS_TESTS
#' @rdname draws_sd
#' @export
draws_cv.rvec_chr <- function(x, na_rm = FALSE) {
  cli::cli_abort("Coefficient of variation not defined for character.")
}

## HAS_TESTS
#' @rdname draws_sd
#' @export
draws_cv.rvec <- function(x, na_rm = FALSE) {
  check_flag(na_rm)
  m <- field(x, "data")
  if (nrow(m) == 0L)
    NA_real_ ## stats::var returns NA with zero-length 'x'
  else {
    if (!is.double(m))
      m <- 1 * m
    numerator <- matrixStats::rowSds(m, na.rm = na_rm)
    denominator <- matrixStats::rowMeans2(m, na.rm = na_rm)
    ans <- numerator / denominator
    ans[denominator == 0] <- NA_real_
    names(ans) <- rownames(m)
    ans
  }
}


## 'draws_fun' ----------------------------------------------------------------

#' Apply Summary Function Across Random Draws
#'
#' Summarise the distribution of random draws
#' in an rvec, using a function.
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
#' - [draws_min()]
#' - [draws_max()]
#' - [draws_median()]
#' - [draws_mean()]
#' - [draws_mode()]
#' - [draws_sd()]
#' - [draws_var()]
#' - [draws_cv()]
#' - [draws_ci()]
#' - [draws_quantile()]
#'
#' Check for missing or finite values:
#' - [draws_any_na()]
#' - [draws_all_na()]
#' - [draws_any_infinite()]
#' - [draws_all_infinite()]
#' - [draws_any_finite()]
#' - [draws_all_finite()]
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


## 'prob' ---------------------------------------------------------------------

#' Calculate Probabilities from Random Draws
#'
#' Convert an rvec of logical values (an [rvec_lgl][rvec_lgl()])
#' into a vector of probabilities. 
#'
#' `prob()` is essentially just [draws_mean()]
#' with a different name. The proportion of
#' draws that are `TRUE` is used as an estimate of the
#' underlying probability. The different name
#' can make the intent of the code clearer.
#'
#' @inheritParams draws_all
#' @param x An object of class [rvec_lgl][rvec_lgl()].
#'
#' @returns A logical vector with the same
#' length as `x`.
#'
#' @seealso
#' - [draws_mean()] Means across draws. Gives
#'   the same result as `prob` when applied
#'   to logical rvecs.
#'
#' @examples
#' m <- rbind(c(FALSE,  TRUE),
#'            c(TRUE,   TRUE),
#'            c(FALSE,  FALSE))
#' x <- rvec(m)
#' x
#' prob(x)
#'
#' ## logical rvec created on the fly
#' ## through operations such as '>'
#' m <- rbind(c(-1,  1.3, 2),
#'            c(2, 0.1, -1),
#'            c(Inf, 0, -0.5))
#' y <- rvec(m)
#' y
#' prob(y > 0)
#' prob(y >= 0)
#' prob(y^2 > 0)
#' @export
prob <- function(x, na_rm = FALSE) {
  UseMethod("prob")
}

## HAS_TESTS
#' @rdname prob
#' @export
prob.rvec_lgl <- function(x, na_rm = FALSE) {
  draws_mean(x = x, na_rm = na_rm)
}

## HAS_TESTS
#' @rdname prob
#' @export
prob.logical <- function(x, na_rm = FALSE) {
  as.double(x)
}

