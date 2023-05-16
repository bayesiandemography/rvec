

## 'median' -------------------------------------------------------------------

## HAS_TESTS
#' @export
median.rvec_chr <- function(x, na.rm = FALSE, ...) {
    m <- field(x, "data")
    ans <- apply(m, 2L, median, na.rm = na.rm, ...)
    ans <- matrix(ans, nrow = 1L)
    rvec(ans)
}

## HAS_TESTS
#' @export
median.rvec_dbl <- function(x, na.rm = FALSE, ...) {
    m <- field(x, "data")
    if (nrow(m) > 0L)
        ans <- matrixStats::colMedians(m, na.rm = na.rm, ...)
    else
        ans <- rep(NA_real_, times = ncol(m)) ## emulate behavior of base 'median'
    ans <- matrix(ans, nrow = 1L)
    rvec_dbl(ans)
}

## HAS_TESTS
#' @export
median.rvec_int <- function(x, na.rm = FALSE, ...) {
    m <- field(x, "data")
    if (nrow(m) > 0L) {
        ans <- matrixStats::colMedians(m, na.rm = na.rm, ...)
        ans_int <- as.integer(ans) ## emulate behavior of base 'median'
        if (all(ans == ans_int, na.rm = TRUE))        
            ans <- ans_int
    }
    else
        ans <- rep(NA_integer_, times = ncol(m)) ## emulate behavior of base 'median'
    ans <- matrix(ans, nrow = 1L)
    rvec(ans)
}

## HAS_TESTS
#' @export
median.rvec_lgl <- function(x, na.rm = FALSE, ...) {
    m <- field(x, "data")
    if (nrow(m) > 0L) {
        m <- 1 * m
        ans <- matrixStats::colMedians(m, na.rm = na.rm, ...)
        ans_lgl <- as.logical(ans) ## emulate behavior of base 'median'
        if (all(ans == ans_lgl, na.rm = TRUE))        
            ans <- ans_lgl
    }
    else
        ans <- rep(NA, times = ncol(m)) ## emulate behavior of base 'median'
    ans <- matrix(ans, nrow = 1L)
    rvec(ans)
}
