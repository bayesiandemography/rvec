
## 'format' -------------------------------------------------------------------

## HAS_TESTS
#' @export
format.rvec <- function(x, ...) {
    m <- field(x, "data")
    nc <- ncol(m)
    if (nc == 1L) {
        m <- format_rvec_elements(m)
        ans <- m[, 1L]
    }
    else if (nc == 2L) {
        m <- format_rvec_elements(m)
        ans <- paste(m[, 1L], m[, 2L], sep = ",")
    }
    else if (nc == 3L) {
        m <- format_rvec_elements(m)
        ans <- paste(m[, 1L], m[, 2L], m[, 3L], sep = ",")
    }
    else {
        ans <- format_rvec_summaries(m)
    }
    names(ans) <- rownames(m)
    ans
}


## Helpers --------------------------------------------------------------------

## HAS_TESTS
#' Format elements of atomic vectors
#' underlying 'rvec' objects, for use
#' in 'format.rvec' when showing
#' individual elements
#'
#' @param x An matrix
#'
#' @returns A character matrix,
#' with the same dimensions as x
#'
#' @noRd
format_rvec_elements <- function(x) {
    if (is.numeric(x))
        ans <- formatC(x, format = "fg")
    else if (is.logical(x))
        ans <- ifelse(x, "T", "F")
    else {
        ans <- sprintf('"%s"', as.character(x))
        ans[is.na(x)] <- NA
    }
    array(ans,
          dim = dim(x),
          dimnames = dimnames(x))
}


## HAS_TESTS
#' Calculate values to use in 'format.rvec'
#' when showing row summaries
#'
#' @param x A matrix
#'
#' @returns A character vector with length(x)
#'
#' @noRd
format_rvec_summaries <- function(x) {
    if (is.character(x)) {
        tabs <- apply(x, 1L, table, useNA = "no", simplify = FALSE)
        nms_tabs <- lapply(tabs, names)
        i_max <- lapply(tabs, which.max)
        ans <- .mapply(`[[`, dots = list(nms_tabs, i_max), MoreArgs = list())
        ans <- unlist(ans)
        ans <- paste0("..", ans, "..")
    }
    else if (is.numeric(x)) {
        ans <- matrixStats::rowQuantiles(x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
        ans <- sprintf("%s (%s, %s)",
                       prettyNum(ans[, 2L], digits = 2L),
                       prettyNum(ans[, 1L], digits = 2L),
                       prettyNum(ans[, 3L], digits = 2L))
    }
    else {
        ans <- matrixStats::rowMeans2(1 * x, na.rm = TRUE)
        ans <- formatC(ans, format = "fg")
        ans <- paste("p =", ans)
    }
    ans
}




#' rvec method for vctrs method 'obj_print_data'
#'
#' Needed because the default method sets
#' the names to NULL
#'
#' @param x An rvec
#' @param ... Not used
#'
#' @returns x, invisibly.
#'
#' @noRd
#' @export
obj_print_data.rvec <- function(x, ...) {
    if (length(x) == 0)
        return(invisible(x))
    out <- format(x)
    print(out, quote = FALSE)
    invisible(x)
}
