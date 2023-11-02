
## 'order' --------------------------------------------------------------------

## (implemented via 'xtfrm')


## 'rank' --------------------------------------------------------------------

## based on
## https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Adding-new-generics

#' Sample Ranks, Including Rvecs
#'
#' Calculate sample ranks, for ordinary vectors, or for rvecs.
#' In the case of rvecs, ranks are calculated independently
#' for each draw.
#'
#' To enable different behavior for rvecs and for ordinary vectors,
#' the base R function [base::rank()] is turned into a generic,
#' with `base::rank()` as the default.
#'
#' For details on the calculations, see the documentation
#' for [base::rank()].
#'
#' @param x An ordinary vector or an [rvec::rvec()].
#' @param na.last Treatment of `NA`s. Options are
#' `TRUE`, `FALSE`, or `keep`. See [base::rank() for details.
#' @param ties.method Treatment of ties.
#' See [base::rank()] for details.
#'
#' @returns An object of class `rvec_int()` if `x` is
#' an rvec. Otherwise an ordinary integer vector.
#'
#' @examples
#' x <- rvec(list(c(3, 30),
#'                c(0, 100)))
#' rank(x)
#' @export
#' @rdname rank
rank <- function(x,
                 na.last = TRUE,
                 ties.method = c("average", "first", "last", "random", "max", "min")) {
    UseMethod("rank")
}

## HAS_TESTS
#' @export
rank.default <- function(x,
                         na.last = TRUE,
                         ties.method = c("average", "first", "last", "random", "max", "min")) {
    ties.method <- match.arg(ties.method)
    base::rank(x = x,
               na.last = na.last,
               ties.method = ties.method)
}

## HAS_TESTS
#' @export
rank.rvec <- function(x,
                      na.last = TRUE,
                      ties.method = c("average", "first", "last", "random", "max", "min")) {
    ties.method <- match.arg(ties.method)
    m <- vctrs::field(x, "data")
    ## 'colRanks' implements 'na.last = "keep"', and only works with numeric or logical
    if (identical(na.last, TRUE) || identical(na.last, FALSE)) {
        if (anyNA(m) || is.character(m))
            ans <- apply(m, 2L, rank, na.last = na.last, ties.method = ties.method)
        else
            ans <- matrixStats::colRanks(m, ties.method = ties.method, preserveShape = TRUE)
    }
    else if (identical(na.last, "keep")) {
        if (is.character(m))
            ans <- apply(m, 2L, rank, na.last = na.last, ties.method = ties.method)
        else
            ans <- matrixStats::colRanks(m, ties.method = ties.method, preserveShape = TRUE)
    }
    else {
        cli::cli_abort("{.arg na.last} is {.val {na.last}}.")
    }
    rownames(ans) <- rownames(m)
    rvec::rvec_int(ans)
}


## 'sort' --------------------------------------------------------------------

## (implemented via 'xtfrm')


## 'xtfrm' --------------------------------------------------------------------

## HAS_TESTS
#' @export
xtfrm.rvec <- function(x) {
    n_draw <- n_draw(x)
    if (n_draw != 1L)
        cli::cli_abort("Sorting of rvec only defined when {.var n_draw} is 1.")
    xtfrm(as.matrix(x))
}


    

