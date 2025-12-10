
## HAS_TESTS
#' Vectorised If-Else, When Condition is an Rvec
#'
#' A version of
#' [if_else](https://dplyr.tidyverse.org/reference/if_else.html)
#' for the situation where `condition` is an rvec.
#'
#' @param condition An object of class [rvec_lgl][rvec_lgl()].
#' @param true,false Vectors (including rvecs) to use for
#' `TRUE` and `FALSE` values of `condition`.
#' @param missing Vectors to use for `NA` values of
#' `condition`. Optional.
#' @param size Length of output. Optional.
#'
#' @returns An rvec with the same number of
#' [draws][n_draw()] as `condition`.
#'
#' @seealso
#' - base R function [ifelse()] does not not work
#'   correctly if any of the inputs are rvecs.
#' - \pkg{dplyr} function
#'   [if_else](https://dplyr.tidyverse.org/reference/if_else.html)
#'   works correctly if arguments `true`, `false` or `missing`
#'   are rvecs, but not if argument `condition` is an rvec.
#'
#' @examples
#' x <- rvec(list(c(1, 11),
#'                c(2, 5),
#'                c(22, 6)))
#'
#' x > 10 ## rvec_lgl
#'
#' ## if_else_rvec needed when
#' ## 'condition' is an rvec
#' if_else_rvec(x > 10, 10, x)
#'
#' ## dplyr::if_else works when
#' ## 'true', 'false', or 'missing'
#' ## (but not 'condition') are rvecs
#' library(dplyr)
#' if_else(c(TRUE, FALSE, TRUE), x, 100)
#' @export
if_else_rvec <- function(condition, true, false, missing = NULL, size = NULL) {
    if (!is_rvec(condition)) {
        msg <- c("{.arg condition} is not an rvec.",
                 i = "{.arg condition} has class {.cls {class(condition)}}.")
        if (is.logical(condition))
            msg <- c(msg,
                     "*" = "Try dplyr::if_else instead?")
        cli::cli_abort(msg)
    }
    if (!inherits(condition, "rvec_lgl"))
        cli::cli_abort(c("{.arg condition} is not a logical rvec.",
                         i = "{.arg condition} has class {.cls {class(condition)}}."))
    if (is.null(size))
        size <- vec_size(condition)
    has_missing <- !is.null(missing)
    condition <- vec_recycle(condition, size = size)
    true <- vec_recycle(true, size = size)
    false <- vec_recycle(false, size = size)
    if (has_missing)
        missing <- vec_recycle(missing, size = size)
    n_draw <- n_draw(condition)
    if (is_rvec(true)) {
        n_draw_true <- n_draw(true)
        if ((n_draw_true != 1L) && (n_draw_true != n_draw))
            cli::cli_abort(paste("{.arg condition} has {n_draw} draw{?s} but {.arg true}",
                                 "has {n_draw_true} draw{?s}."))
    }
    if (is_rvec(false)) {
        n_draw_false <- n_draw(false)
        if ((n_draw_false != 1L) && (n_draw_false != n_draw))
            cli::cli_abort(paste("{.arg condition} has {n_draw} draw{?s} but {.arg false}",
                                 "has {n_draw_false} draw{?s}."))
    }
    if (has_missing && is_rvec(missing)) {
        n_draw_missing <- n_draw(missing)
        if ((n_draw_missing != 1L) && (n_draw_missing != n_draw))
            cli::cli_abort(paste("{.arg condition} has {n_draw} draw{?s} but",
                                 "{.arg missing} has {n_draw_missing} draw{?s}."))
    }
    if (is_rvec(true))
        true <- field(true, "data")
    ans <- matrix(true, nrow = size, ncol = n_draw)
    condition_m <- field(condition, "data")
    if (is_rvec(false))
        false <- field(false, "data")
    false_m <- matrix(false, nrow = size, ncol = n_draw)
    here <- !is.na(condition_m) & !condition_m
    ans[here] <- false_m[here]
    if (has_missing) {
        if (is_rvec(missing))
            missing <- field(missing, "data")
        missing_m <- matrix(missing, nrow = size, ncol = n_draw)
        here <- is.na(condition_m)
        ans[here] <- missing_m[here]
    }
    else
        ans[is.na(condition_m)] <- NA
    rvec(ans)
}
