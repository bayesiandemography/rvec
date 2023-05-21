
## 'var' ----------------------------------------------------------------------

## based on
## https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Adding-new-generics

var <- function(x, y = NULL, na.rm = FALSE, use) {
    UseMethod("var")
}

## HAS_TESTS
#' @export
var.default <- function(x, y = NULL, na.rm = FALSE, use) {
    if (missing(use)) 
        use <- if (na.rm) "na.or.complete" else "everything"
    if (is_rvec(y))
        var_rvec_nonrvec(e1 = y,
                         e2 = x,
                         nm_e2 = "x",
                         na.rm = na.rm,
                         use = use)
    else
        stats::var(x = x,
                   y = y,
                   na.rm = na.rm,
                   use = use)
}

## HAS_TESTS
#' @export
var.rvec <- function(x, y = NULL, na.rm = FALSE, use) {
    if (missing(use)) 
        use <- if (na.rm) "na.or.complete" else "everything"
    if (is.null(y))
        var_rvec(x = x,
                 na.rm = na.rm)
    else if (is_rvec(y))
        var_rvec_rvec(x = x,
                      y = y,
                      na.rm = na.rm,
                      use = use)
    else
        var_rvec_nonrvec(e1 = x,
                         e2 = y,
                         nm_e2 = "y",
                         na.rm = na.rm,
                         use = use)
}

#' @export
var.rvec_chr <- function(x, y = NULL, na.rm = FALSE, use) {
    cli::cli_abort("Variance not defined for character vectors.")
}    


## Helper functions -----------------------------------------------------------

## HAS_TESTS
#' Calculate variance for a single rvec
#'
#' @param x Object of class "rvec"
#' @param na.rm Logical flag
#'
#' @returns Object of class "rvec_dbl"
#'
#' @noRd
var_rvec <- function(x, na.rm) {
    m <- 1 * field(x, "data")
    data <- matrixStats::colVars(m, na.rm = na.rm)
    data <- matrix(data, nrow = 1L)
    rvec_dbl(data)
}


## HAS_TESTS
#' Calculate covariance between two rvecs
#'
#' @param x, y Objects of class "rvec"
#' @param na.rm Logical flag
#' @param use String
#'
#' @returns Object of class "rvec_dbl"
#'
#' @noRd
var_rvec_rvec <- function(x, y, na.rm, use) {
    if (inherits(y, "rvec_chr"))
        cli::cli_abort("Variance not defined for character vectors.")
    check_n_draw_equal(x = x,
                       y = y,
                       x_arg = "x",
                       y_arg = "y")
    xy <- vec_recycle_common(x = x, y = y)
    x <- xy$x
    y <- xy$y
    m_x <- field(x, "data")
    m_y <- field(y, "data")
    m_x <- matrix_to_list_of_cols(m_x)
    m_y <- matrix_to_list_of_cols(m_y)
    data <- .mapply(stats::var,
                    dots = list(m_x, m_y),
                    MoreArgs = list(na.rm = na.rm, use = use))
    data <- unlist(data)
    data <- matrix(data, nrow = 1)
    rvec_dbl(data)
}


## HAS_TESTS
#' Calculate covariance between rvec and non-rvec
#'
#' @param e1 An rvec
#' @param e2 A non-rvec
#' @param nm_e2 Name for 'e2' to be used in error messages
#' @param na.rm Logical flag
#' @param use String
#'
#' @returns Object of class "rvec"
#'
#' @noRd
var_rvec_nonrvec <- function(e1, e2, nm_e2, na.rm, use) {
    if (is.atomic(e2)) {
        e1e2 <- vec_recycle_common(e1 = e1, e2 = e2)
        e1 <- e1e2$e1
        e2 <- e1e2$e2
        m <- field(e1, "data")
        if (nrow(m) > 0L) {
            m <- matrix_to_list_of_cols(m)
            data <- lapply(X = m,
                           FUN = stats::var,
                           y = e2,
                           na.rm = na.rm,
                           use = use)
            data <- unlist(data)
        }
        else
            data <- rep.int(NA_real_, times = ncol(m))
        data <- matrix(data, nrow = 1L)
        rvec_dbl(data)
    }
    else {
        cli::cli_abort("{.arg {nm_e2}} has class {.cls {class(e2)}}.")
    }
}

