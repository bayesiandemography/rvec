
## as.matrix ------------------------------------------------------------------

## HAS_TESTS
#' @export
as.matrix.rvec <- function(x, ...) {
    field(x, "data")
}


## 'format' -------------------------------------------------------------------

#' @export
format.rvec <- function(x, ...) {
    m <- field(x, "data")
    nc <- ncol(m)
    if (nc == 1L)
        return(m[, 1L])
    if (nc == 2L) {
        m <- format_elements_rvec(m)
        paste(m[, 1L], m[, 2L], sep = ",")
    }
    else if (nc == 3L) {
        m <- format_elements_rvec(m)
        paste(m[, 1L], m[, 2L], m[, 3L], sep = ",")
    }
    else {
        m <- m[, c(1L, nc), drop = FALSE]
        m <- format_elements_rvec(m)
        paste(m[, 1L], "..", m[, 2L], sep = ",")
    }
}


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


## 'n_draw' -------------------------------------------------------------------

## HAS_TESTS
#' Number of draws in a random sample
#'
#' @param An object holding a random sample.
#'
#' @returns A count, or `NULL`, if the
#' vector being sampled as no elements.
#'
#' @examples
#' m <- matrix(1:40, nrow = 4, ncol = 10)
#' x <- rvec(m)
#' n_draw(x)
#' @export
n_draw <- function(x) {
    UseMethod("n_draw")
}

#' @rdname n_draw
#' @export
n_draw.rvec <- function(x) {
    data <- field(x, "data")
    ncol(data)
}


## 'sd' -----------------------------------------------------------------------

## based on
## https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Adding-new-generics

#' @export
sd <- function(x, na.rm = FALSE) {
    UseMethod("sd")
}

## HAS_TESTS
#' @export
sd.default <- function(x, na.rm = FALSE) {
    stats::sd(x, na.rm = na.rm)
}

## HAS_TESTS
#' @export
sd.rvec <- function(x, na.rm = FALSE) {
    m <- 1 * field(x, "data")
    data <- matrixStats::colSds(m, na.rm = na.rm)
    data <- matrix(data, nrow = 1L)
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
sd.rvec_chr <- function(x, na.rm = FALSE) {
    cli::cli_abort("Standard deviation not defined for character vectors.")
}    


## 'var' -----------------------------------------------------------------------

## based on
## https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Adding-new-generics

#' @export
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


## Helper functions

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


## 'vec_arith' ----------------------------------------------------------------

## 'x' is rvec_dbl

#' @export
#' @method vec_arith rvec_dbl
vec_arith.rvec_dbl <- function(op, x, y, ...) {
  UseMethod("vec_arith.rvec_dbl", y)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl default
vec_arith.rvec_dbl.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl rvec_dbl
vec_arith.rvec_dbl.rvec_dbl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl rvec_int
vec_arith.rvec_dbl.rvec_int <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl rvec_lgl
vec_arith.rvec_dbl.rvec_lgl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl double
vec_arith.rvec_dbl.double <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = y)
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl integer
vec_arith.rvec_dbl.integer <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = y)
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl logical
vec_arith.rvec_dbl.logical <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = y)
    rvec_dbl(data)
}


## 'x' is rvec_int

#' @export
#' @method vec_arith rvec_int
vec_arith.rvec_int <- function(op, x, y, ...) {
  UseMethod("vec_arith.rvec_int", y)
}

#' @export
#' @method vec_arith.rvec_int default
vec_arith.rvec_int.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_int rvec_dbl
vec_arith.rvec_int.rvec_dbl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_int rvec_int
vec_arith.rvec_int.rvec_int <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_int rvec_lgl
vec_arith.rvec_int.rvec_lgl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_int double
vec_arith.rvec_int.double <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = y)
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_int integer
vec_arith.rvec_int.integer <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = y)
    rvec(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_int logical
vec_arith.rvec_int.logical <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = y)
    rvec(data)
}


## 'x' is rvec_lgl

#' @export
#' @method vec_arith rvec_lgl
vec_arith.rvec_lgl <- function(op, x, y, ...) {
  UseMethod("vec_arith.rvec_lgl", y)
}

#' @export
#' @method vec_arith.rvec_lgl default
vec_arith.rvec_lgl.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_lgl rvec_dbl
vec_arith.rvec_lgl.rvec_dbl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_lgl rvec_int
vec_arith.rvec_lgl.rvec_int <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_lgl rvec_lgl
vec_arith.rvec_lgl.rvec_lgl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_lgl double
vec_arith.rvec_lgl.double <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = y)
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_lgl integer
vec_arith.rvec_lgl.integer <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = y)
    rvec(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_lgl logical
vec_arith.rvec_lgl.logical <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = y)
    rvec(data)
}


## 'x' is double

#' @export
#' @method vec_arith double
vec_arith.double <- function(op, x, y, ...) {
  UseMethod("vec_arith.double", y)
}

## HAS_TESTS
#' @export
#' @method vec_arith.double rvec_dbl
vec_arith.double.rvec_dbl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = x,
                           y = field(y, "data"))
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.double rvec_int
vec_arith.double.rvec_int <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = x,
                           y = field(y, "data"))
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.double rvec_lgl
vec_arith.double.rvec_lgl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = x,
                           y = field(y, "data"))
    rvec_dbl(data)
}


## 'x' is integer

#' @export
#' @method vec_arith integer
vec_arith.integer <- function(op, x, y, ...) {
  UseMethod("vec_arith.integer", y)
}

## HAS_TESTS
#' @export
#' @method vec_arith.integer rvec_dbl
vec_arith.integer.rvec_dbl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = x,
                           y = field(y, "data"))
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.integer rvec_int
vec_arith.integer.rvec_int <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = x,
                           y = field(y, "data"))
    rvec(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.integer rvec_lgl
vec_arith.integer.rvec_lgl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = x,
                           y = field(y, "data"))
    rvec(data)
}

## 'x' is logical

## 'vctrs' already has a vec_arith.logical method,
## so don't create one here

## HAS_TESTS
#' @export
#' @method vec_arith.logical rvec_dbl
vec_arith.logical.rvec_dbl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = x,
                           y = field(y, "data"))
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.logical rvec_int
vec_arith.logical.rvec_int <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = x,
                           y = field(y, "data"))
    rvec(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.logical rvec_lgl
vec_arith.logical.rvec_lgl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = x,
                           y = field(y, "data"))
    rvec(data)
}


## 'vec_cast' -----------------------------------------------------------------

## Casts with unequal numbers of draws throw errors even without
## 'check_n_draw_equal', but the message from 'check_n_draw_equal'
## is clearer

## from current rvec to current rvec

## HAS_TESTS
#' @export
vec_cast.rvec_chr.rvec_chr <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    x
}

## HAS_TESTS
#' @export
vec_cast.rvec_dbl.rvec_dbl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    x
}

## HAS_TESTS
#' @export
vec_cast.rvec_int.rvec_int <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    x
}

## HAS_TESTS
#' @export
vec_cast.rvec_lgl.rvec_lgl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    x
}


## from rvec to higher-resolution rvec

## HAS_TESTS
#' @export
vec_cast.rvec_dbl.rvec_int <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    data <- field(x, "data")
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
vec_cast.rvec_dbl.rvec_lgl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    data <- field(x, "data")
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
vec_cast.rvec_int.rvec_lgl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    data <- field(x, "data")
    rvec_int(data)
}

## from rvec to lower-resolution rvec in cases
## where no information lost

## HAS_TESTS
#' @export
vec_cast.rvec_int.rvec_dbl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    data <- field(x, "data")
    ptype <- matrix(integer(), nrow = 0L, ncol = ncol(data))
    data <- vec_cast(data, ptype)
    rvec_int(data)
}

## HAS_TESTS
#' @export
vec_cast.rvec_lgl.rvec_dbl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    data <- field(x, "data")
    ptype <- matrix(logical(), nrow = 0L, ncol = ncol(data))
    data <- vec_cast(data, ptype)
    rvec_lgl(data)
}

## HAS_TESTS
#' @export
vec_cast.rvec_lgl.rvec_int <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    data <- field(x, "data")
    ptype <- matrix(logical(), nrow = 0L, ncol = ncol(data))
    data <- vec_cast(data, ptype)
    rvec_lgl(data)
}


## from base vector to corresponding rvec

## HAS_TESTS
#' @export
vec_cast.rvec_chr.character <- function(x, to, ...) {
    m <- matrix(x,
                nrow = length(x),
                ncol = n_draw(to))
    rvec_chr(m)
}

## HAS_TESTS
#' @export
vec_cast.rvec_dbl.double <- function(x, to, ...) {
    m <- matrix(x,
                nrow = length(x),
                ncol = n_draw(to))
    rvec_dbl(m)
}

## HAS_TESTS
#' @export
vec_cast.rvec_int.integer <- function(x, to, ...) {
    m <- matrix(x,
                nrow = length(x),
                ncol = n_draw(to))
    rvec_int(m)
}

## HAS_TESTS
#' @export
vec_cast.rvec_lgl.logical <- function(x, to, ...) {
    m <- matrix(x,
                nrow = length(x),
                ncol = n_draw(to))
    rvec_lgl(m)
}


## from base vector to higher-resolution rvec

## HAS_TESTS
#' @export
vec_cast.rvec_dbl.integer <- function(x, to, ...) {
    m <- matrix(x,
                nrow = length(x),
                ncol = n_draw(to))
    rvec_dbl(m)
}

## HAS_TESTS
#' @export
vec_cast.rvec_dbl.logical <- function(x, to, ...) {
    m <- matrix(x,
                nrow = length(x),
                ncol = n_draw(to))
    rvec_dbl(m)
}

## HAS_TESTS
#' @export
vec_cast.rvec_int.logical <- function(x, to, ...) {
    m <- matrix(x,
                nrow = length(x),
                ncol = n_draw(to))
    rvec_int(m)
}


## from base to lower-resolution rvec in cases
## where no information lost

## HAS_TESTS
#' @export
vec_cast.rvec_int.double <- function(x, to, ...) {
    m <- matrix(x,
                nrow = length(x),
                ncol = n_draw(to))
    ptype <- matrix(integer(),
                    nrow = 0L,
                    ncol = n_draw(to))
    data <- vec_cast(m, ptype)
    rvec_int(data)
}

## HAS_TESTS
#' @export
vec_cast.rvec_lgl.double <- function(x, to, ...) {
    m <- matrix(x,
                nrow = length(x),
                ncol = n_draw(to))
    ptype <- matrix(logical(),
                    nrow = 0L,
                    ncol = n_draw(to))
    data <- vec_cast(m, ptype)
    rvec_lgl(data)
}

## HAS_TESTS
#' @export
vec_cast.rvec_lgl.integer <- function(x, to, ...) {
    m <- matrix(x,
                nrow = length(x),
                ncol = n_draw(to))
    ptype <- matrix(logical(),
                    nrow = 0L,
                    ncol = n_draw(to))
    data <- vec_cast(m, ptype)
    rvec_lgl(data)
}


## 'vec_math' -----------------------------------------------------------------

## Note that vec_math methods not currently implemented for
## 'median' or for 'sd' and 'var' (neither of which
## are generic functions in base R), so these have their
## own methods


#' @export
vec_math.rvec_dbl <- function(.fn, .x, ...) {
    m <- field(.x, "data")
    ## summary function in Summary group, has matrixStats fun:
    if (.fn %in% c("prod", "sum", "any", "all")) {
        matrix_fun <- switch(.fn,
                             prod = function(x, ...)
                                 matrixStats::colProds(x, method = "expSumLog", ...),
                             sum = matrixStats::colSums2,
                             any = matrixStats::colAnys,
                             all = matrixStats::colAlls)
        data <- matrix_fun(m, ...)
        data <- matrix(data, nrow = 1L)
    }
    ## summary function not in Summary group but implemented by vec_math:
    else if (.fn == "mean") {
        data <- matrixStats::colMeans2(m, ...)
        data <- matrix(data, nrow = 1L)
    }        
    ## in Math group and has matrixStats fun
    else if (.fn %in% c("cummax", "cummin", "cumprod", "cumsum")) {
        matrix_fun <- switch(.fn,
                             cummax = matrixStats::colCummaxs,
                             cummin = matrixStats::colCummins,
                             cumprod = matrixStats::colCumprods,
                             cumsum = matrixStats::colCumsums)
        data <- matrix_fun(m, ...)
    }
    ## not in Math group but implemented by vec_math:
    else if (.fn %in% c("is.nan", "is.finite", "is.infinite")) {
        .fn <- match.fun(.fn)
        data <- .fn(m, ...)
    }
    ## everything else in Math group:
    else {
        .fn <- match.fun(.fn)
        data <- .fn(m, ...)
    }
    new_rvec(data)
}

## give same types as base functions
#' @export
vec_math.rvec_int <- function(.fn, .x, ...) {
    ans_original <- vec_math.rvec_dbl(.fn = .fn, .x = .x, ...)
    if (.fn == "sum") {
        m <- field(ans_original, "data")
        rvec_int(m)
    }
    else if (.fn == "cumprod") {
        m <- field(ans_original, "data")
        rvec_dbl(m)
    }
    else
        ans_original
}

#' @export
vec_math.rvec_lgl <- function(.fn, .x, ...) {
    data <- field(.x, "data")
    .x <- rvec_int(data)
    vec_math.rvec_int(.fn = .fn, .x = .x, ...)
}


## 'vec_ptype_abbr' -----------------------------------------------------------

#' @export
vec_ptype_abbr.rvec_chr <- function(x, ...) {
    n <- n_draw(x)
    sprintf("rchr<%d>", n)
}

#' @export
vec_ptype_abbr.rvec_dbl <- function(x, ...) {
    n <- n_draw(x)
    sprintf("rdbl<%d>", n)
}

#' @export
vec_ptype_abbr.rvec_int <- function(x, ...) {
    n <- n_draw(x)
    sprintf("rint<%d>", n)
}

#' @export
vec_ptype_abbr.rvec_lgl <- function(x, ...) {
    n <- n_draw(x)
    sprintf("rlgl<%d>", n)
}


## 'vec_ptype_full' -----------------------------------------------------------

#' @export
vec_ptype_full.rvec_chr <- function(x, ...) {
    n <- n_draw(x)
    sprintf("rvec_chr<%d>", n)
}

#' @export
vec_ptype_full.rvec_dbl <- function(x, ...) {
    n <- n_draw(x)
    sprintf("rvec_dbl<%d>", n)
}

#' @export
vec_ptype_full.rvec_int <- function(x, ...) {
    n <- n_draw(x)
    sprintf("rvec_int<%d>", n)
}

#' @export
vec_ptype_full.rvec_lgl <- function(x, ...) {
    n <- n_draw(x)
    sprintf("rvec_lgl<%d>", n)
}


## 'vec_ptype2' ---------------------------------------------------------------

## When combining an rvec objects with a vector, it is
## tempting to compare n_draw with the length of the vector,
## but this length is not available since vec_ptype2 works
## with zero-length prototypes, not the original vector.
## Instead do comparison within vec_cast.

## rvec with current rvec

#' @export
vec_ptype2.rvec_chr.rvec_chr <- function(x, y, ...) {
    check_n_draw_equal(x = x, y = y, x_arg = "x", y_arg = "y")
    x
}
    
#' @export
vec_ptype2.rvec_dbl.rvec_dbl <- function(x, y, ...) {
    check_n_draw_equal(x = x, y = y, x_arg = "x", y_arg = "y")
    x
}

#' @export
vec_ptype2.rvec_int.rvec_int <- function(x, y, ...) {
    check_n_draw_equal(x = x, y = y, x_arg = "x", y_arg = "y")
    x
}

#' @export
vec_ptype2.rvec_lgl.rvec_lgl <- function(x, y, ...) {
    check_n_draw_equal(x = x, y = y, x_arg = "x", y_arg = "y")
    x
}

## rvec with higher-resolution rvec 

#' @export
vec_ptype2.rvec_dbl.rvec_int <- function(x, y, ...) {
    check_n_draw_equal(x = x, y = y, x_arg = "x", y_arg = "y")
    x
}

#' @export
vec_ptype2.rvec_int.rvec_dbl <- function(x, y, ...) {
    check_n_draw_equal(x = x, y = y, x_arg = "x", y_arg = "y")
    y
}

#' @export
vec_ptype2.rvec_dbl.rvec_lgl <- function(x, y, ...) {
    check_n_draw_equal(x = x, y = y, x_arg = "x", y_arg = "y")
    x
}

#' @export
vec_ptype2.rvec_lgl.rvec_dbl <- function(x, y, ...) {
    check_n_draw_equal(x = x, y = y, x_arg = "x", y_arg = "y")
    y
}

#' @export
vec_ptype2.rvec_int.rvec_lgl <- function(x, y, ...) {
    check_n_draw_equal(x = x, y = y, x_arg = "x", y_arg = "y")
    x
}

#' @export
vec_ptype2.rvec_lgl.rvec_int <- function(x, y, ...) {
    check_n_draw_equal(x = x, y = y, x_arg = "x", y_arg = "y")
    y
}
 

## base vector with corresponding rvec

#' @export
vec_ptype2.character.rvec_chr <- function(x, y, ...) y

#' @export
vec_ptype2.rvec_chr.character <- function(x, y, ...) x

#' @export
vec_ptype2.double.rvec_dbl <- function(x, y, ...) y

#' @export
vec_ptype2.rvec_dbl.double <- function(x, y, ...) x

#' @export
vec_ptype2.integer.rvec_int <- function(x, y, ...) y

#' @export
vec_ptype2.rvec_int.integer <- function(x, y, ...) x

#' @export
vec_ptype2.logical.rvec_lgl <- function(x, y, ...) y

#' @export
vec_ptype2.rvec_lgl.logical <- function(x, y, ...) x


## base vector and higher-resolution rvec

#' @export
vec_ptype2.integer.rvec_dbl <- function(x, y, ...) y

#' @export
vec_ptype2.rvec_dbl.integer <- function(x, y, ...) x

#' @export
vec_ptype2.logical.rvec_dbl <- function(x, y, ...) y

#' @export
vec_ptype2.rvec_dbl.logical <- function(x, y, ...) x

#' @export
vec_ptype2.logical.rvec_dbl <- function(x, y, ...) y

#' @export
vec_ptype2.rvec_dbl.logical <- function(x, y, ...) x

