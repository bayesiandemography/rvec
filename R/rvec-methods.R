
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

#' Number of draws in a random sample
#'
#' @param An object holding a random sample.
#'
#' @returns A count, or `NULL`, if the
#' vector being sampled as no elements.
#'
#' @examples
#' data <- matrix(sample(c(TRUE, FALSE), size = 40, replace = TRUE),
#'                nrow = 4)
#' x <- rvec(data)
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


## 'vec_arith' ----------------------------------------------------------------

## 'x' is rvec_dbl

#' @export
#' @method vec_arith rvec_dbl
vec_arith.rvec_dbl <- function(op, x, y, ...) {
  UseMethod("vec_arith.rvec_dbl", y)
}

#' @export
#' @method vec_arith.rvec_dbl default
vec_arith.rvec_dbl.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.rvec_dbl rvec_dbl
vec_arith.rvec_dbl.rvec_dbl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec_dbl(data)
}

#' @export
#' @method vec_arith.rvec_dbl rvec_int
vec_arith.rvec_dbl.rvec_int <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec_dbl(data)
}

#' @export
#' @method vec_arith.rvec_dbl rvec_lgl
vec_arith.rvec_dbl.rvec_lgl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec_dbl(data)
}

#' @export
#' @method vec_arith.rvec_dbl double
vec_arith.rvec_dbl.double <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = y)
    rvec_dbl(data)
}

#' @export
#' @method vec_arith.rvec_dbl integer
vec_arith.rvec_dbl.integer <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = y)
    rvec_dbl(data)
}

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

#' @export
#' @method vec_arith.rvec_int rvec_dbl
vec_arith.rvec_int.rvec_dbl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec_dbl(data)
}

#' @export
#' @method vec_arith.rvec_int rvec_int
vec_arith.rvec_int.rvec_int <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec(data)
}

#' @export
#' @method vec_arith.rvec_int rvec_lgl
vec_arith.rvec_int.rvec_lgl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec(data)
}

#' @export
#' @method vec_arith.rvec_int double
vec_arith.rvec_int.double <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = y)
    rvec_dbl(data)
}

#' @export
#' @method vec_arith.rvec_int integer
vec_arith.rvec_int.integer <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = y)
    rvec(data)
}

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

#' @export
#' @method vec_arith.rvec_lgl rvec_dbl
vec_arith.rvec_lgl.rvec_dbl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec_dbl(data)
}

#' @export
#' @method vec_arith.rvec_lgl rvec_int
vec_arith.rvec_lgl.rvec_int <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec(data)
}

#' @export
#' @method vec_arith.rvec_lgl rvec_lgl
vec_arith.rvec_lgl.rvec_lgl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec(data)
}

#' @export
#' @method vec_arith.rvec_lgl double
vec_arith.rvec_lgl.double <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = y)
    rvec_dbl(data)
}

#' @export
#' @method vec_arith.rvec_lgl integer
vec_arith.rvec_lgl.integer <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = y)
    rvec(data)
}

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

#' @export
#' @method vec_arith.double rvec_dbl
vec_arith.double.rvec_dbl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = x,
                           y = field(y, "data"))
    rvec_dbl(data)
}

#' @export
#' @method vec_arith.double rvec_int
vec_arith.double.rvec_int <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = x,
                           y = field(y, "data"))
    rvec_dbl(data)
}

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

#' @export
#' @method vec_arith.integer rvec_dbl
vec_arith.integer.rvec_dbl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = x,
                           y = field(y, "data"))
    rvec_dbl(data)
}

#' @export
#' @method vec_arith.integer rvec_int
vec_arith.integer.rvec_int <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = x,
                           y = field(y, "data"))
    rvec(data)
}

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

#' @export
#' @method vec_arith.logical rvec_dbl
vec_arith.logical.rvec_dbl <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = x,
                           y = field(y, "data"))
    rvec_dbl(data)
}

#' @export
#' @method vec_arith.logical rvec_int
vec_arith.logical.rvec_int <- function(op, x, y, ...) {
    data <- vec_arith_base(op = op,
                           x = x,
                           y = field(y, "data"))
    rvec(data)
}

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

#' @export
vec_cast.rvec_chr.rvec_chr <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    x
}

#' @export
vec_cast.rvec_dbl.rvec_dbl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    x
}

#' @export
vec_cast.rvec_int.rvec_int <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    x
}

#' @export
vec_cast.rvec_lgl.rvec_lgl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    x
}


## from rvec to higher-resolution rvec

#' @export
vec_cast.rvec_dbl.rvec_int <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    data <- field(x, "data")
    rvec_dbl(data)
}

#' @export
vec_cast.rvec_dbl.rvec_lgl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    data <- field(x, "data")
    rvec_dbl(data)
}

#' @export
vec_cast.rvec_int.rvec_lgl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    data <- field(x, "data")
    rvec_int(data)
}


## from base vector to corresponding rvec

#' @export
vec_cast.rvec_chr.character <- function(x, to, ...) {
    check_length_n_draw_compatible(x = x,
                                   y = to,
                                   x_arg = "x",
                                   y_arg = "to")
    rvec_chr(x)
}

#' @export
vec_cast.rvec_dbl.double <- function(x, to, ...) {
    check_length_n_draw_compatible(x = x,
                                   y = to,
                                   x_arg = "x",
                                   y_arg = "to")
    rvec_dbl(x)
}

#' @export
vec_cast.rvec_int.integer <- function(x, to, ...) {
    check_length_n_draw_compatible(x = x,
                                   y = to,
                                   x_arg = "x",
                                   y_arg = "to")
    rvec_int(x)
}

#' @export
vec_cast.rvec_lgl.logical <- function(x, to, ...) {
    check_length_n_draw_compatible(x = x,
                                   y = to,
                                   x_arg = "x",
                                   y_arg = "to")
    rvec_lgl(x)
}


## from base vector to higher-resolution rvec

#' @export
vec_cast.rvec_dbl.integer <- function(x, to, ...) {
    check_length_n_draw_compatible(x = x,
                                   y = to,
                                   x_arg = "x",
                                   y_arg = "to")
    rvec_dbl(x)
}

#' @export
vec_cast.rvec_dbl.logical <- function(x, to, ...) {
    check_length_n_draw_compatible(x = x,
                                   y = to,
                                   x_arg = "x",
                                   y_arg = "to")
    rvec_dbl(x)
}

#' @export
vec_cast.rvec_int.logical <- function(x, to, ...) {
    check_length_n_draw_compatible(x = x,
                                   y = to,
                                   x_arg = "x",
                                   y_arg = "to")
    rvec_int(x)
}


## 'vec_math' -----------------------------------------------------------------

## Note that vec_math methods not currently implemented for
## 'median' (not clear why) or for 'sd' and 'var' (neither of which
## are generic functions in base R).
#' @export
vec_math.rvec <- function(.fn, .x, ...) {
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
    ans_original <- vec_math.rvec(.fn = .fn, .x = .x, ...)
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

