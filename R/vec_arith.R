
## 'x' is rvec_dbl ------------------------------------------------------------

#' @export
#' @method vec_arith rvec_dbl
vec_arith.rvec_dbl <- function(op, x, y, ...) {
  UseMethod("vec_arith.rvec_dbl", y)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl default
vec_arith.rvec_dbl.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y) # nocov
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl rvec_dbl
vec_arith.rvec_dbl.rvec_dbl <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = TRUE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl rvec_int
vec_arith.rvec_dbl.rvec_int <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = TRUE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl rvec_lgl
vec_arith.rvec_dbl.rvec_lgl <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = TRUE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl double
vec_arith.rvec_dbl.double <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = TRUE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl integer
vec_arith.rvec_dbl.integer <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = TRUE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl logical
vec_arith.rvec_dbl.logical <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = TRUE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl MISSING
vec_arith.rvec_dbl.MISSING <- function(op, x, y, ...) {
    m <- field(x, "data")
    data <- switch(op,
                   `-` = -1 * m,
                   `+` = m,
                   `!` = !m,
                   stop_incompatible_op(op, x, y))
    rvec(data)
}


## 'x' is rvec_int ------------------------------------------------------------

#' @export
#' @method vec_arith rvec_int
vec_arith.rvec_int <- function(op, x, y, ...) {
  UseMethod("vec_arith.rvec_int", y)
}

#' @export
#' @method vec_arith.rvec_int default
vec_arith.rvec_int.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y) # nocov
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_int rvec_dbl
vec_arith.rvec_int.rvec_dbl <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = TRUE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_int rvec_int
vec_arith.rvec_int.rvec_int <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = FALSE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_int rvec_lgl
vec_arith.rvec_int.rvec_lgl <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = FALSE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_int double
vec_arith.rvec_int.double <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = TRUE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_int integer
vec_arith.rvec_int.integer <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = FALSE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_int logical
vec_arith.rvec_int.logical <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = FALSE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_int MISSING
vec_arith.rvec_int.MISSING <- function(op, x, y, ...) {
    m <- field(x, "data")
    data <- switch(op,
                   `-` = -1L * m,
                   `+` = m,
                   `!` = !m,
                   stop_incompatible_op(op, x, y))
    rvec(data)
}


## 'x' is rvec_lgl ------------------------------------------------------------

#' @export
#' @method vec_arith rvec_lgl
vec_arith.rvec_lgl <- function(op, x, y, ...) {
  UseMethod("vec_arith.rvec_lgl", y)
}

#' @export
#' @method vec_arith.rvec_lgl default
vec_arith.rvec_lgl.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y) # nocov
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_lgl rvec_dbl
vec_arith.rvec_lgl.rvec_dbl <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = TRUE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_lgl rvec_int
vec_arith.rvec_lgl.rvec_int <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = FALSE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_lgl rvec_lgl
vec_arith.rvec_lgl.rvec_lgl <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = FALSE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_lgl double
vec_arith.rvec_lgl.double <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = TRUE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_lgl integer
vec_arith.rvec_lgl.integer <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = FALSE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_lgl logical
vec_arith.rvec_lgl.logical <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = FALSE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_lgl MISSING
vec_arith.rvec_lgl.MISSING <- function(op, x, y, ...) {
    m <- field(x, "data")
    data <- switch(op,
                   `-` = -1L * m,
                   `+` = m,
                   `!` = !m,
                   stop_incompatible_op(op, x, y))
    rvec(data)
}


## 'x' is double ------------------------------------------------------------

#' @export
#' @method vec_arith double
vec_arith.double <- function(op, x, y, ...) {
  UseMethod("vec_arith.double", y)
}

## HAS_TESTS
#' @export
#' @method vec_arith.double rvec_dbl
vec_arith.double.rvec_dbl <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = TRUE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.double rvec_int
vec_arith.double.rvec_int <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = TRUE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.double rvec_lgl
vec_arith.double.rvec_lgl <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = TRUE)
}


## 'x' is integer ------------------------------------------------------------

#' @export
#' @method vec_arith integer
vec_arith.integer <- function(op, x, y, ...) {
  UseMethod("vec_arith.integer", y)
}

## HAS_TESTS
#' @export
#' @method vec_arith.integer rvec_dbl
vec_arith.integer.rvec_dbl <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = TRUE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.integer rvec_int
vec_arith.integer.rvec_int <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = FALSE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.integer rvec_lgl
vec_arith.integer.rvec_lgl <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = FALSE)
}


## 'x' is logical ------------------------------------------------------------

## 'vctrs' already has a vec_arith.logical method,
## so don't create one here

## HAS_TESTS
#' @export
#' @method vec_arith.logical rvec_dbl
vec_arith.logical.rvec_dbl <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = TRUE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.logical rvec_int
vec_arith.logical.rvec_int <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = FALSE)
}

## HAS_TESTS
#' @export
#' @method vec_arith.logical rvec_lgl
vec_arith.logical.rvec_lgl <- function(op, x, y, ...) {
    arith_rvec_binary(op, x, y, double = FALSE)
}


## HAS_TESTS
#' Apply binary arithmetic without expanding shared draws
#'
#' Preserve observation recycling separately from draw recycling. The caller
#' specifies whether the result must be double, as in the original S3 method.
#' @noRd
arith_rvec_binary <- function(op, x, y, double) {
    rx <- is_rvec(x)
    ry <- is_rvec(y)
    if (rx && ry) {
        nd <- n_draw_common(x, y, x_arg = "x", y_arg = "y")
        standard <- function(z)
            any(vapply(c("dbl", "int", "lgl"), function(type)
                identical(class(z), c(paste0("rvec_", type), "rvec",
                                      "vctrs_rcrd", "vctrs_vctr")), TRUE))
        compact <- standard(x) && standard(y) &&
            op %in% c("+", "-", "*", "/", "^", "%%", "%/%")
        # Recycling can change base R's NA/NaN propagation on some platforms.
        # Preserve the original layout when both operands contain missing values.
        if (compact && n_draw(x) != n_draw(y) &&
            anyNA(field(if (n_draw(x) == 1L) x else y, "data")) &&
            anyNA(field(if (n_draw(x) == 1L) y else x, "data")))
            compact <- FALSE
        if (!compact) {
            ## Retain conversion dispatch for subclasses and other operators.
            convert <- function(z) {
                if (inherits(z, "rvec_dbl")) rvec_to_rvec_dbl(z, nd)
                else if (inherits(z, "rvec_int")) rvec_to_rvec_int(z, nd)
                else rvec_to_rvec_lgl(z, nd)
            }
            x <- convert(x)
            y <- convert(y)
        }
    }
    mx <- if (rx) field(x, "data") else x
    my <- if (ry) field(y, "data") else y
    if (rx && ry && ncol(mx) != ncol(my)) {
        recycled <- vec_recycle_common(mx, my)
        mx <- recycled[[1L]]
        my <- recycled[[2L]]
        n <- nrow(mx)
        nms <- if (!is.null(rownames(mx))) rownames(mx) else rownames(my)
        if (ncol(mx) == 1L) mx <- as.vector(mx)
        if (ncol(my) == 1L) my <- as.vector(my)
        data <- getExportedValue("base", op)(mx, my)
        dim(data) <- c(n, nd)
        dimnames(data) <- if (is.null(nms)) NULL else list(nms, NULL)
    }
    else
        data <- vec_arith_base(op, mx, my)
    ## Avoid reconstructing an already suitable result matrix. Keep the public
    ## constructor for conversions and unusual results from other operators.
    if (!is.matrix(data) || (double && !is.double(data)))
        return(if (double) rvec_dbl(data) else rvec(data))
    colnames(data) <- NULL
    if (is.double(data)) .new_rvec_dbl(data)
    else if (is.integer(data)) .new_rvec_int(data)
    else if (is.logical(data)) .new_rvec_lgl(data)
    else rvec(data)
}
