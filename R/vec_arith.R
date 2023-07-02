
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
  stop_incompatible_op(op, x, y)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl rvec_dbl
vec_arith.rvec_dbl.rvec_dbl <- function(op, x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    x <- rvec_to_rvec_dbl(x, n_draw = n_draw)
    y <- rvec_to_rvec_dbl(y, n_draw = n_draw)
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl rvec_int
vec_arith.rvec_dbl.rvec_int <- function(op, x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    x <- rvec_to_rvec_dbl(x, n_draw = n_draw)
    y <- rvec_to_rvec_int(y, n_draw = n_draw)
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_dbl rvec_lgl
vec_arith.rvec_dbl.rvec_lgl <- function(op, x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    x <- rvec_to_rvec_dbl(x, n_draw = n_draw)
    y <- rvec_to_rvec_lgl(y, n_draw = n_draw)
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
  stop_incompatible_op(op, x, y)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_int rvec_dbl
vec_arith.rvec_int.rvec_dbl <- function(op, x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    x <- rvec_to_rvec_int(x, n_draw = n_draw)
    y <- rvec_to_rvec_dbl(y, n_draw = n_draw)
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_int rvec_int
vec_arith.rvec_int.rvec_int <- function(op, x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    x <- rvec_to_rvec_int(x, n_draw = n_draw)
    y <- rvec_to_rvec_int(y, n_draw = n_draw)
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_int rvec_lgl
vec_arith.rvec_int.rvec_lgl <- function(op, x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    x <- rvec_to_rvec_int(x, n_draw = n_draw)
    y <- rvec_to_rvec_lgl(y, n_draw = n_draw)
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
  stop_incompatible_op(op, x, y)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_lgl rvec_dbl
vec_arith.rvec_lgl.rvec_dbl <- function(op, x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    x <- rvec_to_rvec_lgl(x, n_draw = n_draw)
    y <- rvec_to_rvec_dbl(y, n_draw = n_draw)
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_lgl rvec_int
vec_arith.rvec_lgl.rvec_int <- function(op, x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    x <- rvec_to_rvec_lgl(x, n_draw = n_draw)
    y <- rvec_to_rvec_int(y, n_draw = n_draw)
    data <- vec_arith_base(op = op,
                           x = field(x, "data"),
                           y = field(y, "data"))
    rvec(data)
}

## HAS_TESTS
#' @export
#' @method vec_arith.rvec_lgl rvec_lgl
vec_arith.rvec_lgl.rvec_lgl <- function(op, x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    x <- rvec_to_rvec_lgl(x, n_draw = n_draw)
    y <- rvec_to_rvec_lgl(y, n_draw = n_draw)
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


## 'x' is logical ------------------------------------------------------------

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
