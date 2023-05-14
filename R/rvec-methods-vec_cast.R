

## Casts with unequal numbers of draws throw errors even without
## 'check_n_draw_equal', but the message from 'check_n_draw_equal'
## is clearer

## from current rvec to current rvec ------------------------------------------

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


## from rvec to higher-resolution rvec ----------------------------------------

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


## from rvec to lower-resolution rvec in cases where no information lost ------

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


## from base vector to corresponding rvec -------------------------------------

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


## from base vector to higher-resolution rvec ---------------------------------

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


## from base to lower-resolution rvec in cases where no information lost ------

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
