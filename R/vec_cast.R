
## Casts with unequal numbers of draws throw errors even without
## 'check_n_draw_equal', but the message from 'check_n_draw_equal'
## is clearer

## to rvec_chr ----------------------------------------------------------------

## HAS_TESTS
#' @export
vec_cast.rvec_chr.rvec_chr <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    x
}

## HAS_TESTS
#' @export
vec_cast.rvec_chr.rvec_dbl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    rvec_to_rvec_chr(x)
}

## HAS_TESTS
#' @export
vec_cast.rvec_chr.rvec_int <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    rvec_to_rvec_chr(x)
}

## HAS_TESTS
#' @export
vec_cast.rvec_chr.rvec_lgl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    rvec_to_rvec_chr(x)
}

## HAS_TESTS
#' @export
vec_cast.rvec_chr.character <- function(x, to, ...) {
    n_draw <- n_draw(to)
    atomic_to_rvec_chr(x, n_draw = n_draw)
}

## HAS_TESTS
#' @export
vec_cast.rvec_chr.double <- function(x, to, ...) {
    n_draw <- n_draw(to)
    atomic_to_rvec_chr(x, n_draw = n_draw)
}

## HAS_TESTS
#' @export
vec_cast.rvec_chr.integer <- function(x, to, ...) {
    n_draw <- n_draw(to)
    atomic_to_rvec_chr(x, n_draw = n_draw)
}

## HAS_TESTS
#' @export
vec_cast.rvec_chr.logical <- function(x, to, ...) {
    n_draw <- n_draw(to)
    atomic_to_rvec_chr(x, n_draw = n_draw)
}


## to rvec_dbl ----------------------------------------------------------------

## HAS_TESTS
#' @export
vec_cast.rvec_dbl.rvec_dbl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    x
}

## HAS_TESTS
#' @export
vec_cast.rvec_dbl.rvec_int <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    rvec_to_rvec_dbl(x)
}

## HAS_TESTS
#' @export
vec_cast.rvec_dbl.rvec_lgl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    rvec_to_rvec_dbl(x)
}

#' @export
vec_cast.rvec_dbl.double <- function(x, to, ...) {
    rvec_to_rvec_dbl(x)
}

## HAS_TESTS
#' @export
vec_cast.rvec_dbl.double <- function(x, to, ...) {
    n_draw <- n_draw(to)
    atomic_to_rvec_dbl(x, n_draw = n_draw)
}

## HAS_TESTS
#' @export
vec_cast.rvec_dbl.integer <- function(x, to, ...) {
    n_draw <- n_draw(to)
    atomic_to_rvec_dbl(x, n_draw = n_draw)
}

## HAS_TESTS
#' @export
vec_cast.rvec_dbl.logical <- function(x, to, ...) {
    n_draw <- n_draw(to)
    atomic_to_rvec_dbl(x, n_draw = n_draw)
}


## to rvec_int ----------------------------------------------------------------

## HAS_TESTS
#' @export
vec_cast.rvec_int.rvec_dbl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    rvec_to_rvec_int(x)
}

## HAS_TESTS
#' @export
vec_cast.rvec_int.rvec_int <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    x
}

## HAS_TESTS
#' @export
vec_cast.rvec_int.rvec_lgl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    rvec_to_rvec_int(x)
}

#' @export
vec_cast.rvec_int.double <- function(x, to, ...) {
    n_draw <- n_draw(to)
    atomic_to_rvec_int(x, n_draw = n_draw)
}

#' @export
vec_cast.rvec_int.integer <- function(x, to, ...) {
    n_draw <- n_draw(to)
    atomic_to_rvec_int(x, n_draw = n_draw)
}

#' @export
vec_cast.rvec_int.logical <- function(x, to, ...) {
    n_draw <- n_draw(to)
    atomic_to_rvec_int(x, n_draw = n_draw)
}


## to rvec_lgl -------------------------------------------------------------------

## HAS_TESTS
#' @export
vec_cast.rvec_lgl.rvec_dbl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    rvec_to_rvec_lgl(x)
}

## HAS_TESTS
#' @export
vec_cast.rvec_lgl.rvec_int <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    rvec_to_rvec_lgl(x)
}

## HAS_TESTS
#' @export
vec_cast.rvec_lgl.rvec_lgl <- function(x, to, ...) {
    check_n_draw_equal(x = x, y = to, x_arg = "x", y_arg = "to")
    x
}

## HAS_TESTS
#' @export
vec_cast.rvec_lgl.double <- function(x, to, ...) {
    n_draw <- n_draw(to)
    atomic_to_rvec_lgl(x, n_draw = n_draw)
}

## HAS_TESTS
#' @export
vec_cast.rvec_lgl.integer <- function(x, to, ...) {
    n_draw <- n_draw(to)
    atomic_to_rvec_lgl(x, n_draw = n_draw)
}

## HAS_TESTS
#' @export
vec_cast.rvec_lgl.logical <- function(x, to, ...) {
    n_draw <- n_draw(to)
    atomic_to_rvec_lgl(x, n_draw = n_draw)
}


## to logical -------------------------------------------------------------------

## THIS IS EXPERIMENTAL

## NO_TESTS
#' @export
vec_cast.logical.rvec_lgl <- function(x, to, ...) {
  x
}
