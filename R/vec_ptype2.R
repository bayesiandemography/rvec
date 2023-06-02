
## rvec_chr -------------------------------------------------------------------

## HAS_TESTS
#' @export
vec_ptype2.rvec_chr.rvec_chr <- function(x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    ptype_rvec(n_draw = n_draw, ptype = character())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_chr.rvec_dbl <- function(x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    ptype_rvec(n_draw = n_draw, ptype = character())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_chr.rvec_int <- function(x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    ptype_rvec(n_draw = n_draw, ptype = character())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_chr.rvec_lgl <- function(x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    ptype_rvec(n_draw = n_draw, ptype = character())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_chr.character <- function(x, y, ...) x

## HAS_TESTS
#' @export
vec_ptype2.rvec_chr.double <- function(x, y, ...) x

## HAS_TESTS
#' @export
vec_ptype2.rvec_chr.integer <- function(x, y, ...) x

## HAS_TESTS
#' @export
vec_ptype2.rvec_chr.logical <- function(x, y, ...) x


## rvec_dbl -------------------------------------------------------------------

## HAS_TESTS
#' @export
vec_ptype2.rvec_dbl.rvec_chr <- function(x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    ptype_rvec(n_draw = n_draw, ptype = character())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_dbl.rvec_dbl <- function(x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    ptype_rvec(n_draw = n_draw, ptype = double())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_dbl.rvec_int <- function(x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    ptype_rvec(n_draw = n_draw, ptype = double())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_dbl.rvec_lgl <- function(x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    ptype_rvec(n_draw = n_draw, ptype = double())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_dbl.character <- function(x, y, ...) {
    n_draw <- n_draw(x)
    ptype_rvec(n_draw = n_draw, ptype = character())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_dbl.double <- function(x, y, ...) x

## HAS_TESTS
#' @export
vec_ptype2.rvec_dbl.integer <- function(x, y, ...) x

## HAS_TESTS
#' @export
vec_ptype2.rvec_dbl.logical <- function(x, y, ...) x


## rvec_int -------------------------------------------------------------------

## HAS_TESTS
#' @export
vec_ptype2.rvec_int.rvec_chr <- function(x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    ptype_rvec(n_draw = n_draw, ptype = character())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_int.rvec_dbl <- function(x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    ptype_rvec(n_draw = n_draw, ptype = double())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_int.rvec_int <- function(x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    ptype_rvec(n_draw = n_draw, ptype = integer())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_int.rvec_lgl <- function(x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    ptype_rvec(n_draw = n_draw, ptype = integer())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_int.character <- function(x, y, ...) {
    n_draw <- n_draw(x)
    ptype_rvec(n_draw = n_draw, ptype = character())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_int.double <- function(x, y, ...) {
    n_draw <- n_draw(x)
    ptype_rvec(n_draw = n_draw, ptype = double())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_int.integer <- function(x, y, ...) x

## HAS_TESTS
#' @export
vec_ptype2.rvec_int.logical <- function(x, y, ...) x


## rvec_lgl -------------------------------------------------------------------

## HAS_TESTS
#' @export
vec_ptype2.rvec_lgl.rvec_chr <- function(x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    ptype_rvec(n_draw = n_draw, ptype = character())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_lgl.rvec_dbl <- function(x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    ptype_rvec(n_draw = n_draw, ptype = double())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_lgl.rvec_int <- function(x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    ptype_rvec(n_draw = n_draw, ptype = integer())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_lgl.rvec_lgl <- function(x, y, ...) {
    n_draw <- n_draw_common(x = x, y = y, x_arg = "x", y_arg = "y")
    ptype_rvec(n_draw = n_draw, ptype = logical())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_lgl.character <- function(x, y, ...) {
    n_draw <- n_draw(x)
    ptype_rvec(n_draw = n_draw, ptype = character())
}


## HAS_TESTS
#' @export
vec_ptype2.rvec_lgl.double <- function(x, y, ...) {
    n_draw <- n_draw(x)
    ptype_rvec(n_draw = n_draw, ptype = double())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_lgl.integer <- function(x, y, ...) {
    n_draw <- n_draw(x)
    ptype_rvec(n_draw = n_draw, ptype = integer())
}

## HAS_TESTS
#' @export
vec_ptype2.rvec_lgl.logical <- function(x, y, ...) x


## character ------------------------------------------------------------------

## HAS_TESTS
#' @export
vec_ptype2.character.rvec_chr <- function(x, y, ...) y

## HAS_TESTS
#' @export
vec_ptype2.character.rvec_dbl <- function(x, y, ...) {
    n_draw <- n_draw(y)
    ptype_rvec(n_draw = n_draw, ptype = character())
}

## HAS_TESTS
#' @export
vec_ptype2.character.rvec_int <- function(x, y, ...) {
    n_draw <- n_draw(y)
    ptype_rvec(n_draw = n_draw, ptype = character())
}

## HAS_TESTS
#' @export
vec_ptype2.character.rvec_lgl <- function(x, y, ...) {
    n_draw <- n_draw(y)
    ptype_rvec(n_draw = n_draw, ptype = character())
}


## double ---------------------------------------------------------------------

## HAS_TESTS
#' @export
vec_ptype2.double.rvec_chr <- function(x, y, ...) y

## HAS_TESTS
#' @export
vec_ptype2.double.rvec_dbl <- function(x, y, ...) y

## HAS_TESTS
#' @export
vec_ptype2.double.rvec_int <- function(x, y, ...) {
    n_draw <- n_draw(y)
    ptype_rvec(n_draw = n_draw, ptype = double())
}

## HAS_TESTS
#' @export
vec_ptype2.double.rvec_lgl <- function(x, y, ...) {
    n_draw <- n_draw(y)
    ptype_rvec(n_draw = n_draw, ptype = double())
}


## integer --------------------------------------------------------------------

## HAS_TESTS
#' @export
vec_ptype2.integer.rvec_chr <- function(x, y, ...) y

## HAS_TESTS
#' @export
vec_ptype2.integer.rvec_dbl <- function(x, y, ...) y

## HAS_TESTS
#' @export
vec_ptype2.integer.rvec_int <- function(x, y, ...) y

## HAS_TESTS
#' @export
vec_ptype2.integer.rvec_lgl <- function(x, y, ...) {
    n_draw <- n_draw(y)
    ptype_rvec(n_draw = n_draw, ptype = integer())
}


## logical --------------------------------------------------------------------

## HAS_TESTS
#' @export
vec_ptype2.logical.rvec_chr <- function(x, y, ...) y

## HAS_TESTS
#' @export
vec_ptype2.logical.rvec_dbl <- function(x, y, ...) y

## HAS_TESTS
#' @export
vec_ptype2.logical.rvec_int <- function(x, y, ...) y

## HAS_TESTS
#' @export
vec_ptype2.logical.rvec_lgl <- function(x, y, ...) y
