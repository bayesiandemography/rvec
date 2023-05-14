

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

