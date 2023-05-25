

## 'is.character' -------------------------------------------------------------

#' @export
is.character.rvec_chr <- function(x) TRUE

#' @export
is.character.rvec_dbl <- function(x) FALSE

#' @export
is.character.rvec_int <- function(x) FALSE

#' @export
is.character.rvec_lgl <- function(x) FALSE


## 'is.double' ----------------------------------------------------------------

#' @export
is.double.rvec_chr <- function(x) FALSE

#' @export
is.double.rvec_dbl <- function(x) TRUE

#' @export
is.double.rvec_int <- function(x) FALSE

#' @export
is.double.rvec_lgl <- function(x) FALSE


## 'is.integer' ----------------------------------------------------------------

#' @export
is.integer.rvec_chr <- function(x) FALSE

#' @export
is.integer.rvec_dbl <- function(x) FALSE

#' @export
is.integer.rvec_int <- function(x) TRUE

#' @export
is.integer.rvec_lgl <- function(x) FALSE


## 'is.logical' ----------------------------------------------------------------

#' @export
is.logical.rvec_chr <- function(x) FALSE

#' @export
is.logical.rvec_dbl <- function(x) FALSE

#' @export
is.logical.rvec_int <- function(x) FALSE

#' @export
is.logical.rvec_lgl <- function(x) TRUE


## 'is.numeric' ----------------------------------------------------------------

#' @export
is.numeric.rvec_chr <- function(x) FALSE

#' @export
is.numeric.rvec_dbl <- function(x) TRUE

#' @export
is.numeric.rvec_int <- function(x) TRUE

#' @export
is.numeric.rvec_lgl <- function(x) FALSE
