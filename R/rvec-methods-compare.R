




## ## Methods related to equality

## ## Use our own methods rather than the vctrs ones because
## ## we want to apply in parallel across draws

## as_rvec_dbl <- function(x) {
##     if (is.matrix(x)) {
##         data <- x
##     }
##     else if (is_rvec(x)) {
##         data <- field(x, "data")
##     }
##     else {
##         cli::cli_abort(c("{.arg x} must be a matrix or an rvec",
##                          "i" = "{.arg x} has class {.cls {class(x)}}"))
##     }
##     rvec_dbl(data)
## }


## as_rvec_chr <- function(x) {
##     if (is.matrix(x)) {
##         data <- x
##     }
##     else if (is_rvec(x)) {
##         data <- field(x, "data")
##     }
##     else {
##         cli::cli_abort(c("{.arg x} must be a matrix or an rvec",
##                          "i" = "{.arg x} has class {.cls {class(x)}}"))
##     }
##     if (!is.character(data))
##         data <- array(as.character(data),
##                       dim = dim(data),
##                       dimnames = dimnames(data))
##     rvec_chr(data)
## }

    
    
        
#' @export
`==.rvec` <- function(e1, e2) {
    args <- vec_recycle_common(e1, e2)
    e1 <- args[[1L]]
    e2 <- args[[2L]]
    m1 <- as.matrix(e1)
    m2 <- as.matrix(e2)
    data <- m1 == m2
    rvec_lgl(data)
}


## rvec_cast_compare <- function(e1, e2) {
##     is_chr_1 <- inherits(e1, "rvec_chr")
##     is_chr_2 <- inherits(e2, "rvec_chr")
##     ## case 1: return character
##     if (is_chr_1 || is_chr_2) {
##         if (!is_chr_1)
##             e1 <- as_rvec_chr(e1)
##         if (!is_chr_2)
##             e2 <- as_rvec_chr(e2)
##         vec_cast_compare(e1, e2)
##     }
##     ## case 2: return double
##     else {
##         is_dbl_1 <- inherits(e1, "rvec_dbl")
##         is_dbl_2 <- inherits(e2, "rvec_dbl")
##         if (!is_dbl_1)
##             e1 <- as_rvec_dbl(e1)
##         if (!is_dbl_2)
##             e2 <- as_rvec_dbl(e2)
##         vec_cast_compare(e1, e1)
##     }
## }    

## compare_rvec <- function(e1, e2, op) {
##     args <- vec_recycle_common(e1, e2)
##     e1 <- args[[1L]]
##     e2 <- args[[2L]]
##     args <- rvec_cast_compare(e1, e2)
##     e1 <- args[[1L]]
##     e2 <- args[[2L]]
##     m1 <- field(e1, "data")
##     m2 <- field(e2, "data")
##     fun <- match.fun(op)
##     data <- fun(m1, m2)
##     rvec_lgl(data)
## }
    

## #' @export
## `==.rvec` <- function(e1, e2) {
##     compare_rvec(e1 = e1, e2 = e2, op = "==")
## }

## #' @export
## `!=.rvec` <- function(e1, e2) {
##     compare_rvec(e1 = e1, e2 = e2, op = "!=")
## }

## #' @export
## `<.rvec` <- function(e1, e2) {
##     compare_rvec(e1 = e1, e2 = e2, op = "<")
## }

## #' @export
## `<=.rvec` <- function(e1, e2) {
##     compare_rvec(e1 = e1, e2 = e2, op = "<=")
## }

## #' @export
## `>=.rvec` <- function(e1, e2) {
##     compare_rvec(e1 = e1, e2 = e2, op = ">=")
## }

## #' @export
## `>.rvec` <- function(e1, e2) {
##     compare_rvec(e1 = e1, e2 = e2, op = ">")
## }

