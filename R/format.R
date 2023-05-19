
## 'format' -------------------------------------------------------------------

## HAS_TESTS
#' @export
format.rvec <- function(x, ...) {
    m <- field(x, "data")
    nc <- ncol(m)
    if (nc == 1L) {
        m <- format_elements_rvec(m)
        ans <- m[, 1L]
    }
    else if (nc == 2L) {
        m <- format_elements_rvec(m)
        ans <- paste(m[, 1L], m[, 2L], sep = ",")
    }
    else if (nc == 3L) {
        m <- format_elements_rvec(m)
        ans <- paste(m[, 1L], m[, 2L], m[, 3L], sep = ",")
    }
    else {
        m <- m[, c(1L, nc), drop = FALSE]
        m <- format_elements_rvec(m)
        ans <- paste(m[, 1L], "..", m[, 2L], sep = ",")
    }
    names(ans) <- rownames(m)
    ans
}


## Helpers --------------------------------------------------------------------

## HAS_TESTS
#' Format elements of atomic vectors
#' underlying 'rvec' objects, for use
#' in 'format.rvec'
#'
#' @param x An matrix
#'
#' @returns A character matrix,
#' with the same dimensions as x
#'
#' @noRd
format_elements_rvec <- function(x) {
    if (is.numeric(x))
        ans <- formatC(x, format = "fg")
    else if (is.logical(x))
        ans <- ifelse(x, "T", "F")
    else {
        ans <- sprintf('"%s"', as.character(x))
        ans[is.na(x)] <- NA
    }
    array(ans,
          dim = dim(x),
          dimnames = dimnames(x))
}



#' rvec method for vctrs method 'obj_print_data'
#'
#' Needed because the default method sets
#' the names to NULL
#'
#' @param x An rvec
#' @param ... Not used
#'
#' @returns x, invisibly.
#' 
#' @noRd
#' @export
obj_print_data.rvec <- function(x, ...) {
    if (length(x) == 0) 
        return(invisible(x))
    out <- format(x)
    print(out, quote = FALSE)
    invisible(x)
}
