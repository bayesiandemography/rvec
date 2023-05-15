
## 'format' -------------------------------------------------------------------

## HAS_TESTS
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
    array(ans, dim = dim(x))
}
