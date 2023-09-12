
## Matrix multiplication

## requires R(>= 4.30), since matrixOps introduced in v4.3

#' Matrix multiplication with rvecs
#'
#' Matrix multiplication `%*%` can be used
#' with [rvecs][rvec()]. However, in constrast to
#' standard R vectors, multiplying an rvec
#' by a matrix does not produce a row or
#' column vector. Instead it produces an
#' ordinary rvec, with no dimensions.
#'
#' @param x,y Vectors, matrices, or rvecs.
#'
#' @returns An rvec, if `x` or `y`
#' is an rvec.
#'
#' @examples
#' A <- matrix(c(10, 10, 10,
#'               11, 11, 11),
#'             nrow = 2, byrow = TRUE)
#' x <- rvec(list(c(1, 2),
#'                c(3, 4),
#'                c(5, 6)))
#' A %*% x
#'
#' ## matrix multiplication with an
#' ## ordinary R matrix produces
#' ## a row or column vector
#' y <- c(1, 3, 5)
#' A %*% y
#' @method matrixOps rvec
#' @export
matrixOps.rvec <- function(x, y) {
    is_rvec_x <- is_rvec(x)
    is_rvec_y <- is_rvec(y)
    if (is_rvec_x && is_rvec_y) 
        ans <- sum(x * y)
    else if (!is_rvec_x && is_rvec_y) {
        my <- field(y, "data")
        ans <- x %*% my
        ans <- rvec(ans)
    }
    else {
        mx <- field(x, "data")
        ans <- t(crossprod(mx, y))
        ans <- rvec(ans)
    }
    ans
}
