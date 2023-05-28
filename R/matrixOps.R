
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
#' @param e1,e2 Vectors, matrices, or rvecs.
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
matrixOps.rvec <- function(e1, e2) {
    is_rvec_e1 <- is_rvec(e1)
    is_rvec_e2 <- is_rvec(e2)
    if (is_rvec_e1 && is_rvec_e2) 
        ans <- sum(e1 * e2)
    else if (!is_rvec_e1 && is_rvec_e2) {
        my <- field(e2, "data")
        ans <- e1 %*% my
        ans <- rvec(ans)
    }
    else {
        mx <- field(e1, "data")
        ans <- t(crossprod(mx, e2))
        ans <- rvec(ans)
    }
    ans
}
