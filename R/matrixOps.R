
## Matrix multiplication

## requires R >= 4.3.0, since matrixOps introduced in 4.3.0

#' Matrix Multiplication with Rvecs
#'
#' Matrix multiplication `%*%` can be used
#' with [rvecs][rvec()], provided that the
#' version of R in use is version 4.3.0 or higher.
#'
#' Multiplying an rvec by a matrix produces
#' an rvec, with no dimensions. This is
#' different from an ordinary R vector:
#' multiplying an ordinary vector by a
#' matrix produces a row or column matrix.
#'
#' @param x,y Vectors, matrices, or rvecs
#' 
#' @returns An rvec if one or both
#' of the inputs is an rvec;
#' otherwise the default
#' `%*%` result.
#'
#' @examples
#' if (getRversion() >= "4.3.0") {
#'   A <- matrix(c(10, 10, 10,
#'                 11, 11, 11),
#'               nrow = 2, byrow = TRUE)
#'   x <- rvec(list(c(1, 2),
#'                  c(3, 4),
#'                  c(5, 6)))
#'   A %*% x
#' }
#' @name rvec-matrix-mult
NULL

#' @noRd
matrixOps.rvec <- function(x, y) {
  if (getRversion() < "4.3.0")
    cli::cli_abort("rvec %*% requires R >= 4.30") # nocov
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

#' @rdname rvec-matrix-mult
methods::setMethod("%*%",
                   methods::signature(x = "Matrix", y = "rvec"),
                   function(x, y) {
                     mx <- as.matrix(x)
                     my <- vctrs::field(y, "data")
                     ans <- mx %*% my
                     rvec(ans)
                   })

#' @rdname rvec-matrix-mult
methods::setMethod("%*%",
                   methods::signature(x = "rvec", y = "Matrix"),
                   function(x, y) {
                     mx <- vctrs::field(x, "data")
                     my <- as.matrix(y)
                     ans <- t(crossprod(mx, my))
                     rvec(ans)
                   })
