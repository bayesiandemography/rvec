## HAS_TESTS
#' @export
vec_restore.rvec <- function(x, to, ...) {
    ## Restoration bypasses the low-level constructors, so enforce the same
    ## draw invariant here. Row slicing must retain the matrix column count.
    check_x_has_at_least_one_col(x$data)
    NextMethod()
}
