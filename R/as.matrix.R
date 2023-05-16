
## as.matrix ------------------------------------------------------------------

## HAS_TESTS
#' @export
as.matrix.rvec <- function(x, ...) {
    field(x, "data")
}
