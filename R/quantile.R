## HAS_TESTS
#' @importFrom stats quantile
#' @export
#' @noRd
quantile.rvec <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE,
                          names = TRUE, type = 7, ...) {
    m <- field(x, "data")
    # Use base quantile algorithms separately within each draw, including
    # version-specific defaults for additional arguments such as fuzz.
    results <- lapply(seq_len(ncol(m)), function(j)
        stats::quantile(m[, j], probs = probs, na.rm = na.rm,
                        names = names, type = type, ...))
    data <- matrix(unlist(results, use.names = FALSE), ncol = ncol(m))
    rownames(data) <- names(results[[1L]])
    rvec(data)
}
