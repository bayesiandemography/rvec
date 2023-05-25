
## HAS_TESTS
#' @export
rep.rvec <- function(x, ...) {
    m_old <- field(x, "data")
    nrow_old <- nrow(m_old)
    rownum_old <- seq_len(nrow_old)
    rownum_new <- rep(rownum_old, ...)
    m_new <- m_old[rownum_new, , drop = FALSE]
    rvec(m_new)
}

## HAS_TESTS
#' @export
rep_len.rvec <- function(x, length.out) {
    m_old <- field(x, "data")
    nrow_old <- nrow(m_old)
    rownum_old <- seq_len(nrow_old)
    rownum_new <- rep_len(rownum_old, length.out = length.out)
    m_new <- m_old[rownum_new, , drop = FALSE]
    rvec(m_new)
}

## HAS_TESTS
#' @export
rep.int.rvec <- function(x, times) {
    m_old <- field(x, "data")
    nrow_old <- nrow(m_old)
    rownum_old <- seq_len(nrow_old)
    rownum_new <- rep.int(rownum_old, times = times)
    m_new <- m_old[rownum_new, , drop = FALSE]
    rvec(m_new)
}
