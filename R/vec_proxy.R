
## #' @export
## vec_proxy.rvec <- function(x, ...) {
##     m <- field(x, "data")
##     matrix_to_list_of_rows(m)
## }

## #' @export
## vec_restore.rvec <- function(x, to, ...) {
##     n_draw <- max(lengths(x))
##     x <- lapply(x, rep_len, length.out = n_draw)
##     m <- matrix(unlist(x), nrow = length(x))
##     rvec(m)
## }

    
