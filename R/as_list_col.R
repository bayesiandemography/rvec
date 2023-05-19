
#' Convert to list column
#'
#' Convert an [rvec][rvec()] or matrix
#' to a list that can be used
#' as a list column in a data frame.
#'
#' @param An [rvecs][rvec()] or matrix.
#'
#' @returns A list:
#' - If `x` is an `rvec`, then the list contains
#' `length(x)` vectors, each of which has
#' `n_draw(x)` elements.
#' - If `x` is a matrix, then the list contains
#' `nrow(x)` vectors, each of which has
#' `ncol(x)` elements.
#'
#' @seealso 
#' - [rvec()] to construct an `rvec`.
#' - [expand_from_rvecs()] to convert a data frame
#' from using `rvec`s to using `draw` and `value`
#' columns.
#' - as_rvar???
#' - converting `rvec`s to 
#' - Functions for summarising and plotting
#' distributions in package
#' [ggdist][https://CRAN.R-project.org/package=ggdist]
#' understand list columns.
#' 
#' @examples
#' l <- list(1:3,
#'           4:6)
#' r <- rvec(l)
#' as_list_col(r)
#' @export
as_list_col <- function(x) {
    UseMethod("as_list_col")
}

## HAS_TESTS
#' @rdname as_list_col
#' @export
as_list_col.rvec <- function(x) {
    m <- field(x, "data")
    ans <- matrix_to_list_of_rows(m)
    ans
}

## HAS_TESTS
#' @rdname as_list_col
#' @export
as_list_col.matrix <- function(x) {
    ans <- matrix_to_list_of_rows(x)
    ans
}
