

## HAS_TESTS
#' Construct a named vector of column indices
#' for a whole data frame
#'
#' Constructed a named vector of indices
#' equivalent to the vectors produced by
#' tidyselect::eval_select, but for every
#' column in a data frame.
#'
#' @param data A data frame.
#'
#' @returns A named integer vector.
#'
#' @noRd
get_colnums_all <- function(data) {
    ans <- seq_along(data)
    names(ans) <- names(data)
    ans
}


## HAS_TESTS
#' Get a named vector of column indices
#' for the grouping variables in a
#' grouped data frame
#'
#' Constructed a named vector of indices
#' equivalent to the vectors produced by
#' tidyselect::eval_select, but for the grouping
#' variables in an object of class
#' "grouped_df".
#' 
#' @param data An object of class
#' "grouped_df" (eg created by dplyr::group_by.)
#'
#' @returns A named integer vector.
#'
#' @noRd
get_colnums_groups <- function(data) {
    attr <- attributes(data)
    nms_data <- attr$names
    groups <- attr$groups
    nms_groups <- setdiff(colnames(groups), ".rows")
    ans <- match(nms_groups, nms_data)
    names(ans) <- nms_groups
    ans
}


## HAS_TESTS
#' Given a code, return an rvec constructor function
#'
#' Helper function for 'get_vec_funs'.
#'
#' @param code A single letter, or "?"
#'
#' @returns A function
#'
#' @noRd
get_rvec_fun <- function(code) {
    switch(code,
           c = new_rvec_chr,
           d = new_rvec_dbl,
           i = new_rvec_int,
           l = new_rvec_lgl,
           "?" = new_rvec,
           cli::cli_abort("Internal error: {.val {code}} is not a valid code."))
}


## HAS_TESTS
#' Given a string specifying types, return a list
#' of rvec constructor functions
#'
#' @param type A string
#'
#' @returns A list of functions.
#'
#' @noRd
get_rvec_funs <- function(type) {
    n <- nchar(type)
    ans <- vector(mode = "list", length = n)
    for (i in seq_len(n)) {
        code <- substr(type, start = i, stop = i)
        ans[[i]] <- get_rvec_fun(code)
    }
    ans
}    


## HAS_TESTS
#' Test whether an object is an rvec
#'
#' Test whether `x` inherits from
#' class `"rvec"`.
#'
#' @param x An object
#'
#' @returns `TRUE` or `FALSE`.
#'
#' @seealso
#' - [rvec()] to create an rvec
#' - [as_rvec()] to convert an object
#' into an rvec
#' - [as.matrix()], [as_list_col()],
#' [as_rvar()][rvec::as_rvar()] to convert an
#' rvec into something else
#' 
#' @examples
#' x <- rvec_dbl()
#' is_rvec(x)
#' @export
is_rvec <- function(x) {
    inherits(x, "rvec")
}


## HAS_TESTS
#' Given one or more interval widths, construct
#' the 'probs' argument to use for calculating
#' quantiles.
#'
#' @param width A numeric vector, where all
#' entries between 0 and 1, inclusive.
#' The elements of widths are unique and
#' decreasing.
#'
#' @return A numeric vector with length
#' 2 * length(width).
#'
#' @noRd
make_probs <- function(width) {
    half_alpha <- 0.5 * (1 - width)
    c(half_alpha, rev(1 - half_alpha))
}


## HAS_TESTS
#' Turn a matrix into a list of columns
#'
#' Given a matrix 'm', create a list,
#' each element of which contains a
#' a column from 'm'.
#'
#' @param m A matrix
#'
#' @return A list of vectors.
#'
#' @noRd
matrix_to_list_of_cols <- function(m, .ptype = NULL) {
    if (ncol(m) > 0L)
        apply(m,
              MARGIN = 2L,
              FUN = function(y) y,
              simplify = FALSE)
    else
        list()
}


## HASx_TESTS
#' Turn a matrix into a list of rows
#'
#' Given a matrix 'm', create a list,
#' each element of which contains a
#' a row from 'm'.
#'
#' @param m A matrix
#'
#' @return A list of vectors.
#'
#' @noRd
matrix_to_list_of_rows <- function(m, .ptype = NULL) {
    if (nrow(m) > 0L)
        apply(m,
              MARGIN = 1L,
              FUN = function(y) y,
              simplify = FALSE)
    else
        list()
}


## HAS_TESTS
#' Choose the appropriate rvec constructor function,
#' based on the type of 'x'
#'
#' @param An object with a type,
#' typically a vector or matrix.
#'
#' @returns A function.
#'
#' @noRd
get_new_rvec_fun <- function(x) {
    type <- typeof(x)
    switch(type,
           character = new_rvec_chr,
           double = new_rvec_dbl,
           integer = new_rvec_int,
           logical = new_rvec_lgl,
           cli::cli_abort("Internal error: {.arg x} is {.obj_type_friendly {x}}"))
}
