
arith_data <- function(op, x, y) {
    data_x <- field(x, "data")
    data_y <- field(y, "data")
    vec_arith_base(op, x = data_x, y = data_y)
}


abbr_elements <- function(x) {
    if (is.double(x))
        signif(x, 3L)
    else if (is.logical(x))
        ifelse(x, "T", "F")
    else
        x
}



## NO_TESTS
coerce_matrix <- function(x, ptype) {
    x <- unname(x)
    ncol <- ncol(x)
    matrix_ptype <- matrix(ptype, nrow = 0L, ncol = ncol)
    vec_cast(x, to = matrix_ptype)
}



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
#' Turn a matrix into a list_of object
#'
#' Given a matrix 'm', create a 'list_of'
#' object, of size nrow(m),
#' each element of which is
#' a row of 'm'. The prototype can be
#' specified, or derived from the data.
#'
#' @param m A matrix
#' @param .ptype A prototype, or NULL.
#'
#' @return A list of vectors.
#'
#' @noRd
matrix_to_list_of <- function(m, .ptype = NULL) {
    if (is.null(.ptype))
        .ptype <- vector(mode = typeof(m), length = 0L)
    if (nrow(m) > 0L)
        x <- apply(m,
                   MARGIN = 1L,
                   FUN = function(y) y,
                   simplify = FALSE)
    else
       x <- list()
    as_list_of(x, .ptype = .ptype)
}


n_obs <- function(x) vec_size(x)

prepare_x_for_new_rvec <- function(x) {
    if (is.list(x)) {
        check_lengths_equal(x)
    }
    else if (is.matrix(x)) {
        x <- matrix_to_list_of(x)
    }
    else {
        cl <- class(x)
        cli::cli_abort(c("{.var x} must be a list or a matrix.",
                         "i" = "{.var x} has class {.class cl}."))
    }
    x
}


get_new_rvec_fun <- function(x) {
    type <- typeof(x)
    switch(type,
           character = new_rvec_chr,
           double = new_rvec_dbl,
           integer = new_rvec_int,
           logical = new_rvec_lgl,
           cli::cli_abort("Internal error: Can't handle {.arg x} with type {.type {type}}"))
}

rvec_inner <- function(x, ptype) {
    if (is.null(x))
        data <- matrix(ptype, nrow = 0L, ncol = 1L)
    else if (is.matrix(x)) {
        check_x_has_at_least_one_col(x)
        data <- coerce_matrix(x, ptype)
    }
    else if (is.vector(x)) {
        check_x_at_least_length_one(x)
        data <- vec_cast(x, to = ptype)
    }
    else
        cli::cli_abort(c("{.arg x} must be a matrix, a vector, or {.val NULL}",
                         "i" = "{.arg x} has class {.cls {class(x)}}"))
    new_rvec_fun <- get_new_rvec_fun(ptype)
    new_rvec_fun(data)
}
