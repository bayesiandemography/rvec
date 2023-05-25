
## HAS_TESTS
#' Insert a column into a data frame
#'
#' Try not to change class and attributes
#' of 'df'.
#'
#' Assume that 'df' does not already
#' have a column called nm.
#'
#' @param df A data frame.
#' @param value The values to insert.
#' @param after Column number. The value is inserted
#' before this. Min value is 0, max is ncol(df).
#' @param nm Name of the new column.
#'
#' @returns A data frame with one more column than 'df'.
#'
#' @noRd
append_col <- function(df, value, after, nm) {
    n <- ncol(df)
    if (n == 0L)
        cli::cli_abort("Internal error: {.arg df} has length 0")
    if (!(after %in% 0:n))
        cli::cli_abort("Internal error: {.arg after} invalid")
    if (nm %in% names(df))
        cli::cli_abort("Internal error: {.arg df} already has column called {.val {nm}}")
    df[[nm]] <- value
    perm <- order(c(seq_len(n), after), c(rep(0L, n), 1))
    df[perm]
}
        

## HAS_TESTS
#' Create a rvec_chr prototype
#'
#' @param x An rvec with the desired
#' number of draws.
#'
#' @returns An rvec_chr
#'
#' @noRd
as_ptype_rvec_chr <- function(x) {
    n_draw <- n_draw(x)
    m <- matrix(character(),
                nrow = 0L,
                ncol = n_draw)
    new_rvec_chr(m)
}


## HAS_TESTS
#' Create a rvec_dbl prototype
#'
#' @param x An rvec with the desired
#' number of draws.
#'
#' @returns An rvec_dbl
#'
#' @noRd
as_ptype_rvec_dbl <- function(x) {
    n_draw <- n_draw(x)
    m <- matrix(double(),
                nrow = 0L,
                ncol = n_draw)
    new_rvec_dbl(m)
}


## HAS_TESTS
#' Create a rvec_int prototype
#'
#' @param x An rvec with the desired
#' number of draws.
#'
#' @returns An rvec_int
#'
#' @noRd
as_ptype_rvec_int <- function(x) {
    n_draw <- n_draw(x)
    m <- matrix(integer(),
                nrow = 0L,
                ncol = n_draw)
    new_rvec_int(m)
}


## HAS_TESTS
#' Get a named length-1 vector giving the
#' location of the draw variable,
#'
#' Assume that 'draw' has already been
#' verified to be a string.
#' 
#' @param draw Name of the draw variable. A string.
#' @param data A data frame
#'
#' @returns A named integer vector of length 1.
#'
#' @noRd
get_colnum_draw <- function(draw, data) {
    nms <- names(data)
    ans <- match(draw, nms, nomatch = 0L)
    if (ans == 0L)
        cli::cli_abort(c("Variable specified by {.arg draw} not found in {.arg data}.",
                         i = "{.arg draw}: {.val {draw}}.",
                         i = "Variables in {.arg data}: {nms}"))
    names(ans) <- draw
    ans
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
#' Get a named vector of column indices
#' for columns with rvecs
#' 
#' @param data A data frame.
#'
#' @returns A named integer vector.
#'
#' @noRd
get_colnums_rvec <- function(data)
    which(vapply(data, is_rvec, TRUE))


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
           c = rvec_chr,
           d = rvec_dbl,
           i = rvec_int,
           l = rvec_lgl,
           "?" = rvec,
           cli::cli_abort("Internal error: {.val {code}} is not a valid code."))
}


## HAS_TESTS
#' Given a string specifying types, return a list
#' of rvec constructor functions
#'
#' @param type A string
#' @param colnums_values Named integer vector
#' giving locations of values variables.
#'
#' @returns A list of functions.
#'
#' @noRd
get_rvec_funs <- function(type, colnums_values) {
    if (is.null(type)) {
        n <- length(colnums_values)
        ans <- rep(list(rvec), times = n)
    }
    else {
        n <- nchar(type)
        ans <- vector(mode = "list", length = n)
        for (i in seq_len(n)) {
            code <- substr(type, start = i, stop = i)
            ans[[i]] <- get_rvec_fun(code)
        }
    }
    ans
}    


## HAS_TESTS
#' Test whether an object is an rvec
#'
#' Test whether `x` inherits from
#' class `"rvec"`.
#'
#' @param x An object.
#'
#' @returns `TRUE` or `FALSE`.
#'
#' @seealso
#' - [rvec()] to create an rvec
#' - [as.matrix()], [as_list_col()],
#'   to convert an rvec into other formats
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
matrix_to_list_of_cols <- function(m) {
    if (ncol(m) > 0L) {
        ans <-  apply(m,
                      MARGIN = 2L,
                      FUN = function(y) y,
                      simplify = FALSE)
        names(ans) <- colnames(m)
    }
    else
        ans <- list()
    ans
}


## HAS_TESTS
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
matrix_to_list_of_rows <- function(m) {
    if (nrow(m) > 0L) {
        ans <- apply(m,
                     MARGIN = 1L,
                     FUN = function(y) y,
                     simplify = FALSE)
        names(ans) <- rownames(m)
    }
    else
        ans <- list()
    ans
}


#' Given a data frame containing rvecs,
#' calculate 'n_draw'
#'
#' If 'n_draw' is the same across all rvecs,
#' return it. Otherwise raise an error.
#'
#' @param df A data 
#' the
n_draw_df <- function(df) {
    is_rvec <- vapply(df, is_rvec, TRUE)
    if (!any(is_rvec))
        cli::cli_abort("Internal error: {.arg df} does not contain any rvecs.")
    n_draw <- vapply(df[is_rvec], n_draw, 1L)
    if (length(n_draw) > 1L) {
        n_draw_1 <- n_draw[[1L]]
        is_equal <- n_draw == n_draw_1
        i_not_equal <- match(FALSE, is_equal, nomatch = 0L)
        if (i_not_equal > 0L) {
            nms <- names(n_draw)
            nm_1 <- nms[[1L]]
            nm_not <- nms[[i_not_equal]]
            n_draw_not <- n_draw[[i_not_equal]]
            cli::cli_abort("Internal error: Value for {.arg n_draw} varies across rvecs.")
        }
        n_draw <- n_draw_1
    }
    n_draw
}


## HAS_TESTS
#' Make 'n' to use in random variate functions
#'
#' The elements of 'args' should all of the same
#' length. None, some, or could be rvecs.
#' If any are rvecs, then 'n' is multiplied
#' by 'n_draw'.
#' 
#' @param n The value of 'n' supplied by the user
#' @param args A list of arguments.
#'
#' @returns An integer.
#'
#' @noRd
n_rdist <- function(n, args) {
    ans <- n
    for (arg in args) {
        if (is_rvec(arg)) {
            ans <- n_draw(arg) * ans
            break
        }
    }
    ans
}    


#' Paste together the columns from a data frame,
#' separated by a dot
#'
#' @param df A data frame.
#'
#' @returns A string with length equal
#' to nrow(df)
#'
#' @noRd
paste_dot <- function(df)
    do.call(function(...) paste(..., sep = "."), args = df)


## HAS_TESTS
#' Recycle two arguments to same length, using base R rules
#'
#' @param arg1,arg2 Vectors
#'
#' @return List with recycled versions of
#' arg1, arg2
#'
#' @noRd
recycle_common_2 <- function(arg1, arg2) {
    nm1 <- rlang::as_name(rlang::enquo(arg1))
    nm2 <- rlang::as_name(rlang::enquo(arg2))
    n1 <- length(arg1)
    n2 <- length(arg2)
    if (n1 == 0L)
        cli::cli_abort("{.arg {nm1}} has length 0")
    if (n2 == 0L)
        cli::cli_abort("{.arg {nm2}} has length 0")
    n <- max(n1, n2)
    arg1 <- rep_len(arg1, length.out = n)
    arg2 <- rep_len(arg2, length.out = n)
    list(arg1, arg2)
}


## HAS_TESTS
#' Recycle three arguments to same length, using base R rules
#'
#' @param arg1,arg2,arg3 Vectors
#'
#' @return List with recycled versions of
#' arg1, arg2, arg3
#'
#' @noRd
recycle_common_3 <- function(arg1, arg2, arg3) {
    nm1 <- rlang::as_name(rlang::enquo(arg1))
    nm2 <- rlang::as_name(rlang::enquo(arg2))
    nm3 <- rlang::as_name(rlang::enquo(arg3))
    n1 <- length(arg1)
    n2 <- length(arg2)
    n3 <- length(arg3)
    if (n1 == 0L)
        cli::cli_abort("{.arg {nm1}} has length 0")
    if (n2 == 0L)
        cli::cli_abort("{.arg {nm2}} has length 0")
    if (n3 == 0L)
        cli::cli_abort("{.arg {nm3}} has length 0")
    n <- max(n1, n2, n3)
    arg1 <- rep_len(arg1, length.out = n)
    arg2 <- rep_len(arg2, length.out = n)
    arg3 <- rep_len(arg3, length.out = n)
    list(arg1, arg2, arg3)
}


## HAS_TESTS
#' Set cols of a data frame to list columns of NULLs
#'
#' @param df A data frame
#' @param colnums Indices of columns to set to blank
#'
#' @returns Modified version of 'df'
#'
#' @noRd
set_cols_to_blank <- function(df, colnums) {
    n <- nrow(df)
    blank_col <- rep(list(NULL), times = n)
    for (colnum in colnums) {
        df[[colnum]] <- blank_col
    }
    df
}
