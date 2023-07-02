
## HAS_TESTS
#' Check that 'colnum_draw' has length 1
#'
#' @param colnum_draw A named integer vector of length 1
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_colnum_draw <- function(colnum_draw) {
    if (length(colnum_draw) == 0L)
        cli::cli_abort("No {.var draw} variable selected.")
    if (length(colnum_draw) > 1L)
        cli::cli_abort("More than one {.var draw} variable selected.")
    invisible(TRUE)
}


## HAS_TESTS
#' Check that 'colnum_values' does not have length 0
#'
#' @param colnums_values A named integer vector.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_colnums_values <- function(colnums_values) {
    if (length(colnums_values) == 0L)
        cli::cli_abort("No {.var values} variables selected.")
    invisible(TRUE)
}


## HAS_TESTS
#' Check that 'draw' is a valid string
#'
#' @param draw Name of the 'draw' variable.
#' A string.
#'
#' @returns TRUE, invisibly.
#'
#' @noRd
check_draw <- function(draw) {
    if (!identical(length(draw), 1L))
        cli::cli_abort("{.arg draw} does not have length 1.")
    if (is.na(draw))
        cli::cli_abort("{.arg draw} is {.val {NA}}.")
    if (!is.character(draw))
        cli::cli_abort("{.arg draw} has class {.cls {class(draw)}}.")
    if (nchar(draw) == 0L)
        cli::cli_abort("{.arg draw} is blank")
    invisible(TRUE)
}


## HAS_TESTS
#' Check a logical flag
#'
#' @param x TRUE or FALSE
#'
#' @returns TRUE, invisibly
#' @noRd
check_flag <- function(x) {
    nm <- deparse1(substitute(x))
    if (!identical(length(x), 1L))
        cli::cli_abort("{.arg {nm}} does not have length 1")
    if (!is.logical(x))
        cli::cli_abort("{.arg {nm}} has class {.cls {class(x)}}")
    if (is.na(x))
        cli::cli_abort("{.arg {nm}} is {.val {NA}}")
    invisible(TRUE)
}


## HAS_TESTS
#' Check that indices for position in data
#' part of rvec have no duplicates
#'
#' @param idx Two-column matrix, where first column
#' indexes row in data part of rvec and second
#' column indexes column.
#' @param data A data frame
#' @param colnum_draw Named integer vector
#' of length 1 identifying 'draw' column
#' @param colnums_id Named integer vector
#' indentifying ID columns
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_idx_dup <- function(idx,
                          data,
                          colnum_draw,
                          colnums_id) {
    is_dup <- duplicated(idx)
    i_dup <- match(TRUE, is_dup, nomatch = 0L)
    if (i_dup > 0L) {
        colnums <- c(colnums_id, colnum_draw)
        nms <- names(colnums)
        msg_vals <- sprintf("{.var %s}: {.val {%s}}", nms, nms)
        msg_vals <- rlang::set_names(msg_vals, nm = " ")
        .envir <- data[i_dup, colnums, drop = FALSE]
        .envir <- as.environment(.envir)
        cli::cli_abort(c("Multiple rows with the same values for ID variables:",
                         msg_vals),
                       .envir = .envir)
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check that indices for position in data
#' part of rvec have no gaps
#'
#' @param idx Two-column matrix, where first column
#' indexes row in data part of rvec and second
#' column indexes column.
#' @param idvars_ans Data frame with id variables
#' used in answer.
#' @param draw_ans Vector with draws used
#' in answer.
#' @param nm_draw The name of the 'draw' variable.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_idx_gap <- function(idx,
                          idvars_ans,
                          draw_ans,
                          nm_draw) {
    nrow <- nrow(idvars_ans)
    ncol <- length(draw_ans)
    x <- matrix(NA, nrow = nrow, ncol = ncol)
    x[idx] <- 0L
    pos_na <- match(NA, x, nomatch = 0L)
    if (pos_na > 0L) {
        ind_na <- arrayInd(pos_na, .dim = c(nrow, ncol))
        i_id <- ind_na[[1L]]
        i_draw <- ind_na[[2L]]
        vals_id <- idvars_ans[i_id, , drop = FALSE]
        val_draw <- draw_ans[[i_draw]]
        nms <- c(names(idvars_ans), nm_draw)
        msg <- sprintf("{.var %s}: {.val {%s}}", nms, nms)
        msg <- rlang::set_names(msg, nm = " ")
        .envir <- vals_id
        .envir[[nm_draw]] <- val_draw
        .envir <- as.environment(.envir)
        cli::cli_abort(c("Missing combination of values for ID and draw variables:",
                         msg),
                       .envir = .envir)
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check that a data frame does not contain
#' any rvecs
#'
#' @param df A data frame
#' @param nm_df Name for df to be used in error message.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_has_no_rvecs <- function(df, nm_df) {
    is_rv <- vapply(df, is_rvec, FALSE)
    i_rv <- match(TRUE, is_rv, nomatch = 0L)
    if (i_rv > 0L) {
        nm_rv <- names(df)[[i_rv]]
        cls_rv <- class(df[[i_rv]])
        cli::cli_abort(c("{.arg {nm_df}} contains an rvec",
                         i = "{.var {nm_rv}} has class {.cls cls_rv}"))
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check that the length of a base vector equals
#' one or the number of draws of an rvec object
#' 
#' @param x A base vector
#' @param y An rvec object
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_length_n_draw_compatible <- function(x, y, x_arg, y_arg) {
    length <- length(x)
    if (length != 1L) {
        n_draw <- n_draw(y)
        if (length != n_draw) {
            message <- c(glue::glue("Length of vector `{x_arg}` must equal number ",
                                    "of draws of rvec `{y_arg}`.",
                                    x_arg = x_arg,
                                    y_arg = y_arg),
                         i = glue::glue("`{x_arg}` has length {length}",
                                          x_arg = x_arg,
                                          length = length),
                         i = glue::glue("`{y_arg}` has {n_draw} draws",
                                          y_arg = y_arg,
                                          n_draw = n_draw))
            stop_incompatible_type(x = x,
                                   y = y,
                                   x_arg = x_arg,
                                   y_arg = y_arg,
                                   message = message)
        }
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check that elements of list all have same length
#'
#' @param A list
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_lengths_equal <- function(x) {
    if (length(x) >= 2L) {
        lengths <- lengths(x)
        len1 <- lengths[[1L]]
        is_same <- lengths == len1
        i_diff <- match(FALSE, is_same, nomatch = 0L)
        if (i_diff > 0L) {
            len_diff <- lengths[[i_diff]]
            cli::cli_abort(c("Elements of {.arg x} do not have equal lengths.",
                             i = "Element 1 of {.arg x} has length {len1}.",
                             i = "Element {i_diff} of {.arg x} has length {len_diff}."))
        }
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check that all elements of a list have non-zero length
#'
#' @param A list
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_lengths_nonzero <- function(x) {
    if (length(x) > 0L) {
        lengths <- lengths(x)
        i_zero <- match(0L, lengths, nomatch = 0L)
        if (i_zero > 0L) {
            cli::cli_abort(c("All elements of {.arg x} must have non-zero length.",
                             i = "Element {i_zero} of {.arg x} has length 0."))
        }
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check that 'n' is a positive integer
#'
#' @param n
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_n <- function(n) {
    check_nonneg_num_scalar(n)
    if (round(n) != n)
        cli::cli_abort("{.arg n} has non-integer value ({n}).")
    if (n == 0L)
        cli::cli_abort("{.arg n} equals 0.")
    invisible(TRUE)
}


## HAS_TESTS
#' Check that 'n_draw' is a positive integer
#'
#' @param n_draw
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_n_draw <- function(n_draw) {
    check_nonneg_num_scalar(n_draw)
    if (round(n_draw) != n_draw)
        cli::cli_abort("{.arg n_draw} has non-integer value ({n_draw}).")
    if (n_draw == 0L)
        cli::cli_abort("{.arg n_draw} equals 0.")
    invisible(TRUE)
}


## HAS_TESTS
#' Check that two rvec objects have the same number
#' of draws
#'
#' @param x, y Objects of class "rvec"
#' @param x_arg, y_arg Names to appear in error messages
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_n_draw_equal <- function(x, y, x_arg, y_arg) {
    n_x <- n_draw(x)
    n_y <- n_draw(y)
    if (n_x != n_y) {
        message <- c(glue::glue("Number of draws of rvec `{x_arg}` must equal number ",
                                "of draws of rvec `{y_arg}`.",
                                x_arg = x_arg,
                                y_arg = y_arg),
                     i = glue::glue("`{x_arg}` has {n_x} draws",
                                      x_arg = x_arg,
                                      n_x = n_x),
                     i = glue::glue("`{y_arg}` has {n_y} draws",
                                      y_arg = y_arg,
                                      n_y = n_y))
        stop_incompatible_type(x = x,
                               y = y,
                               x_arg = x_arg,
                               y_arg = y_arg,
                               message = message)
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check a non-negative numeric scalar
#'
#' @param x A non-negative numeric scalar
#'
#' @returns TRUE, invisibly
#' @noRd
check_nonneg_num_scalar <- function(x) {
    nm <- deparse1(substitute(x))
    if (!identical(length(x), 1L))
        cli::cli_abort("{.arg {nm}} does not have length 1")
    if (!is.numeric(x))
        cli::cli_abort("{.arg {nm}} has class {.cls {class(x)}}")
    if (is.na(x))
        cli::cli_abort("{.arg {nm}} is {.val {NA}}")
    if (x < 0)
        cli::cli_abort("{.arg {nm}} is negative")
    invisible(TRUE)
}


## HAS_TESTS
#' Check a non-negative scalar
#'
#' @param x A non-negative numeric vector
#'
#' @returns TRUE, invisibly
#' @noRd
check_nonneg_num_vector <- function(x) {
    nm <- deparse1(substitute(x))
    if (!is.numeric(x))
        cli::cli_abort("{.arg {nm}} has class {.cls {class(x)}}")
    n_na <- sum(is.na(x))
    if (n_na > 0L)
        cli::cli_abort("{.arg {nm}} has {n_na} NA{?s}")
    n_neg <- sum(x < 0)
    if (n_neg > 0)
        cli::cli_abort("{.arg {nm}} has {n_neg} negative value{?s}")
    invisible(TRUE)
}


## HAS_TESTS
#' Check that an rvec is not an rvec_chr
#'
#' @param arg An rvec.
#' @param nm_arg Name to be used in error messages.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_not_rvec_chr <- function(arg, nm_arg) {
    if (inherits(arg, "rvec_chr"))
        cli::cli_abort("{.arg {nm_arg}} has class {.cls {class(arg)}}")
    invisible(TRUE)
}


## HAS_TESTS
#' Check that draw variable not used as grouping variable
#'
#' Assume that the colnum_draw and colnums_groups
#' are both valid.
#'
#' @param colnum_draw A named integer vector of
#' length 1 giving the location of the draw variable
#' @param colnums_groups A named vector of
#' length >0, giving the location(s) of the
#' grouping variable(s)
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_overlap_draw_groups <- function(colnum_draw, colnums_groups) {
    if (colnum_draw %in% colnums_groups) {
        nm <- names(colnum_draw)
        nms_gp <- names(colnums_groups)
        cli::cli_abort(c("{.var {nm}} is a grouping variable, so cannot be used for {.arg draw}.",
                         i = "grouping variables: {nms_gp}",
                         i = "{.arg draw}: {nm}"))
    }
    invisible(TRUE)
}    


## HAS_TESTS
#' Check that draw variable not used as values variable
#'
#' Assume that the colnum_draw and colnums_values
#' are both valid.
#'
#' @param colnum_draw A named integer vector of
#' length 1 giving the location of the draw variable
#' @param colnums_values A named vector of
#' length >0, giving the location(s) of the
#' values variable(s)
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_overlap_draw_values <- function(colnum_draw, colnums_values) {
    if (colnum_draw %in% colnums_values) {
        nm <- names(colnum_draw)
        nms_val <- names(colnums_values)
        cli::cli_abort(c("{.var {nm}} used in {.arg draw} and in {.arg values}.",
                         i = "{.arg draw}: {nm}",
                         i = "{.arg values}: {nms_val}"))
    }
    invisible(TRUE)
}    
                                      

## HAS_TESTS
#' Check if any values variables are also groups variables
#'
#' Assume that colnums_values and colnums_groups are both valid.
#'
#' @param colnums_values A named vector of
#' length >0, giving the location(s) of the
#' values variable(s)
#' @param colnums_groups A named vector of
#' length >0, giving the location(s) of the
#' grouping variable(s)
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_overlap_values_groups <- function(colnums_values, colnums_groups) {
    is_in_gp <- colnums_values %in% colnums_groups
    i_in_gp <- match(TRUE, is_in_gp, nomatch = 0L)
    if (i_in_gp > 0L) {
        nms_val <- names(colnums_values)
        nms_gp <- names(colnums_groups)
        nm_in <- nms_val[[i_in_gp]]
        cli::cli_abort(c("{.var {nm_in}} is a grouping variable, so cannot be included in {.arg values}",
                         i = "grouping variable{?s}: {nms_gp}",
                         i = "{.arg values}: {nms_val}"))
    }
    invisible(TRUE)
}    
    

## HAS_TESTS
#' Check probs argument
#'
#' @param probs A positive-length numeric vector
#' with values between 0 and 1 (inclusive).
#'
#' @return TRUE, invisibly.
#'
#' @noRd
check_probs <- function(probs) {
    if (!is.numeric(probs))
        cli::cli_abort(c("{.arg probs} must be numeric",
                         i = "{.arg probs} has class {.class {class(probs)}}"))
    if (identical(length(probs), 0L))
        cli::cli_abort("{.arg probs} has length 0")
    if (anyNA(probs))
        cli::cli_abort("{.arg probs} has {.val {NA}}s")
    if (any(probs < 0))
        cli::cli_abort("{.arg probs} has negative values")
    if (any(probs > 1))
        cli::cli_abort("{.arg probs} has values greater than 1")
    invisible(TRUE)
}


## HAS_TESTS
#' Check that 'x' and 'y' are the same length
#'
#' @param x,y Vectors (including rvecs)
#' @param x_arg,x_y Names for 'x' and 'y'
#' to be used in error messages.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_same_length <- function(x, y, x_arg, y_arg) {
    n_x <- length(x)
    n_y <- length(y)
    if (n_x != n_y)
        cli::cli_abort(c("{.arg {x_arg}} and {.arg {y_arg}} have different lengths.",
                         i = "{.arg {x_arg}} has length {n_x}.",
                         i = "{.arg {y_arg}} has length {n_y}."))
    invisible(TRUE)
}



## HAS_TESTS
#' Check that type codes are valid
#'
#' Check that a string with one-letter codes
#' for rvec types is valid
#'
#' @param types String.
#'
#' @returns TRUE, invisibly.
#'
#' @noRd
check_type <- function(type) {
    choices <- c("c", "d", "i", "l", "?")
    if (!is.null(type)) {
        if (!is.character(type))
            cli::cli_abort(c("{.arg type} must have class {.cls character}.",
                             "x" = "{.arg type} has class {.cls {class(type)}}."))
        if (length(type) != 1L)
            cli::cli_abort(c("{.arg type} must be a single string",
                             "x" = "{.arg type} has length {length(type)}."))
        n_char <- nchar(type)
        get_char <- function(i) substr(type, i, i)
        code <- vapply(seq_len(n_char), get_char, " ")
        is_valid <- code %in% choices
        i_invalid <- match(FALSE, is_valid, nomatch = 0L)
        if (i_invalid > 0L)
            cli::cli_abort(c("{.val {code[[i_invalid]]}} is not a valid code for {.arg type}",
                             i = "Valid codes: {.val {choices}}"))
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check that number of characters in 'type'
#' equal to length of values
#'
#' @param colnums_values A named integer vector
#' giving locations of values variables
#' @param type A string
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_values_type_consistent <- function(colnums_values, type) {
    if (!is.null(type)) {
        n_values <- length(colnums_values)
        n_type <- nchar(type)
        if (!identical(n_type, n_values))
            cli::cli_abort(c("Number of characters in {.arg type} must equal number of values variables",
                             i = "{.arg type} has {n_type} character{?s}.",
                             i = "{.arg values} has length {n_values}."))
    }
    invisible(TRUE)
}


## HAS TESTS
#' Check that 'x' has at least one column
#'
#' @param x A matrix
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_x_has_at_least_one_col <- function(x) {
    if (ncol(x) == 0L)
        cli::cli_abort("{.arg x} must have at least one column")
    invisible(TRUE)
}


## HAS_TESTS
#' Check that 'x' is a matrix
#'
#' @param An object
#'
#' @returns TRUE, invisibly.
#'
#' @noRd
check_x_is_matrix <- function(x) {
    if (!is.matrix(x))
        cli::cli_abort(c("{.arg x} must be a matrix",
                         "x" = "{.arg x} has class {.cls {class(x)}}"))
    invisible(TRUE)
}


## HAS_TESTS
#' Check that a vector has at least one element
#'
#' @param A vector
#'
#' @returns TRUE, invisibly.
#'
#' @noRd
check_x_length_at_least_one <- function(x) {
    if (length(x) == 0L)
        cli::cli_abort("{.arg x} has length 0")
    invisible(TRUE)
}

