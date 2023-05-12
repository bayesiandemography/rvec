
## HAS_TESTS
#' Check widths argument, and return a
#' strictly decreasing version
#'
#' @param width A positive-length numeric vector
#' with values between 0 and 1 (inclusive).
#'
#' @return A vector of strictly decreasing values.
#'
#' @noRd
check_and_tidy_width <- function(width) {
    if (!is.numeric(width))
        cli::cli_abort("{.arg width} has class {.class {class(width)}}")
    if (identical(length(width), 0L))
        cli::cli_abort("{.arg width} has length 0")
    if (anyNA(width))
        cli::cli_abort("{.arg width} has {.val {NA}}s")
    if (any(width < 0))
        cli::cli_abort("{.arg width} has negative values")
    if (any(width > 1))
        cli::cli_abort("{.arg width} has values greater than 1")
    sort(unique(width), decreasing = TRUE)
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
#' @param nm_draw The name of the 'draw' column.
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
        cli::cli_abort(c("Missing combination of values for ID and draw columns:",
                         msg),
                       .envir = .envir)
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
                         "i" = glue::glue("`{x_arg}` has length {length}",
                                          x_arg = x_arg,
                                          length = length),
                         "i" = glue::glue("`{y_arg}` has {n_draw} draws",
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
                     "i" = glue::glue("`{x_arg}` has {n_x} draws",
                                      x_arg = x_arg,
                                      n_x = n_x),
                     "i" = glue::glue("`{y_arg}` has {n_y} draws",
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
#' Check that 'na_rm' is a logial flag
#'
#' @param na_rm TRUE or FALSE
#'
#' @returns TRUE, invisibly
check_na_rm <- function(na_rm) {
    if (!identical(length(na_rm), 1L))
        cli::cli_abort("{.arg na_rm} does not have length 1")
    if (!is.logical(na_rm))
        cli::cli_abort("{.arg na_rm} has class {.cls {class(na_rm)}}")
    if (is.na(na_rm))
        cli::cli_abort("{.arg na_rm} is {.val {NA}}")
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
check_types <- function(types) {
    choices <- c("c", "d", "i", "l", "?")
    if (!is.character(types))
        cli::cli_abort(c("{.arg types} must have class {.cls character}.",
                         "x" = "{.arg types} has class {.cls {class(types)}}."))
    if (length(types) != 1L)
        cli::cli_abort(c("{.arg types} must be a single string",
                         "x" = "{.arg types} has length {length(types)}."))
    n_char <- nchar(types)
    get_char <- function(i) substr(types, i, i)
    code <- vapply(seq_len(n_char), get_char, " ")
    is_valid <- code %in% choices
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        cli::cli_abort(c("{.val {code[[i_invalid]]}} is not a valid code for {.arg types}",
                         "i" = "Valid codes: {.val {choices}}"))
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

