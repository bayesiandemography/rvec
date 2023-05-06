
check_lengths_equal <- function(x) {
    if (length(x) > 1L) {
        lengths <- lengths(x)
        length_1 <- lengths[[1L]]
        is_equal <- lengths == length_1
        i_unequal <- match(FALSE, is_equal, nomatch = 0L)
        if (i_unequal > 0L) {
            length_unequal <- lengths[[i_unequal]]
            cli::cli_abort(c("All elements of {.var x} must have the same length.",
                             "i" = "Element 1 has length {length_1}.",
                             "i" = "Element {i_unequal} has length {length_unequal}."))
        }
    }
    invisible(TRUE)
}



check_to_rvecs_idx_dup <- function(idx,
                                   data,
                                   colnum_draw,
                                   colnums_id) {
    is_dup_idx <- duplicated(idx)
    i_dup_idx <- match(TRUE, is_dup_idx, nomatch = 0L)
    if (i_dup_idx > 0L) {
    colnums <- c(colnums_id, colnum_draw)
    nms <- names(colnums)
    msg_vals <- sprintf("{.var %s}: {.val {%s}}", nms, nms)
    msg_vals <- rlang::set_names(msg_vals, " ")
    .envir <- data[i_dup_idx, colnums, drop = FALSE]
    .envir <- as.environment(.envir)
    cli::cli_abort(c("Multiple rows with the following values:",
                     msg_vals),
                   .envir = .envir)
    }
    invisible(TRUE)
}

check_to_rvecs_idx_gap <- function(idx,
                                   idvars_ans,
                                   draw_ans) {
    nrow <- nrow(idvars_ans)
    ncol <- length(draw_ans)
    x <- matrix(NA, nrow = nrow, ncol = ncol)
    x[idx] <- 0L
    pos_na <- match(NA, x)
    if (pos_na > 0L) {
        ind_na <- arrayInd(pos_na, .dim = dim(x))
        vals_id <- idvars_ans[ind[[1L]], ]
        val_draw <- draws_ans[[ind[[2L]]]]
        ## complete
    }
    invisible(TRUE)
}


check_to_rvecs_types <- function(types, colnames_values) {
    choices <- c("c", "d", "i", "l", "?")
    n_values <- length(i_values)
    if (!is.character(types))
        cli::cli_abort(c("{.arg types} must have class {.cls character}.",
                         "x" = "{.arg types} has class {.cls {class(types)}}."))
    if (length(types) != 1L)
        cli::cli_abort(c("{.arg types} must have length 1",
                         "x" = "{.arg types} has length {length(types)}."))
    n_types <- nchar(types)
    if (n_types != n_values)
        cli::cli_abort(c("{.code nchar(types)} must equal {.code length(values)}.",
                         "i" = "{.code nchar(types)} equals {nchar(types)}",
                         "i" = "{.code length(values)} equals {n_values}"))
    code <- vapply(seq_len(n_types), function(i) substr(types, i, i), " ")
    is_valid <- code %in% choices
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        cli::cli_abort(c("{.val {code[[i_invalid]]}} is not a valid code for {.arg types}",
                         "i" = "Valid codes: {.val {choices}}"))
    invisible(TRUE)
}

