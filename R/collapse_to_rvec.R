
## User-visible functions -----------------------------------------------------

#' Convert a Data Frame Between 'Database'
#' and 'Rvec' Formats
#'
#' `collapse_to_rvec()` converts a data frame from
#' a 'database' format to an 'rvec' format.
#' `expand_from_rvec()`, does the opposite,
#' converting a data frame from an rvecs format
#' to a database format.
#'
#' In database format, each row represents
#' one random draw. The data frame contains
#' a 'draw' variable that distinguishes different
#' draws within the same combination
#' of 'by' variables. In rvec format,
#' each row represents one
#' combination of 'by' variables, and
#' multiple draws are stored in an [rvec][rvec()].
#' See below for examples.
#'
#' @section `by` argument:
#'
#' The `by` argument is used to specify stratifying
#' variables. For instance if `by` includes `sex` and `age`,
#' then data frame produced by `collapse_to_rvec()`
#' has separate rows for each
#' combination of `sex` and `age`.
#'
#' If `data` is a
#' [grouped](https://dplyr.tidyverse.org/reference/group_data.html)
#' data frame, then the grouping variables
#' take precedence over `by`.
#'
#' If no value for `by` is provided,
#' and `data` is not a grouped data frame,
#' then `collapse_to_rvec()`
#' assumes that all variables in `data` that are
#' not included in `value`
#' and `draw` should be included in `by`.
#' 
#' @section `type` argument:
#'
#' By default, `collapse_to_rvec()` calls function
#' [rvec()] on each values variable in `data`.
#' [rvec()] chooses the class of the output (ie
#' `rvec_chr`, `rvec_dbl`, `rvec_int`, or `rvec_lgl`)
#' depending on the input. Types can instead
#' be specified in advance, using the `type` argument.
#' `type` is a string, each character of which
#' specifies the class of the corresponding values variable.
#' The characters have the following meanings:
#' - `"c"`: `rvec_chr`
#' - `"d"`: `rvec_dbl`
#' - `"i"`: `rvec_int`
#' - `"l"`: `rvec_lgl`
#' - `"?"`: Depends on inputs.
#'
#' The codes for `type` are modified from ones used by the
#' [readr](https://readr.tidyverse.org) package.
#'
#' @param data A data frame, possibly
#' [grouped](https://dplyr.tidyverse.org/reference/group_data.html).
#' @param draw <[`tidyselect`][tidyselect::language]>
#' The variable that uniquely identifies
#' random draws within each combination of
#' values for the 'by' variables. Must be quoted
#' for `expand_from_rvec()`.
#' @param values <[`tidyselect`][tidyselect::language]>
#' One or more variables of `data` that hold measurements.
#' @param by <[`tidyselect`][tidyselect::language]>
#' Variables used to stratify or cross-classify the data.
#' See Details.
#' @param type Code specifying the class of rvec
#' to use for each value, combined into a string.
#' Optional. See Details.
#'
#' @returns A data frame.
#' - `collapse_to_rvec()` **reduces** the number of rows
#'    by a factor of [n_draw()].
#' - `expand_from_rvec()` **increases** the number of rows
#'    by a factor of [n_draw()].
#' - `collapse_to_rvec()` silently drops all variables
#'   that are not draw, value or grouping variables
#'   if `data` is a
#'   [grouped](https://dplyr.tidyverse.org/reference/group_data.html)
#'   data frame.
#'
#' @seealso
#' - [rvec()] to construct a single `rvec`.
#' - [as_list_col()] to convert an `rvec`
#'   to a list variable.
#' - [dplyr::group_vars()](https://dplyr.tidyverse.org/reference/group_data.html)
#'   gives the names of the grouping variables
#'   in a grouped data frame.
#'
#' `collapse_to_rvec()` and `expand_from_rvec()`
#' are analogous to
#' [tidyr::nest()](https://tidyr.tidyverse.org/reference/nest.html)
#' and
#' [tidyr::unnest()](https://tidyr.tidyverse.org/reference/unnest.html)
#' though `collapse_to_rvec()` and
#' `expand_from_rvec()` move values into and
#' out of rvecs, while `tidyr::nest()` and
#' `tidyr::unnest()` move them in and out
#' of data frames. (`tidyr::nest()` and
#' `tidyr::unnest()` are also a lot
#' more flexible.)
#'
#' @examples
#' library(dplyr)
#' data_db <- tribble(
#'   ~occupation,    ~sim, ~pay,
#'   "Statistician", 1,    100,
#'   "Statistician", 2,    80,
#'   "Statistician", 3,    105,
#'   "Banker",       1,    400,
#'   "Banker",       2,    350,
#'   "Banker",       3,    420
#' )
#'
#' ## database format to rvec format
#' data_rv <- data_db |>
#'   collapse_to_rvec(draw = sim,
#'                    values = pay)
#' data_rv
#'
#' ## rvec format to database format
#' data_rv |>
#'   expand_from_rvec()
#'
#' ## provide a name for the draw variable
#' data_rv |>
#'   expand_from_rvec(draw = "sim")
#' 
#' ## specify that rvec variable
#' ## must be rvec_int
#' data_rv <- data_db |>
#'   collapse_to_rvec(draw = sim,
#'                    values = pay,
#'                    type = "i")
#'
#' ## specify stratifying variable explicitly,
#' ## using 'by' argument
#' data_db |>
#'   collapse_to_rvec(draw = sim,
#'                    values = pay,
#'                    by = occupation)
#'
#' ## specify stratifying variable explicitly,
#' ## using 'group_by'
#' library(dplyr)
#' data_db |>
#'   group_by(occupation) |>
#'   collapse_to_rvec(draw = sim,
#'                    values = pay)
#' @export
collapse_to_rvec <- function(data,
                             draw = draw,
                             values = value,
                             by = NULL,
                             type = NULL) {
    UseMethod("collapse_to_rvec")
}

## HAS_TESTS
#' @rdname collapse_to_rvec
#' @export
collapse_to_rvec.data.frame <- function(data,
                                        draw = draw,
                                        values = value,
                                        by = NULL,
                                        type = NULL) {
    draw_quo <- rlang::enquo(draw)
    values_quo <- rlang::enquo(values)
    by_quo <- rlang::enquo(by)
    draw_colnum <- tidyselect::eval_select(draw_quo, data = data)
    values_colnums <- tidyselect::eval_select(values_quo, data = data)
    by_colnums <- tidyselect::eval_select(by_quo, data = data)
    groups_colnums <- integer()
    collapse_to_rvec_inner(data = data,
                           draw_colnum = draw_colnum,
                           values_colnums = values_colnums,
                           by_colnums = by_colnums,
                           groups_colnums = groups_colnums,
                           type = type)
}

## HAS_TESTS
#' @rdname collapse_to_rvec
#' @export
collapse_to_rvec.grouped_df <- function(data,
                                        draw = draw,
                                        values = value,
                                        by = NULL,
                                        type = NULL) {
    draw_quo <- rlang::enquo(draw)
    values_quo <- rlang::enquo(values)
    by_quo <- rlang::enquo(by)
    draw_colnum <- tidyselect::eval_select(draw_quo, data = data)
    values_colnums <- tidyselect::eval_select(values_quo, data = data)
    by_colnums <- tidyselect::eval_select(by_quo, data = data)
    groups_colnums <- get_groups_colnums(data)
    collapse_to_rvec_inner(data = data,
                           draw_colnum = draw_colnum,
                           values_colnums = values_colnums,
                           by_colnums = by_colnums,
                           groups_colnums = groups_colnums,
                           type = type)
}

#' @rdname collapse_to_rvec
#' @export
expand_from_rvec <- function(data,
                             draw = "draw") {
    UseMethod("expand_from_rvec")
}

## HAS_TESTS
#' @rdname collapse_to_rvec
#' @export
expand_from_rvec.data.frame <- function(data,
                                        draw = "draw") {
    values_colnums <- get_rvec_colnums(data)
    expand_from_rvec_inner(data = data,
                           draw = draw,
                           values_colnums = values_colnums)
}

## HAS_TESTS
#' @rdname collapse_to_rvec
#' @export
expand_from_rvec.grouped_df <- function(data,
                                        draw = "draw") {
    values_colnums <- get_rvec_colnums(data)
    expand_from_rvec_inner(data = data,
                           draw = draw,
                           values_colnums = values_colnums)
}


## Helper functions -----------------------------------------------------------

## HAS_TESTS
#' Function that does the actual work for 'collapse_to_rvec'
#'
#' We check inputs here, rather than in the methods,
#' to minimise repetition.
#'
#' @param data A data frame
#' @param draw_colnum String. A named integer vector
#' giving the location of the draw column.
#' @param values_colnums A named integer vector
#' giving the locations of rvec columns.
#' @param by_colnums A named integer vector
#' giving the locations of any by columns.
#' @param groups_colnums A named integer vector
#' giving the locations of any grouping columns.
#'
#' @returns A modified version of 'data'
#' (with same class and other attributes)
#'
#' @noRd
collapse_to_rvec_inner <- function(data,
                                   draw_colnum,
                                   values_colnums,
                                   by_colnums,
                                   groups_colnums,
                                   type) {
    ## check and process inputs
    check_draw_colnum(draw_colnum)
    check_values_colnums(values_colnums)
    check_overlap_draw_values(draw_colnum = draw_colnum,
                              values_colnums = values_colnums)
    check_overlap_draw_by(draw_colnum = draw_colnum,
                          by_colnums = by_colnums)
    check_overlap_draw_groups(draw_colnum = draw_colnum,
                              groups_colnums = groups_colnums)
    check_overlap_values_by(values_colnums = values_colnums,
                            by_colnums = by_colnums)
    check_overlap_values_groups(values_colnums = values_colnums,
                                groups_colnums = groups_colnums)
    check_not_has_by_and_groups(by_colnums = by_colnums,
                                groups_colnums = groups_colnums)
    check_has_no_rvecs(data, nm_df = "data")
    check_type(type)
    check_values_type_consistent(values_colnums = values_colnums,
                                 type = type)
    all_colnums <- get_all_colnums(data)
    has_groups <- length(groups_colnums) > 0L
    has_by <- length(by_colnums) > 0L
    if (has_groups || has_by) {
        if (has_groups)
            by_colnums <- groups_colnums
        delete_colnums <- vec_set_difference(all_colnums,
                                             c(draw_colnum,
                                               values_colnums,
                                               by_colnums))
    }
    else {
        by_colnums <- vec_set_difference(all_colnums,
                                         c(draw_colnum,
                                           values_colnums))
        delete_colnums <- integer()
    }
    ## start calculations
    rvec_funs <- get_rvec_funs(type = type,
                               values_colnums = values_colnums)
    if (nrow(data) > 0L) {
        blank_colnums <- c(draw_colnum, values_colnums, delete_colnums)
        ans <- set_cols_to_blank(data, colnums = blank_colnums)
        ans <- unique(ans)
        ## derive and check indices for contents of rvecs
        key_id_data <- paste_dot(data[by_colnums])
        key_id_ans <- paste_dot(ans[by_colnums])
        draw_data <- data[[draw_colnum]]
        draw_ans <- sort(unique(draw_data))
        idx <- cbind(match(key_id_data, key_id_ans),
                     match(draw_data, draw_ans))
        check_idx_dup(idx = idx,
                      data = data,
                      draw_colnum = draw_colnum,
                      by_colnums = by_colnums)
        nm_draw <- names(draw_colnum)
        idvars_ans <- ans[by_colnums]
        check_idx_gap(idx = idx,
                      idvars_ans = idvars_ans,
                      draw_ans = draw_ans,
                      nm_draw = nm_draw)
        ## make rvec objects
        m_tmp <- matrix(nrow = nrow(ans), ncol = length(draw_ans))
        for (i in seq_along(values_colnums)) {
            colnum <- values_colnums[[i]]
            m_tmp[idx] <- data[[colnum]]
            rvec_fun <- rvec_funs[[i]]
            ans[[colnum]] <- rvec_fun(m_tmp)
        }
    }
    else {
        ans <- data
        for (i in seq_along(values_colnums)) {
            colnum <- values_colnums[[i]]
            rvec_fun <- rvec_funs[[i]]
            m <- matrix(data[[colnum]], nrow = 0L, ncol = 1L)
            ans[[colnum]] <- rvec_fun(m)
        }
    }
    ans <- ans[-c(draw_colnum, delete_colnums)]
    rownames(ans) <- NULL
    ans
}


## HAS_TESTS
#' Function that does the actual work for 'expand_from_rvec'
#'
#' @param data A data frame
#' @param draw Name of the new draw
#' column being created. A string.
#' @param values_colnums A named integer vector
#' giving the locations of rvec columns
#'
#' @returns A modified version of 'data'
#' (with same class and other attributes)
#'
#' @noRd
expand_from_rvec_inner <- function(data,
                                   draw,
                                   values_colnums) {
    check_str(draw, x_arg = "draw")
    if (draw %in% names(data))
        cli::cli_abort(c("Name clash with {.arg draw}.",
                         i = "{.arg data} already has a column called {.val {draw}}."))
    if (length(values_colnums) == 0L)
        cli::cli_abort("{.arg data} does not have any rvecs")
    ans <- set_cols_to_blank(data, colnums = values_colnums)
    n_draw <- n_draw_df(data)
    ans <- vec_rep_each(ans, times = n_draw)
    for (colnum in values_colnums) {
        var <- data[[colnum]]
        m <- field(var, "data")
        ans[[colnum]] <- as.vector(t(m))
    }
    val_draw <- vec_rep(seq_len(n_draw), times = nrow(data))
    after <- values_colnums[[1L]] - 1L
    ans <- append_col(ans,
                      value = val_draw,
                      after = after,
                      nm = draw)
    ans
}
        
        

        
    
    
