
## User-visible functions -----------------------------------------------------

#' Convert a data frame between 'database'
#' and 'rvec' formats
#'
#' `collapse_to_rvec()` converts a data frame from
#' a 'database' format to an 'rvec' format.
#' `expand_from_rvec()` converts a data frame
#' from an rvecs format to a database format.
#'
#' In database format, each row represents
#' one random draw. The data frame contains
#' a 'draw' column that distinguishes different
#' draws within the same combination
#' of ID variables. In rvec format,
#' each row represents one
#' combination of ID variables, and
#' multiple draws are stored in an [rvec][rvec()].
#' See below for examples.
#'
#' @section Data frame columns:
#'
#' `collapse_to_rvec()` and `expand_from_rvec()`
#' operate with three kinds of columns:
#' - Values columns, holding hold measurements
#'   of some random or uncertain quantity.
#'   In `collapse_to_rvec()`,
#'   values columns are specified by the
#'   `values` argument. `expand_from_rvec()`
#'   treats all rvecs as value columns.
#' - The draw column, used to uniquely identify
#'   each draw in a database format, within
#'   each combination of ID values. Specified
#'   by the `draw` argument.
#' - ID columns, which hold covariates or
#'   classification variables.
#'       - In an ordinary data frame,
#'         `collapse_to_rvec()` and `expand_from_rvec()`
#'         assume that all columns that are not
#'         values or draw columns are ID columns.
#'       - In a
#'         [grouped](https://dplyr.tidyverse.org/reference/group_data.html)
#'         data frame, `collapse_to_rvec()` and `expand_from_rvec()`
#'         assume that all *grouping* columns
#'         that are not values or draw columns are
#'         ID columns.
#'
#' @section `type` argument:
#'
#' By default, `collapse_to_rvec()` calls function
#' [rvec()] on each values column in `data`.
#' [rvec()] chooses between `rvec_chr, `rvec_dbl`,
#' `rvec_int`,and  `rvec_lgl`, based
#' on the contents of each values column.
#'
#' Instead of leaving the decision to [rvec()],
#' the choice of rvec class can be specified
#' in advance, using the `type` argument.
#' `type` is a string, with the number of
#' characters equal to the number of values columns.
#' The characters have the following interpretations:
#' - `"c"`: `rvec_chr`
#' - `"d"`: `rvec_dbl`
#' - `"i"`: `rvec_int`
#' - `"l"`: `rvec_lgl`
#' - `"?"`: Depends on inputs.
#'
#' These codes are modified from codes used by the
#' [readr](https://readr.tidyverse.org) package.
#'
#' @param data A data frame, possibly
#' [grouped](https://dplyr.tidyverse.org/reference/group_data.html).
#' @param draw <[`tidyselect`][tidyselect::language]>
#' The column that uniquely identifies random draws
#' within each combination of ID variables.
#' @param values <[`tidyselect`][tidyselect::language]>
#' One or more columns of `data` that hold measurements.
#' @param type A string, each letter of which
#' represents an rvec constructor function. Optional.
#'
#' @returns A data frame.
#' - `collapse_to_rvec()` **reduces** the number of rows
#'    by a factor of [n_draw()].
#' - `expand_from_rvec()` **increases** the number of rows
#'    by a factor of [n_draw()].
#' - If `data` is a
#'   [grouped](https://dplyr.tidyverse.org/reference/group_data.html)
#'   data frame, then `collapse_to_rvec()` and
#'   `expand_from_rvec()` silently drop all columns
#'   that are not specified by `draw` or `value`
#'   and that are not grouping columns.
#'
#' @seealso
#' - [rvec()] to construct a single `rvec`.
#' - [as_list_col()] to convert an `rvec`
#'   to a list column.
#' - [dplyr::group_vars()](https://dplyr.tidyverse.org/reference/group_data.html)
#'   gives the names of the grouping variables
#'   in a grouped data frame.
#'
#' `collapse_to_rvec()` and `expand_from_rvec()`
#' are reminiscent of
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
#' data_db <- tibble::tribble(
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
#' data_rv <- data_db %>%
#'   collapse_to_rvecs(draw = sim,
#'                     values = pay)
#' data_rv
#'
#' ## rvec format to database format
#' data_rv %>%
#'   expand_from_rvecs()
#'
#' ## provide a name for the draw columnnn
#' data_rv %>%
#'   expand_from_rvecs(draw = "sim")
#' 
#' ## specify that rvec column
#' ## must be rvec_int
#' data_rv <- data_db %>%
#'   collapse_to_rvecs(draw = sim,
#'                     values = pay,
#'                     type = "i")
#' 
#' ## if we add add a redundant column.
#' ## then our earlier call no longer works
#' data_db2 <- data_db %>%
#'   mutate(newcol = "pointless")
#' try(
#'   data_db2 %>%
#'     collapse_to_rvec(draw = sim,
#'                      values = pay)
#' )
#'
#' ## one solution: use 'group_by' to
#' ## specify the ID columns
#' data_db2 %>%
#'   group_by(occupation) %>%
#'   collapse_to_rvec(draw = sim,
#'                    values = pay)
#' @export
collapse_to_rvec <- function(data,
                             draw = draw,
                             values = value,
                             type = NULL) {
    UseMethod("collapse_to_rvec")
}

#' @rdname collapse_to_rvec
#' @export
collapse_to_rvec.data.frame <- function(data,
                                        draw = draw,
                                        values = value,
                                        type = NULL) {
    draw_quo <- rlang::enquo(draw)
    values_quo <- rlang::enquo(values)
    colnum_draw <- tidyselect::eval_select(draw_quo, data = data)
    colnums_values <- tidyselect::eval_select(values_quo, data = data)
    colnums_all <- get_colnums_all(data)
    colnums_id <- vec_set_difference(colnums_all,
                                     c(colnum_draw, colnums_values))
    colnums_groups <- integer()
    collapse_to_rvec_inner(data = data,
                           colnum_draw = colnum_draw,
                           colnums_values = colnums_values,
                           colnums_groups = integer(),
                           colnums_id = colnums_groups,
                           type = type)
}

#' @rdname collapse_to_rvec
#' @export
collapse_to_rvec.grouped_df <- function(data,
                                        draw = draw,
                                        values = value,
                                        type = NULL) {
    draw_quo <- rlang::enquo(draw)
    values_quo <- rlang::enquo(values)
    colnum_draw <- tidyselect::eval_select(draw_quo, data = data)
    colnums_values <- tidyselect::eval_select(values_quo, data = data)
    colnums_groups <- get_colnums_groups(data)
    colnums_id <- vec_set_difference(colnums_groups,
                                     c(colnum_draw, colnums_values))
    collapse_to_rvec_inner(data = data,
                           colnum_data = colnum_data,
                           colnums_values = colnums_values,
                           colnums_groups = colnums_groups,
                           colnums_id = colnums_id,
                           type = type)
}

#' @rdname collapse_to_rvec
#' @export
expand_from_rvec <- function(data, draw = draw) {
    UseMethod("expand_from_rvec")
}

#' @rdname collapse_to_rvec
#' @export
expand_from_rvec.data.frame <- function(data, draw = draw) {
    draw_quo <- rlang::enquo(draw)
    nm_draw <- rlang::as_name(draw_quo)
    colnum_values <- vapply(data, is_rvec, TRUE)
    colnums_all <- get_colnums_all(data)
    colnums_id <- vec_set_difference(colnums_all, colnums_values)
    colnums_groups = integer()
    expand_from_rvec_inner(data = data,
                           nm_draw = nm_draw,
                           colnums_values = colnums_values,
                           colnums_id = colnums_id,
                           colnums_groups = colnums_groups)
}

#' @rdname collapse_to_rvec
#' @export
expand_from_rvec.grouped_df <- function(data, draw = draw) {
    draw_quo <- rlang::enquo(draw)
    nm_draw <- rlang::as_name(draw_quo)
    colnum_values <- vapply(data, is_rvec, TRUE)
    colnums_groups <- get_colnums_groups(data)
    colnums_id <- vec_set_difference(colnums_groups, colnums_values)
    expand_from_rvec_inner(data = data,
                           nm_draw = nm_draw,
                           colnums_values = colnums_values,
                           colnums_id = colnums_id,
                           colnums_groups = colnums_groups)
}


## Helper functions -----------------------------------------------------------

## need to make sure that groups retained
collapse_to_rvec_inner <- function(data,
                                   colnum_draw,
                                   colnums_values,
                                   colnums_id,
                                   colnums_groups,
                                   type) {
    ## check and process inputs
    if (length(colnum_draw) == 0L)
        cli::cli_abort("No {.var draw} column selected.")
    if (length(colnum_draw) > 1L)
        cli::cli_abort("Attempt to select more than one {.var draw} column.")
    if (length(colnums_values) == 0L)
        cli::cli_abort("No {.var values} columns selected.")
    if (colnum_draw %in% colnums_values) {
        nm_draw <- names(colum_draw)
        cli::cli_abort("{.var {nm_draw}} used in {.arg draw} and in {.arg values}")
    }
    check_is_not_rvec(data)
    check_type(type)
    n_values <- length(colnums_values)
    if (!identical(nchar(type), n_values))
        cli::cli_abort(c(paste("Number of characters in {.arg type} must equal",
                               "number of columns selected by {.arg values}."),
                         i = "{.arg type} has {nchar(type)} characters.",
                         i = "{.arg values} selects {n_values} columns."))
    rvec_funs <- get_rvec_funs(type)
    if (nrow(data) > 0L) {
        colnum_blank <- c(colnum_draw, colnums_values)
        ans <- set_cols_to_blank(data, colnums = colnum_blank)
        ans <- unique(ans)
        ## derive and check indices for contents of rvecs
        key_id_data <- paste_cols(data[colnums_id])
        key_id_ans <- paste_cols(ans[colnums_id])
        draw_data <- data[[colnum_draw]]
        draw_ans <- sort(unique(draw_data))
        idx <- cbind(match(key_id_data, key_id_ans),
                     match(draw_data, draw_ans))
        check_idx_dup(idx = idx,
                      data = data,
                      colnum_draw = colnum_draw,
                      colnums_id = colnums_id)
        nm_draw <- name(colnum_draw)
        check_idx_gap(idx = idx,
                      idvars_ans = idvars_ans,
                      draw_ans = draw_ans,
                      nm_draw = nm_draw)
        ## make rvec objects
        m_tmp <- matrix(nrow = nrow(ans), ncol = length(draw_ans))
        for (colnum in colnums_values) {
            m_tmp[idx] <- data[[colnum]]
            rvec_fun <- rvec_funs[[colnum]]
            ans[[colnum]] <- rvec_fun(m_tmp)
        }
    }
    else {
        for (colnum in colnums_values) {
            rvec_fun <- rvec_funs[[colnum]]
            ans[[colnum]] <- rvec_fun()
        }
    }
    ans
}
    

## need to make sure that groups retained
expand_from_rvecs <- function(data,
                              nm_draw,
                              colnums_values) {
    nm_draw <- check_nm_draw(nm_draw)
    if (nm_draw %in% names(df))
        cli::cli_abort(paste("{.arg draw} is {.val {nm_data}} but {.arg data} already",
                             "has a column called {.val {nm_data}}"))
    if (length(colnums_values) == 0L)
        cli::cli_abort("{.arg data} does not have any rvecs")
    ans <- set_cols_to_blank(data, colnums = colnums_values)
    n_draw <- n_draw_df(data)
    ans <- vec_rep_each(ans, times = n_draw)
    for (colnum in colnums_values) {
        var <- data[[colnum]]
        m <- field(var, "data")
        ans[[colnum]] <- as.vector(t(m))
    }
    s <- seq_len(nrow(data))
    draw <- vec_rep_each(s, times = n_draw)
    after <- colnums_values[[1L]] - 1L
    ans <- append_col(ans,
                      value = draw,
                      after = after,
                      nm = nm_draw)
    ans
}
        
        

        
    
    
