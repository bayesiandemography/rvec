
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
#' a 'draw' variable that distinguishes different
#' draws within the same combination
#' of ID variables. In rvec format,
#' each row represents one
#' combination of ID variables, and
#' multiple draws are stored in an [rvec][rvec()].
#' See below for examples.
#'
#' @section Data frame variables:
#'
#' `collapse_to_rvec()` and `expand_from_rvec()`
#' operate with three kinds of variables:
#' - Values variables, holding hold measurements
#'   of some random or uncertain quantity.
#'   In `collapse_to_rvec()`,
#'   values variables are specified by the
#'   `values` argument. `expand_from_rvec()`
#'   treats all rvecs as value variables.
#' - The draw variable, used to uniquely identify
#'   each draw in a database format, within
#'   each combination of ID values. Specified
#'   by the `draw` argument.
#' - ID variables, which hold covariates or
#'   classification variables.
#'       - If `data` is an ordinary data frame, then
#'         `collapse_to_rvec()` and `expand_from_rvec()`
#'         assume that all variables that are not
#'         values or draw variables are ID variables.
#'       - If `data` is a
#'         [grouped](https://dplyr.tidyverse.org/reference/group_data.html)
#'         data frame, then `collapse_to_rvec()`
#'         and `expand_from_rvec()` assume that the
#'         grouping variables are ID variables.
#'
#' @section `type` argument:
#'
#' By default, `collapse_to_rvec()` calls function
#' [rvec()] on each values variable in `data`.
#' [rvec()] chooses between `rvec_chr, `rvec_dbl`,
#' `rvec_int`,and  `rvec_lgl`, based
#' on the contents of each values variable.
#'
#' Instead of leaving the choice of class to [rvec()],
#' it can be specified in advance, using the `type` argument.
#' `type` is a string, each character of which
#' specifies the class of the corresponding values variable.
#' The characters have the following meanings:
#' - `"c"`: `rvec_chr`
#' - `"d"`: `rvec_dbl`
#' - `"i"`: `rvec_int`
#' - `"l"`: `rvec_lgl`
#' - `"?"`: Depends on inputs.
#'
#' These codes are modified from ones used by the
#' [readr](https://readr.tidyverse.org) package.
#'
#' @param data A data frame, possibly
#' [grouped](https://dplyr.tidyverse.org/reference/group_data.html).
#' @param draw The variable that uniquely identifies
#' random draws within each combination of
#' values for the ID variables. A string.
#' @param values <[`tidyselect`][tidyselect::language]>
#' One or more variables of `data` that hold measurements.
#' @param type Code specifying the class of rvec
#' to use for each value, combined into a string.
#' Optional.
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
#' data_rv <- data_db %>%
#'   collapse_to_rvec(draw = "sim",
#'                    values = pay)
#' data_rv
#'
#' ## rvec format to database format
#' data_rv %>%
#'   expand_from_rvec()
#'
#' ## provide a name for the draw variable
#' data_rv %>%
#'   expand_from_rvec(draw = "sim")
#' 
#' ## specify that rvec variable
#' ## must be rvec_int
#' data_rv <- data_db %>%
#'   collapse_to_rvec(draw = "sim",
#'                    values = pay,
#'                    type = "i")
#' 
#' ## if we add add a redundant variable,
#' ## then our earlier call no longer works
#' data_db2 <- data_db %>%
#'   mutate(newcol = "pointless")
#' try(
#'   data_db2 %>%
#'     collapse_to_rvec(draw = "sim",
#'                      values = pay)
#' )
#'
#' ## one solution: use 'group_by' to
#' ## specify the ID variables
#' data_db2 %>%
#'   group_by(occupation) %>%
#'   collapse_to_rvec(draw = "sim",
#'                    values = pay)
#' @export
collapse_to_rvec <- function(data,
                             draw = "draw",
                             values = value,
                             type = NULL) {
    UseMethod("collapse_to_rvec")
}

## HAS_TESTS
#' @rdname collapse_to_rvec
#' @export
collapse_to_rvec.data.frame <- function(data,
                                        draw = "draw",
                                        values = value,
                                        type = NULL) {
    values_quo <- rlang::enquo(values)
    colnums_values <- tidyselect::eval_select(values_quo, data = data)
    colnums_groups <- integer()
    collapse_to_rvec_inner(data = data,
                           draw = draw,
                           colnums_values = colnums_values,
                           colnums_groups = colnums_groups,
                           type = type)
}

## HAS_TESTS
#' @rdname collapse_to_rvec
#' @export
collapse_to_rvec.grouped_df <- function(data,
                                        draw = "draw",
                                        values = value,
                                        type = NULL) {
    values_quo <- rlang::enquo(values)
    colnums_values <- tidyselect::eval_select(values_quo, data = data)
    colnums_groups <- get_colnums_groups(data)
    collapse_to_rvec_inner(data = data,
                           draw = draw,
                           colnums_values = colnums_values,
                           colnums_groups = colnums_groups,
                           type = type)
}

#' @rdname collapse_to_rvec
#' @export
expand_from_rvec <- function(data, draw = "draw") {
    UseMethod("expand_from_rvec")
}

## HAS_TESTS
#' @rdname collapse_to_rvec
#' @export
expand_from_rvec.data.frame <- function(data, draw = "draw") {
    colnums_values <- get_colnums_rvec(data)
    expand_from_rvec_inner(data = data,
                           draw = draw,
                           colnums_values = colnums_values)
}

## HAS_TESTS
#' @rdname collapse_to_rvec
#' @export
expand_from_rvec.grouped_df <- function(data, draw = "draw") {
    colnums_values <- get_colnums_rvec(data)
    expand_from_rvec_inner(data = data,
                           draw = draw,
                           colnums_values = colnums_values)
}


## Helper functions -----------------------------------------------------------

## HAS_TESTS
#' Function that does the actual work for 'collapse_to_rvec'
#'
#' We check inputs here, rather than in the methods,
#' to minimise repetition.
#'
#' @param data A data frame
#' @param draw String. The name of the draw column.
#' @param colnums_values A named integer vector
#' giving the locations of rvec columns.
#' @param colnums_groups A named integer vector
#' giving the locations of any grouping columns.
#'
#' @returns A modified version of 'data'
#' (with same class and other attributes)
#'
#' @noRd
collapse_to_rvec_inner <- function(data,
                                   draw,
                                   colnums_values,
                                   colnums_groups,
                                   type) {
    ## check and process inputs
    check_str(draw, x_arg = "draw")
    colnum_draw <- get_colnum_draw(draw = draw, data = data)
    check_colnums_values(colnums_values)
    check_overlap_draw_values(colnum_draw = colnum_draw,
                              colnums_values = colnums_values)
    check_overlap_draw_groups(colnum_draw = colnum_draw,
                              colnums_groups = colnums_groups)
    check_overlap_values_groups(colnums_values = colnums_values,
                                colnums_groups = colnums_groups)
    check_has_no_rvecs(data, nm_df = "data")
    check_type(type)
    check_values_type_consistent(colnums_values = colnums_values,
                                 type = type)
    colnums_all <- get_colnums_all(data)
    has_groups <- length(colnums_groups) > 0L
    if (has_groups) {
        colnums_id <- colnums_groups
        colnums_delete <- vec_set_difference(colnums_all,
                                             c(colnum_draw,
                                               colnums_values,
                                               colnums_id))
    }
    else {
        colnums_id <- vec_set_difference(colnums_all,
                                         c(colnum_draw,
                                           colnums_values))
        colnums_delete <- integer()
    }
    ## start calculations
    rvec_funs <- get_rvec_funs(type = type,
                               colnums_values = colnums_values)
    if (nrow(data) > 0L) {
        colnums_blank <- c(colnum_draw, colnums_values, colnums_delete)
        ans <- set_cols_to_blank(data, colnums = colnums_blank)
        ans <- unique(ans)
        ## derive and check indices for contents of rvecs
        key_id_data <- paste_dot(data[colnums_id])
        key_id_ans <- paste_dot(ans[colnums_id])
        draw_data <- data[[colnum_draw]]
        draw_ans <- sort(unique(draw_data))
        idx <- cbind(match(key_id_data, key_id_ans),
                     match(draw_data, draw_ans))
        check_idx_dup(idx = idx,
                      data = data,
                      colnum_draw = colnum_draw,
                      colnums_id = colnums_id)
        nm_draw <- names(colnum_draw)
        idvars_ans <- ans[colnums_id]
        check_idx_gap(idx = idx,
                      idvars_ans = idvars_ans,
                      draw_ans = draw_ans,
                      nm_draw = nm_draw)
        ## make rvec objects
        m_tmp <- matrix(nrow = nrow(ans), ncol = length(draw_ans))
        for (i in seq_along(colnums_values)) {
            colnum <- colnums_values[[i]]
            m_tmp[idx] <- data[[colnum]]
            rvec_fun <- rvec_funs[[i]]
            ans[[colnum]] <- rvec_fun(m_tmp)
        }
    }
    else {
        ans <- data
        for (i in seq_along(colnums_values)) {
            colnum <- colnums_values[[i]]
            rvec_fun <- rvec_funs[[i]]
            m <- matrix(data[[colnum]], nrow = 0L, ncol = 1L)
            ans[[colnum]] <- rvec_fun(m)
        }
    }
    ans <- ans[-c(colnum_draw, colnums_delete)]
    rownames(ans) <- NULL
    ans
}


## HAS_TESTS
#' Function that does the actual work for 'expand_from_rvec'
#'
#' @param data A data frame
#' @param draw Name of the new draw
#' column being created. A string.
#' @param colnums_values A named integer vector
#' giving the locations of rvec columns
#'
#' @returns A modified version of 'data'
#' (with same class and other attributes)
#'
#' @noRd
expand_from_rvec_inner <- function(data,
                                   draw,
                                   colnums_values) {
    check_str(draw, x_arg = "draw")
    if (draw %in% names(data))
        cli::cli_abort(c("Name clash with {.arg draw}.",
                         i = "{.arg data} already has a column called {.val {draw}}."))
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
    val_draw <- vec_rep(seq_len(n_draw), times = nrow(data))
    after <- colnums_values[[1L]] - 1L
    ans <- append_col(ans,
                      value = val_draw,
                      after = after,
                      nm = draw)
    ans
}
        
        

        
    
    
