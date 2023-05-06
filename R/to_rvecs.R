
## User-visible functions -----------------------------------------------------

#' Convert random draws in a data frame to an 'rvec' format
#'
#' Given a data frame in which each row represents one
#' draw, create a new, smaller data frame that uses
#' [rvecs][rvec()] to represent draws.
#'
#' @section Kinds of columns in `data`:
#'
#' `to_rvecs()` recognises three kinds of columns:
#' - *values* Measurements of some random process.
#' - *ID* Variables that jointly identify
#' sets of draws.
#' - *draw* Indices distinguishing draws
#' within a single set.
#'
#' In the following dataset, for instance,
#' `pay` is a values column, `occupation`
#' is an ID column, and `sim` is a draw column.
#'
#' ```{r, include = FALSE}
#' data <- tibble::tribble(
#'   ~occupation,    ~sim, ~pay,
#'   "Statistician", 1,    100,
#'   "Statistician", 2,    80,
#'   "Statistician", 3,    105,
#'   "Banker",       1,    400,
#'   "Banker",       2,    350,
#'   "Banker",       3,    420
#' )
#' ```
#' ```{r, echo = FALSE}
#' print(data)
#' ```
#'
#' @section Using `to_rvecs` with ordinary data frames:
#'
#' When `data` is an ordinary data frame,
#' `to_rvecs` assumes that all columns in `data`
#' that are not included in the `draw` and `values`
#' arguments are ID columns.
#'
#' If `data` in fact contains columns that are
#' not draw, value, or ID columns, then these
#' extra columns should be removed before calling
#' `to_rvecs()`. See below for an example.
#'
#' @section Using `to_rvecs` with grouped data frames:
#'
#' When `data` is a [grouped][dplyr::grouped_df] data frame,
#' `to_rvecs` only assumes that a column is a ID column if
#' it is not included in the `draw` and `values` argument,
#' and if is a grouping variable for `data`.
#' (To check whether a column in `data` is a grouping variable,
#' print `data` and look at the metadata near the top,
#' or call [dplyr::group_vars()].)
#'
#' `rvecs` silently removes from `data` any columns that
#' are not included in the `draw` and `values` arguments,
#' and that are not grouping variables.
#'
#' @section Types:
#'
#' By default, `to_rvecs()` calls function
#' [rvec()] on each values column in `data`.
#' [rvec()] chooses the class for each
#' `rvec` object it creates based on
#' the contents of each values column.
#'
#' To enforce particular choices of classes,
#' supply a value for the `types` argument.
#' `type` must be a string, with the number of
#' characters equal to the length of `values`.
#' Each character in `type` specifies
#' particular constructor function:
#' - c = [rvec_chr()]
#' - d = [rvec_dbl()]
#' - i = [rvec_int()]
#' - l = [rvec_lgl()]
#' - ? = [rvec()]
#'
#' (These codes are based on codes used in the
#' `readr` package.)
#'
#' @param data A data frame, possibly
#' [grouped][dplyr::grouped_df].
#' @param draw <[`tidyselect`][tidyselect::language]>
#' Column within `data` that identifies draws within
#' each combination of ID variables.
#' @param values <[`tidyselect`][tidyselect::language]>
#' One or more columns in `data` that hold
#' values for random draws.
#' @param types Optional. A string, each letter
#' of which represents specifies an `rvec` constructor
#' function.
#'
#' @returns A [tibble][tibble::tibble()].
#'
#' @seealso
#' - [rvec()] to construct a single `rvec`.
#'
#' @examples
#' to_rvecs(data = divorces,
#'          draw = sim,
#'          values = rate)
#'
#' ## specify type
#' to_rvecs(data = divorces,
#'          draw = sim,
#'          values = rate,
#'          type = "d")
#'
#' ## dataset with a redundant column
#' library(dplyr)
#' divorces2 <- divorces %>%
#'   mutate(extra = seq_len(n()))
#'
#' ## earlier call no longer works
#' try(
#'   to_rvecs(data = divorces2,
#'            draw = sim,
#'            values = rate)
#' )
#'
#' ## one solution: use 'group_by' to
#' ## specify the ID columns
#' divorces2 <- divorces2 %>%
#'   group_by(age, sex, time)
#' to_rvecs(data = divorces2,
#'          draw = sim,
#'          values = rate)
#' @export
to_rvecs <- function(data,
                     draw = draw,
                     values = value,
                     types = NULL) {
    UseMethod("to_rvecs")
}

#' @export
to_rvecs.data.frame <- function(data,
                                draw = draw,
                                values = value,
                                types = NULL) {
    draw <- rlang::enquo(draw)
    values <- rlang::enquo(values)
    colnum_draw <- tidyselect::eval_select(draw, data = data)
    colnums_values <- tidyselect::eval_select(values, data = data)
    colnums_all <- get_colnames_all(data)
    colnums_id <- colnums_all[!(colnums_all %in% c(colnum_draw, colnums_values))]
    to_rvecs_inner(data = data,
                   colnum_data = colnum_data,
                   colnums_values = colnums_values,
                   colnums_id = colnums_id,
                   types = types)
}

#' @export
to_rvecs.grouped_df <- function(data,
                                draw = draw,
                                values = value,
                                types = NULL) {
    draw <- rlang::enquo(draw)
    values <- rlang::enquo(values)
    colnum_draw <- tidyselect::eval_select(draw, data = data)
    colnums_values <- tidyselect::eval_select(values, data = data)
    colnums_groups <- get_colnums_groups(data)
    colnums_id <- colnums_groups[!(colnums_group %in% c(colnum_draw, colnums_values))]
    data <- data[c(colnums_draw, colnums_values, colnums_id)]
    to_rvecs_inner(data = data,
                   colnum_data = colnum_data,
                   colnums_values = colnums_values,
                   colnums_id = colnums_id,
                   types = types)
}


## Function that does the actual work -----------------------------------------


to_rvecs_inner <- function(data,
                           colnum_draw,
                           colnums_values,
                           colnums_id,
                           types) {
    ## check and process inputs
    if (length(colnum_draw) == 0L)
        cli::cli_abort("No {.var draw} column selected.")
    if (length(colnum_draw) > 1L)
        abort("Attempt to select more than one {.var draw} column.")
    if (length(colnums_values) == 0L)
        cli::cli_abort("No {.var values} columns selected.")
    if (colnum_draw %in% colnums_values)
        cli::cli_abort(paste("{.arg draw} and {.arg values} both use",
                             "column {.var names(colnum_draw)}."))
    check_to_rvecs_types(types = types,
                         colnums_values = colnums_values)
    rvecs <- lapply(colnums_values, list())
    rvec_funs <- get_rvec_funs(types)
    if (nrow(data) > 0L) {
        ## derive and check indices for contents of rvecs
        idvars_data <- data[colnums_id]
        idvars_ans <- unique(idvars_data)
        key_id_data <- paste_cols(idvars_data)
        key_id_ans <- paste_cols(idvars_ans)
        draw_data <- data[[colnum_draw]]
        draw_ans <- sort(unique(draw_data))
        idx <- cbind(match(key_id_data, key_id_ans),
                     match(draw_data, draw_ans))
        check_to_rvecs_idx_dup(idx = idx,
                               data = data,
                               colnum_draw = colnum_draw,
                               colnums_id = colnums_id)
        check_to_rvecs_idx_gap(idx = idx,
                               idvars_ans = idvars_ans,
                               draw_ans = draw_ans)
        ## make rvec objects
        x <- matrix(nrow = nrow(idvars_ans), ncol = length(draw_ans))
        for (i in seq_along(colnums_values)) {
            colnum_val <- colnums_values[[i]]
            x[idx] <- data[[colnum_val]]
            rvecs[[i]] <- rvec_funs[[i]](x)
        }
    }
    else {
        idvars_ans <- data[colnums_id]
        for (i in seq_along(colnums_values))
            rvecs[[i]] <- rvec_funs[[i]]()
    }
    ## assemble ans
    idvars_ans <- tibble::as_tibble(idvars_ans)
    rvecs <- tibble::as_tibble(rvecs)
    ans <- tibble(idvars_ans, rvecs)
    ans
}



