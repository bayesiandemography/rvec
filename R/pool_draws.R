
#' Pool Draws
#'
#' Combine draws within each
#' combination of grouping or 'by' variables
#' in a data frame.
#'
#' Each combination of grouping or 'by'
#' variables must have the same
#' number of rows.
#'
#' @param data A data frame with one or
#' more rvecs. Can be
#' [grouped](https://dplyr.tidyverse.org/reference/group_data.html).
#' @param by The variables distingishing
#' units after combining. Used if
#' `data` is not
#' [grouped](https://dplyr.tidyverse.org/reference/group_data.html).
#'
#' @returns A data frame.
#'
#' @seealso
#' - [collapse_to_rvec()] Convert from 'draws-and-value'
#'   fromat to rvec format
#' - [expand_from_rvec()] Convert from rvec format to
#'   'draws-and-value' format
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' 
#' df <-  tibble(
#'   a = c(1, 1, 2, 2),
#'   x = rvec(list(1:2, 3:4, 5:6, 7:8))
#' )
#' df
#' df |> pool_draws(by = a)
#' df |> group_by(a) |> pool_draws()
#' df |> pool_draws()
#' 
#' df_big <- tibble(
#'   a = c(1, 1, 2, 2, 1, 1, 2, 2),
#'   b = c(1, 1, 1, 1, 2, 2, 2, 2),
#'   x = rvec(list(1:2, 3:4, 5:6, 7:8,
#'                 9:10, 11:12, 13:14, 15:16)),
#'   y = rvec(list(1:3, 4:6, 7:9, 10:12,
#'                 13:15, 16:18, 19:21, 22:24))
#' )
#' df_big |> pool_draws(by = c(a, b))
#' df_big |> group_by(a, b) |> pool_draws()
#' df_big |> pool_draws(by = a)
#' @export
pool_draws <- function(data,
                       by = NULL) {
  UseMethod("pool_draws")
}

## HAS_TESTS
#' @rdname pool_draws
#' @export
pool_draws.data.frame <- function(data,
                                  by = NULL) {
  by_quo <- rlang::enquo(by)
  by_colnums <- tidyselect::eval_select(by_quo, data = data)
  groups_colnums <- integer()
  pool_draws_inner(data = data,
                   by_colnums = by_colnums,
                   groups_colnums = groups_colnums)
}

## HAS_TESTS
#' @rdname pool_draws
#' @export
pool_draws.grouped_df <- function(data,
                                  by = NULL) {
  by_quo <- rlang::enquo(by)
  by_colnums <- tidyselect::eval_select(by_quo, data = data)
  groups_colnums <- get_groups_colnums(data)
  pool_draws_inner(data = data,
                   by_colnums = by_colnums,
                   groups_colnums = groups_colnums)
}


## HAS_TESTS
#' Pool Draws within Each Combination of the 'by' or Grouping Variables
#'
#' @param data A data frame
#' @param by_colnums Column numbers of the 'by' variables
#' @param groups_colnums Column numbers of the grouping variables
#'
#' @returns A data frame with number of rows
#' equal to the number of distinct combinations
#' of the 'by' or grouping variables
#' 
#' @noRd
pool_draws_inner <- function(data,
                             by_colnums,
                             groups_colnums) {
  ## check and process inputs
  if (identical(nrow(data), 0L))
    cli::cli_abort("{.arg data} has 0 rows.")
  check_not_has_by_and_groups(by_colnums = by_colnums,
                              groups_colnums = groups_colnums)
  rvec_colnums <- get_rvec_colnums(data)
  if (length(rvec_colnums) == 0L)
    cli::cli_abort("{.arg data} does not have any rvecs.")
  check_overlap_rvec_groups(groups_colnums = groups_colnums,
                            rvec_colnums = rvec_colnums)
  check_overlap_rvec_by(by_colnums = by_colnums,
                        rvec_colnums = rvec_colnums)
  has_by <- length(by_colnums) > 0L
  has_groups <- length(groups_colnums) > 0L
  ## calculations
  if (has_by || has_groups) {
    if (has_groups)
      by_colnums <- groups_colnums
    data_split <- vctrs::vec_split(data[rvec_colnums], data[by_colnums])
    val <- data_split$val
    key <- data_split$key
    n_val <- vapply(val, nrow, 1L)
    is_equal <- n_val == n_val[[1L]]
    i_unequal <- match(FALSE, is_equal, nomatch = 0L)
    if (i_unequal > 0L) {
      if (has_by) {
        n_by <- length(by_colnums)
        if (n_by > 1L)
          msg <- paste("Some combinations of the 'by' variables",
                       "occur more often than others.")
        else
          msg <- paste("Some values of the 'by' variable",
                       "occur more often than others.")
      }
      else {
        n_groups <- length(groups_colnums)
        if (n_groups > 1L)
          msg <- paste("Some combinations of the grouping variables",
                       "occur more often than others.")
        else
          msg <- paste("Some values of the grouping variable",
                       "occur more often than others.")
      }
      cli::cli_abort(msg)
    }
    val <- lapply(val, pool_draws_df)
    val <- vctrs::vec_rbind(!!!val)
    ans <- vctrs::vec_cbind(key, val)
  }
  else {
    ans <- lapply(data[rvec_colnums], pool_draws_vec)
    ans <- tibble::as_tibble_row(ans)
  }
  ans
}


## HAS_TESTS
#' Pool Draws in a Data Frame of Rvecs
#'
#' @param df A data frame, consisting entirely of rvecs
#'
#' @returns A data frame, in which each rvec has
#' length 1.
#'
#' @noRd
pool_draws_df <- function(df) {
  if (nrow(df) > 0L) {
    ans <- lapply(df, pool_draws_vec)
    ans <- tibble::as_tibble_row(ans)
  }
  else
    ans <- df
  ans
}


## HAS_TESTS
#' Combine Draws From All Elements of an Rvec
#'
#' @param vec An rvec
#'
#' @returns An rvec of length 1
#'
#' @noRd
pool_draws_vec <- function(vec) {
  if (!rvec::is_rvec(vec))
    cli::cli_abort("Internal error: {.arg vec} is not an rvec.")
  if (length(vec) > 0L) {
    m <- field(vec, "data")
    attributes(m) <- NULL
    ans <- matrix(m, nrow = 1L)
    ans <- rvec(ans)
  }
  else
    ans <- vec
  ans
}
