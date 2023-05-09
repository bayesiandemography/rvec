#' Package 'rvec'
#'
#' - logic from `vctrs`
#' - uses `matrixStats` for speed
#'
#' - motivation
#'     - reduce size of data frames
#'     - make 
#'
#' # Functions
#'
#' ## Creating and converting
#' 
#' Create individual `rvec`s:
#' - [rvec()]
#' - [rvec_dbl()]
#' - [rvec_int()]
#' - [rvec_lgl()]
#' - [rvec_chr()]
#'
#' Reformat an entire data frame:
#' - [collapse_to_rvecs()]
#' - [expand_from_rvecs()]
#'
#' Convert between `rvec`s, list columns, and matrices
#' - [as_list_col()]
#' - [as_rvec()]
#' - [as_matrix()], [as.matrix()]
#' - [posterior::as_rvar()]
#'
#'
#' ## Map operations
#'
#' - vctrs vs base vectors
#' - [rvec_if_else()]
#'
#'
#' ## Reduce operations
#'
#' [reduce_to_means()]
#' [reduce_to_medians()]
#' [reduce_to_modes()]
#' [reduce_to_ci()]
#'
#' 
#'
#' @aliases rvec-package NULL
#' @import vctrs
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
