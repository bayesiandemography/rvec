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
#' - [collapse_to_rvec()]
#' - [expand_from_rvec()]
#'
#' Casting to other formats:
#' - [as_list_col()]
#' - [as_rvec()]
#' - [as.matrix()]
#' - [posterior::as_rvar()]
#'
#'
#' ## Map operations
#'
#' - vctrs vs base vectors
#' - [if_else_rvec()]
#'
#'
#' ## Reduce operations
#'
#' [reduce_rvec()]
#' [reduce_rvec_cols()]
#' 
#' 
#'
#' @aliases rvec-package NULL
#' @import vctrs
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
