
## HAS_TESTS
#' Extract a Single Draw From an Rvec
#'
#' Extract a single draw from `x`.
#' If a value is supplied for `i`,
#' extract the `i`th draw; otherwise
#' extract a random draw.
#'
#' @param x An [rvec][rvec()].
#' @param i Index for the draw
#' to be extracted. A number between
#' `1` and `n_draw(x)`. If no value is
#' supplied, a draw is chosen at random.
#'
#' @returns
#' A vector, with type
#' - double, if `x` has class `"rvec_dbl"`,
#' - integer, if `x` has class `"rvec_int"`,
#' - character, if `x` has class `"rvec_chr"`,
#' - logical, if `x` has class `"rvec_lgl"`.
#'
#' @seealso
#' [n_draw()] Number of draws
#' 
#' @examples
#' x <- rvec(matrix(1:50, ncol = 5))
#' extract_draw(x, i = 1)
#' extract_draw(x)
#' @export
extract_draw <- function(x, i = NULL) {
  if (!is_rvec(x))
    cli::cli_abort(c("{.arg x} is not an rvec.",
                     i = "{.arg x} has class {.cls {class(x)}}."))
  n_draw <- n_draw(x)
  if (is.null(i))
    i <- sample(x = n_draw, size = 1L)
  else
    check_i(i = i, n_draw = n_draw)
  m <- as.matrix(x)
  m[, i]
}

    
  
