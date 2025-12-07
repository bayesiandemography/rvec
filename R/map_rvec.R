
## HAS_TESTS
#' Apply a Function and Put Results in an Rvec
#'
#' Apply function `.f` to each element of `.x`,
#' and then combine the results into an
#' rvec with the same length as `.x`.
#'
#' Each call to function `.f` should produce
#' an [rvec][rvec()] with length 1.
#'
#' @param .x A vector.
#' @param .f A function.
#' @param ... Additional arguments passed to `.f`.
#'
#' @returns An [rvec][rvec()] with the same
#' length as `.x`.
#'
#' @seealso `map_rvec()` is based on the
#' map functions in package
#' [purrr](https://purrr.tidyverse.org/reference/map.html),
#' though the internal implementation is different.
#'
#' Base R functions [sapply()] and [vapply()]
#' do not work properly with rvecs.
#' [lapply()] works, but to combine the
#' results into a single rvec, functions such
#' as [c()] or [vec_c()][vctrs::vec_c()] are needed.
#' @examples
#' l <- list(a = rvec(matrix(1:2, 1)),
#'           b = rvec(matrix(1:4, 2)),
#'           c = rvec(matrix(1:6, 3)))
#' l
#' map_rvec(l, sum)
#'
#' ## sapply does not work with rvecs
#' sapply(l, sum)
#' @export
map_rvec <- function(.x, .f, ...) {
    if (missing(.x))
        cli::cli_abort("{.arg .x} is missing, with no default.")
    if (missing(.f))
        cli::cli_abort("{.arg .f} is missing, with no default.")
    nm_f <- deparse1(substitute(.f))
    if (!vctrs::obj_is_vector(.x))
        cli::cli_abort(c("{.arg .x} is not a vector.",
                         i = "{.arg .x} has class {.cls {class(.x)}}."))
    if (!is.function(.f))
        cli::cli_abort(c("{.arg .f} is not a function.",
                         i = "{.arg .f} has class {.cls {class(.f)}}."))
    ans <- vector(mode = "list", length = length(.x))
    for (i in seq_along(.x)) {
        val <- tryCatch(.f(.x[[i]], ...),
                        error = function(e) e)
        if (inherits(val, "error"))
            cli::cli_abort(c(paste("Problem applying {.fun {nm_f}}",
                                   "to element {i} of {.arg .x}."),
                             ans$message))
        len_val <- length(val)
        if (len_val != 1L)
            cli::cli_abort(c(paste("Return value from applying {.fun {nm_f}}",
                                   "to element {i} of {.arg .x} is",
                                   "not length 1."),
                             i = "Return value is length {len_val}."))
        if (!is_rvec(val))
            cli::cli_abort(c(paste("Return value from applying {.fun {nm_f}}",
                                   "to element {i} of {.arg .x}",
                                   "does not have class {.cls rvec}"),
                             i = "Return value has class {.cls {class(val)}}."))
        ans[[i]] <- val
    }
    ans <- vec_c(!!!ans)
    rownames(field(ans, "data")) <- names(.x)
    ans
}
                                   
            
        
                       
    
