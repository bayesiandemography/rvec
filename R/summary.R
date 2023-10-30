
#' @export
summary.rvec <- function(object, ...) {
    ans <- c(Length = length(object),
             Class = class(object)[[1L]],
             Mode = mode(vctrs::field(object, "data")),
             n_draw = n_draw(object))
    class(ans) <- c("summaryDefault", "table")
    ans
}
             
