
#' @export
`==.rvec` <- function(e1, e2) {
    compare_rvec(e1 = e1, e2 = e2, op = "==")
}

#' @export
`!=.rvec` <- function(e1, e2) {
    compare_rvec(e1 = e1, e2 = e2, op = "!=")
}

#' @export
`<.rvec` <- function(e1, e2) {
    compare_rvec(e1 = e1, e2 = e2, op = "<")
}

#' @export
`<=.rvec` <- function(e1, e2) {
    compare_rvec(e1 = e1, e2 = e2, op = "<=")
}

#' @export
`>=.rvec` <- function(e1, e2) {
    compare_rvec(e1 = e1, e2 = e2, op = ">=")
}

#' @export
`>.rvec` <- function(e1, e2) {
    compare_rvec(e1 = e1, e2 = e2, op = ">")
}


## Helper functions -----------------------------------------------------------

compare_rvec <- function(e1, e2, op) {
    args <- vec_recycle_common(e1, e2)
    args <- vec_cast_common(!!!args)
    args <- lapply(args, as.matrix)
    data <- Reduce(op, args)
    new_rvec_lgl(data)
}
