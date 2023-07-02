
## HAS_TESTS
#' @export
`==.rvec` <- function(e1, e2) {
    compare_rvec(e1 = e1, e2 = e2, op = "==")
}

## HAS_TESTS
#' @export
`!=.rvec` <- function(e1, e2) {
    compare_rvec(e1 = e1, e2 = e2, op = "!=")
}

## HAS_TESTS
#' @export
`<.rvec` <- function(e1, e2) {
    compare_rvec(e1 = e1, e2 = e2, op = "<")
}

## HAS_TESTS
#' @export
`<=.rvec` <- function(e1, e2) {
    compare_rvec(e1 = e1, e2 = e2, op = "<=")
}

## HAS_TESTS
#' @export
`>=.rvec` <- function(e1, e2) {
    compare_rvec(e1 = e1, e2 = e2, op = ">=")
}

## HAS_TESTS
#' @export
`>.rvec` <- function(e1, e2) {
    compare_rvec(e1 = e1, e2 = e2, op = ">")
}


## Helper functions -----------------------------------------------------------

## HAS_TESTS
#' Apply comparison operator 'op' to 'e1' and 'e2'
#'
#' @param e1,e2 Vectors, one or both of
#' which is an rvec
#' @param op A comparison function
#'
#' @returns An rvec
#'
#' @noRd
compare_rvec <- function(e1, e2, op) {
    args <- vec_recycle_common(e1, e2)
    args <- vec_cast_common(!!!args)
    args <- lapply(args, as.matrix)
    data <- Reduce(op, args)
    new_rvec_lgl(data)
}
