

## NO_TESTS
#' @export
names.rvec <- function(x) {
    data <- field(x, "data")
    rownames(data)
}



## NO_TESTS
#' @export
`names<-.rvec` <- function(x, value) {
    if (is.null(value))
        rownames(field(x, "data")) <- NULL
    else {
        n_x <- nrow(field(x, "data"))
        n_value <- length(value)
        if (n_value < n_x) {
            padding <- rep(NA, times = n_x - n_value)
            value <- c(value, padding)
        }
        if (n_value > n_x)
            cli::cli_abort(c("Names vector too long.",
                             i = "{.val x} has length {n_x}.",
                             i = "Names vector has length {n_value}."))
        rownames(field(x, "data")) <- value
    }
    x
}


