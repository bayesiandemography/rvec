
## HAS_TESTS    
#' @export
plot.rvec_chr <- function(x,
                          y = NULL,
                          ylab = NULL,
                          factor = NULL,
                          alpha = NULL,
                          col = NULL,
                          las = NULL,
                          ...) {
    m <- field(x, "data")
    if (!is.null(y))
        cli::cli_abort(c("Value supplied for {.arg y}.",
                         i = "Value cannot be supplied for {.arg y} when {.arg x} has class {.cls {class(x)}}."))
    if (is.null(ylab))
        ylab <- deparse1(substitute(x))
    if (is.null(factor))
        factor <- 0.25
    if (is.null(alpha))
        alpha <- max(1 - 0.001 * length(m), 0.25)
    if (is.null(col))
        col <- grDevices::gray(level = 0.5, alpha = alpha)
    if (is.null(las))
        las <- 1
    Index <- as.integer(row(m))
    yval_labels <- yval_labels_for_plot_chr(m)
    yval <- yval_labels$yval
    labels <- yval_labels$labels
    Index <- jitter(Index, factor = factor)
    yval <- jitter(yval, factor = factor)
    labels[is.na(labels)] <- "NA"
    graphics::plot(yval ~ Index,
                   ylab = ylab,
                   col = col,
                   yaxt = "n",
                   las = las,
                   ...)
    graphics::axis(side = 2L,
                   at = seq_along(labels),
                   labels = labels,
                   las = las,
                   ...)
}

## HAS_TESTS
#' @export
plot.rvec_dbl <- function(x,
                          y = NULL,
                          ylab = NULL,
                          alpha = NULL,
                          col = NULL,
                          las = NULL,
                          ...) {
    m <- field(x, "data")
    if (!is.null(y))
        cli::cli_abort(c("Value supplied for {.arg y}.",
                         i = "Value cannot be supplied for {.arg y} when {.arg x} has class {.cls {class(x)}}."))
    if (is.null(ylab))
        ylab <- deparse1(substitute(x))
    if (is.null(alpha))
        alpha <- max(1 - 0.001 * length(m), 0.25)
    if (is.null(col))
        col <- grDevices::gray(level = 0.5, alpha = alpha)
    if (is.null(las))
        las <- 1
    Index <- as.integer(row(m))
    yval <- as.double(m)
    graphics::plot(yval ~ Index,
                   ylab = ylab,
                   col = col,
                   las = las,
                   ...)
}

## HAS_TESTS
#' @export
plot.rvec_int <- function(x,
                          y = NULL,
                          ylab = NULL,
                          factor = NULL,
                          alpha = NULL,
                          col = NULL,
                          las = NULL,
                          ...) {
    m <- field(x, "data")
    if (!is.null(y))
        cli::cli_abort(c("Value supplied for {.arg y}.",
                         i = "Value cannot be supplied for {.arg y} when {.arg x} has class {.cls {class(x)}}."))
    if (is.null(ylab))
        ylab <- deparse1(substitute(x))
    if (is.null(factor))
        factor <- 0.25
    if (is.null(alpha))
        alpha <- max(1 - 0.001 * length(m), 0.25)
    if (is.null(col))
        col <- grDevices::gray(level = 0.5, alpha = alpha)
    if (is.null(las))
        las <- 1
    Index <- as.integer(row(m))
    yval <- as.double(m)
    Index <- jitter(Index, factor = factor)
    yval <- jitter(yval, factor = factor)
    graphics::plot(yval ~ Index,
                   ylab = ylab,
                   col = col,
                   las = las,
                   ...)
}

## HAS_TESTS
#' @export
plot.rvec_lgl <- function(x,
                          y = NULL,
                          ylab = NULL,
                          factor = NULL,
                          alpha = NULL,
                          col = NULL,
                          las = NULL,
                          ...) {
    m <- field(x, "data")
    if (!is.null(y))
        cli::cli_abort(c("Value supplied for {.arg y}.",
                         i = "Value cannot be supplied for {.arg y} when {.arg x} has class {.cls {class(x)}}."))
    if (is.null(ylab))
        ylab <- deparse1(substitute(x))
    if (is.null(factor))
        factor <- 0.25
    if (is.null(alpha))
        alpha <- max(1 - 0.001 * length(m), 0.25)
    if (is.null(col))
        col <- grDevices::gray(level = 0.5, alpha = alpha)
    if (is.null(las))
        las <- 1
    Index <- as.integer(row(m))
    yval <- as.integer(m)
    Index <- jitter(Index, factor = factor)
    yval <- jitter(yval, factor = factor)
    graphics::plot(yval ~ Index,
                   ylab = ylab,
                   col = col,
                   yaxt = "n",
                   las = las,
                   ...)
    graphics::axis(side = 2,
                   at = c(0, 1),
                   labels = c("FALSE", "TRUE"),
                   las = las,
                   ...)
}


## Helpers --------------------------------------------------------------------

## HAS_TESTS
#' Create yval and labels to use in
#' plot.rvec_chr
#'
#' @param m Matrix of data from rvec
#'
#' @returns A named list
#'
#' @noRd
yval_labels_for_plot_chr <- function(m) {
    n_max <- 15L
    str_other <- "<Other>"
    tab <- table(m, useNA = "ifany")
    tab <- sort(tab, decreasing = TRUE)
    labels <- names(tab)
    n <- length(tab)
    if (n > n_max) {
        labels <- c(labels[seq_len(n_max - 1L)],
                    str_other)
        m[!(m %in% labels)] <- str_other
    }
    yval <- min(n, n_max) + 1L - match(m, labels)
    list(yval = yval, labels = labels)
}

         
    
    
