
reduce_rvec_cols <- function(data,
                             cols = NULL,
                             method = c("median", "mean", "mode"),
                             width = 0.95,
                             na_rm = FALSE,
                             format = c("long", "wide")) {
    ## check and process inputs
    cols <- rlang::enquo(cols)
    colnums <- tidyselect::eval_select(cols, data = data)
    if (length(colnums) == 0L)
        colnums <- get_colnums_rvec(data)
    nms_colnums <- names(colnums)
    n_cols <- length(colnums)
    for (nm in nms_colnums) {
        x <- data[[nm]]
        if (!is_rvec(x))
            cli::cli_abort(c("{.var {nm}} is not an rvec",
                             "x" = "{.var {nm}} has class {.cls {class(x)}}"))
    }
    n_width <- length(width)
    ## nothing to do if no cols
    if (n_cols == 0L)
        return(data)
    ## calculate point estimates
    vals <- lapply(data[colnums],
                   reduce_rvec,
                   method = method,
                   na_rm = na_rm)
    names(points) <- paste(nms_colnums)
    ## if no intervals, then we're finished
    if (n_width == 0L) {
        ans[colnums] <- vals
    }
    ## otherwise calculate intervals
    else {
        intervals <- lapply(data[col_nums],
                            reduce_rvec_intervals,
                            width = width,
                            na_rm = na_rm,
                            vname = nms_colnums,
                            format = format)
        ans[colnums] <- NA
        if (format == "long")
            ans <- do.call(vec_rbind, rep.int(ans, n_width))
        for (i in seq_along(colnums)) {
            colnum <- colnums_cols[[i]]
            val <- c(vals[i], intervals[[i]])
            ans[[colnum]] <- val
        }
        ans <- new_data_frame(ans)
    }
    ans <- tibble::tibble(ans)
    ans
}



reduce_rvec_intervals <- function(x, width, na_rm, vname, format = c("wide", "long")) {
    width <- check_and_tidy_width(width)
    check_na_rm(na_rm)
    format <- rlang::arg_match(format)
    m <- field(m, "data")
    probs <- make_probs(width)
    ans <- matrixStats::rowQuantiles(m, probs = probs, na.rm = na_rm)
    if (format == "wide") {
        colnames(ans) <- paste(vname,
                               rep(c(".lower", ".upper"), each = nrow(quantile) / 2),
                               c(width, rev(width)),
                               sep = ".")
        ans <- matrix_to_list_of_cols(ans)
    }
    else {
        ans <- matrix(ans, ncol = 2L)
        colnames(ans) <- paste(vname,
                               c(".lower", ".upper"),
                               sep = ".")
        ans <- matrix_to_list_of_cols(ans)
        .width <- rep(width, each = nrow(m))
        ans <- c(ans, list(.width = .width))
    }
    ans
}
        


#' Scalar summaries of draws
#'
#' Reduce an [rvec][rvec()] object to a vector
#' of scalar summaries.
#'
#' When `method` is `"mode"`, `reduce_rvec()`
#' returns the most common value for each
#' observation. When there is a tie, it returns
#' `NA`.
#'
#' @param x An object of class [rvec][rvec()].
#' @param method Summary measure: a choice of
#' `"median"` (the default), `"mean"`, and `"mode"`.
#' @param na_rm Whether to remove NAs before
#' calculating summaries. Default is `FALSE`.
#'
#' @returns A vector.
#'
#' @seealso
#' Multi-number summaries:
#' - [reduce_rvec_ci()]
#' - [reduce_rvec()]
#'
#' Means and medians are calculated using functions
#' from package `matrixStats`.
#'
#' @examples
#' m <- cbind(c(1, 1, 1, 2, 3),
#'            c(2, 4, 0, 2, 3),
#'            c(0, 0, 1, 0, 100))
#' x <- rvecs(m)
#' x
#' reduce_rvec(x) # defaults to median
#' reduce_rvec(x, method = "mean")
#' reduce_rvec(x, method = "mode")
#' @export
reduce_rvec <- function(x,
                      method = c("median", "mean", "mode"),
                      na_rm = FALSE) {
    UseMethod("reduce_rvec")
}

#' @rdname reduce_rvec
#' @export
reduce_rvec.rvec <- function(x,
                           method = c("median", "mean", "mode"),
                           na_rm = FALSE) {
    method <- rlang::arg_match(method)
    check_na_rm(na_rm)
    m <- field(x, "data")
    if (method == "median")
        matrixStats::rowMedians(m, na.rm = na_rm)
    else if (method == "mean")
        matrixStats::rowMeans2(m, na.rm = na_rm)
    else
        row_modes(m, na_rm = na_rm)
}

#' @rdname reduce_rvec
#' @export
reduce_rvec.rvec_chr <- function(x,
                               method = c("median", "mean", "mode"),
                               na_rm = FALSE) {
    method <- rlang::arg_match(method)
    check_na_rm(na_rm)
    m <- field(x, "data")
    if (method == "median")
        cli::cli_abort("Median not defined for characters")
    else if (method == "mean")
        cli::cli_abort("Mean not defined for characters")
    else
        row_modes(m, na_rm = na_rm)
}
    
#' @rdname reduce_rvec
#' @export
reduce_rvec.rvec_lgl <- function(x, 
                               method = c("median", "mean", "mode"),
                               na_rm = FALSE) {
    method <- rlang::arg_match(method)
    check_na_rm(na_rm)
    m <- field(x, "data")
    if (method == "median") {
        m <- 1 * m
        matrixStats::rowMedians(m, na.rm = na_rm)
    }
    if (method == "mean") {
        m <- 1 * m
        matrixStats::rowMeans2(m, na.rm = na_rm)
    }
    else
        row_modes(m, na_rm = na_rm)
}


row_modes <- function(x, na_rm) {
    m <- field(x, "data")
    storage_mode <- storage.mode(m)
    useNA <- if (na_rm) "no" else "ifany"
    tabs <- apply(m, 1L, table, useNA = useNA, simplify = FALSE)
    nms_tabs <- lapply(tabs, names)
    i_max <- lapply(tabs, function(x) which(x == max(x))) # allows multiple
    has_unique_mode <- vapply(i_max, length, 1L) == 1L
    ans <- rep(NA, times = nrow(m))
    modes <- .mapply(function(x, i) x[[i]],
                     dots = list(nms_tabs[has_unique_mode],
                                 i_max[has_unique_mode]),
                     MoreArgs = list())
    modes <- unlist(modes, use.names = FALSE)
    ans[has_unique_mode] <- modes
    storage.mode(ans) <- storage_mode
    ans    
}



