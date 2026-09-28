
## Note that vec_math methods not currently implemented for
## 'median' or for 'sd' and 'var' (neither of which
## are generic functions in base R), so 'rvec' gives them
## their own methods

#' @export
Summary.rvec <- function(..., na.rm = FALSE) {
    if (.Generic %in% c("sum", "prod", "any", "all") && ...length() == 1L) {
        args <- list(...)
        x <- args[[1L]]
        cls <- class(x)
        # Preserve vec_c's name handling and subclass dispatch on the fallback.
        if (is.null(names(args)) &&
            cls[[1L]] %in% c("rvec_dbl", "rvec_int", "rvec_lgl") &&
            identical(cls[-1L], c("rvec", "vctrs_rcrd", "vctrs_vctr")))
            return(vec_math(.Generic, x, na.rm = na.rm))
    }
    NextMethod()
}

#' @export
vec_math.rvec_dbl <- function(.fn, .x, ...) {
  m <- field(.x, "data")
  ## --- Not in Math or Summary group but implemented by vec_math ---
  ## Unlike default vctrs treatment of is.na, these
  ## functions are applied to individual elements 'm', and the
  ## result is a logical matrix with the same dimensions as 'm'
  if (.fn %in% c("is.nan", "is.finite", "is.infinite")) {
    .fn <- match.fun(.fn)
    data <- .fn(m)
  }
  ## --- in Summary group, has matrixStats fun ---
  else if (.fn %in% c("prod", "sum", "any", "all")) {
    matrix_fun <- switch(.fn,
                         prod = function(x, ...)
                           matrixStats::colProds(x, method = "expSumLog", ...),
                         sum = matrixStats::colSums2,
                         any = matrixStats::colAnys,
                         all = matrixStats::colAlls)
    data <- matrix_fun(m, ...)
    data <- matrix(data, nrow = 1L)
  }
  ## --- in Summary group but implemented by vec_math ---
  else if (.fn == "mean") {
    data <- matrixStats::colMeans2(m, ...)
    data <- matrix(data, nrow = 1L)
  }        
  ## --- in Math group and has matrixStats fun ---
  else if (.fn %in% c("cummax", "cummin", "cumprod", "cumsum")) {
    matrix_fun <- switch(.fn,
                         cummax = matrixStats::colCummaxs,
                         cummin = matrixStats::colCummins,
                         cumprod = matrixStats::colCumprods,
                         cumsum = matrixStats::colCumsums)
    data <- matrix_fun(m, ...)
  }
  ## --- everything else in Math group ---
  else {
    .fn <- match.fun(.fn)
    data <- .fn(m, ...)
  }
  ## --- return rvec ---
  if (is.double(data))
    .new_rvec_dbl(data)
  else if (is.integer(data))
    .new_rvec_int(data)
  else if (is.logical(data))
    .new_rvec_lgl(data)
  else
    cli::cli_abort("Internal error: {.arg data} has type {typeof(data)}.") ## nocov
}

## give same types as base functions
#' @export
vec_math.rvec_int <- function(.fn, .x, ...) {
    ans_original <- vec_math.rvec_dbl(.fn = .fn, .x = .x, ...)
    if (.fn == "sum") {
        m <- field(ans_original, "data")
        rvec_int(m)
    }
    else if (.fn == "cumprod") {
        m <- field(ans_original, "data")
        rvec_dbl(m)
    }
    else
        ans_original
}

#' @export
vec_math.rvec_lgl <- function(.fn, .x, ...) {
    if (.fn %in% c("is.nan", "is.finite", "is.infinite"))
        return(vec_math.rvec_dbl(.fn = .fn, .x = .x, ...))
    m <- field(.x, "data")
    data <- as.integer(m)
    dim(data) <- dim(m)
    rownames(data) <- rownames(m)
    .x <- .new_rvec_int(data)
    vec_math.rvec_int(.fn = .fn, .x = .x, ...)
}
