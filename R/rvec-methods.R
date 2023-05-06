
## as.matrix ------------------------------------------------------------------

#' @export
as.matrix.rvec <- function(x, ...) {
    data <- vec_data(x)$data
    nrow <- n_obs(x)
    ncol <- n_draw(x)
    matrix(unlist(data, use.names = FALSE),
           nrow = nrow,
           ncol = ncol,
           byrow = TRUE)
}
                  

## 'format' -------------------------------------------------------------------

#' @export
format.rvec <- function(x, ...) {
    if (vec_is_empty(x))
        return(character())
    m <- as.matrix(x)
    means <- matrixStats::rowMeans2(m)
    sds <- matrixStats::rowSds(m)
    sprintf("%s ± %s",
            signif(means, 2L),
            signif(sds, 2L))
}

#' @export
format.rvec_chr <- function(x, ...) {
    if (vec_is_empty(x))
        return(character())
    m <- as.matrix(x)
    tab <- table(m, row(m)) ## idea from matrixStats::rowTabulates
    tab <- t(tab)           ## to allow use of max.col
    i_max <- max.col(tab)
    val <- colnames(tab)
    most_common_val <- val[i_max]
    sprintf("%s,...",
            most_common_val)
}

#' @export
format.rvec_lgl <- function(x, ...) {
    if (vec_is_empty(x))
        return(character())
    m <- as.matrix(x)
    pr <- matrixStats::rowMeans2(m)
    sprintf("p = %s",
            signif(pr, 2L))
}



## 'n_draw' -------------------------------------------------------------------

#' Number of draws in a random sample
#'
#' @param An object holding a random sample.
#'
#' @returns A count, or `NULL`, if the
#' vector being sampled as no elements.
#'
#' @examples
#' data <- matrix(sample(c(TRUE, FALSE), size = 40, replace = TRUE),
#'                nrow = 4)
#' x <- rvec(data)
#' n_draw(x)
#' @export
n_draw <- function(x) {
    UseMethod("n_draw")
}

#' @rdname n_draw
#' @export
n_draw.rvec <- function(x) {
    data <- vec_data(x)$data
    if (length(data) > 0L)
        length(data[[1L]])
    else
        NULL
}


## 'vec_arith' ----------------------------------------------------------------

vec_arith.rvec <- function(op, x, y, ...) {
  UseMethod("vec_arith.rvec", y)
}

vec_arith.rvec.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}




## 'vec_math' -----------------------------------------------------------------


#' @export
vec_math.rvec <- function(.fn, .x, ...) {
    m <- as.matrix(.x)
    if (.fn == "prod") {
        ans <- matrixStats::colProds(m, method = "expSumLog")
        ans <- matrix(ans, nrow = 1L)
    }
    else if (.fn %in% c("sum", "any", "all", "cummax", "cummin", "cumprod", "cumsum")) {
        matrix_fun <- switch(.fn,
                             sum = matrixStats::colSums2,
                             any = matrixStats::colAnys,
                             all = matrixStats::colAlls,
                             cummax = matrixStats::colCummaxs,
                             cummin = matrixStats::colCummins,
                             cumprod = matrixStats::colCumprods,
                             cumsum = matrixStats::colCumsums)
        ans <- matrix_fun(m)
        if (.fn %in% c("sum", "any", "all"))
            ans <- matrix(ans, nrow = 1L)
    }
    else {
        .fn <- match.fun(.fn)
        ans <- .fn(m)
    }
    rvec(ans)
}

## give same types as base functions
#' @export
vec_math.rvec_int <- function(.fn, .x, ...) {
    ans <- vec_math.rvec(.fn = .fn, .x = .x, ...)
    if (.fn == "sum") {
        data <- vec_data(ans)$data
        data <- as_list_of(data, .ptype = integer())
        new_rvec_int(data)
    }
    else if (.fn == "cumprod")
        vec_cast(ans, to = new_rvec_dbl())
    else
        ans
}

#' @export
vec_math.rvec_lgl <- function(.fn, .x, ...) {
    .x <- vec_cast(.x, rvec_int())
    vec_math.rvec_int(.x = .x, .fn = .fn, ...)
}


## 'vec_ptype_abbr' -----------------------------------------------------------

#' @export
vec_ptype_abbr.rvec_chr <- function(x, ...) {
  "rv_chr"
}

#' @export
vec_ptype_abbr.rvec_dbl <- function(x, ...) {
  "rv_dbl"
}

#' @export
vec_ptype_abbr.rvec_int <- function(x, ...) {
  "rv_int"
}

#' @export
vec_ptype_abbr.rvec_lgl <- function(x, ...) {
  "rv_lgl"
}


## 'vec_ptype2' ---------------------------------------------------------------

## coercion to current class

#' @export
vec_ptype2.rvec_chr.rvec_chr <- function(x, y, ...) new_rvec_chr()

#' @export
vec_ptype2.rvec_dbl.rvec_dbl <- function(x, y, ...) new_rvec_dbl()

#' @export
vec_ptype2.rvec_int.rvec_int <- function(x, y, ...) new_rvec_int()

#' @export
vec_ptype2.rvec_lgl.rvec_lgl <- function(x, y, ...) new_rvec_lgl()


## logical to integer

#' @export
vec_ptype2.rvec_lgl.rvec_int <- function(x, y, ...) new_rvec_int()

#' @export
vec_ptype2.rvec_int.rvec_lgl <- function(x, y, ...) new_rvec_int()


## integer to double

#' @export
vec_ptype2.rvec_int.rvec_dbl <- function(x, y, ...) new_rvec_dbl()

#' @export
vec_ptype2.rvec_dbl.rvec_int <- function(x, y, ...) new_rvec_dbl()


## 'vec_cast' -----------------------------------------------------------------

## cast to current class

#' @export
vec_cast.rvec_chr.rvec_chr <- function(x, to, ...) x

#' @export
vec_cast.rvec_dbl.rvec_dbl <- function(x, to, ...) x

#' @export
vec_cast.rvec_int.rvec_int <- function(x, to, ...) x

#' @export
vec_cast.rvec_lgl.rvec_lgl <- function(x, to, ...) x


## logical to integer

#' @export
vec_cast.rvec_int.rvec_lgl <- function(x, to, ...) {
    data <- vec_data(x)$data
    data <- as_list_of(data, .ptype = integer())
    new_rvec_int(data)
}

#' @export
vec_cast.rvec_dbl.rvec_int <- function(x, to, ...) {
    data <- vec_data(x)$data
    data <- as_list_of(data, .ptype = double())
    new_rvec_dbl(data)
}    
    
    
    




    
    
    
