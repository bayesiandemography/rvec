
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
    standard <- function(x) {
        if (!is_rvec(x))
            return(is.atomic(x) && is.vector(x) &&
                   typeof(x) %in% c("double", "integer", "logical", "character"))
        any(vapply(c("dbl", "int", "lgl", "chr"), function(type)
            identical(class(x), c(paste0("rvec_", type), "rvec",
                                  "vctrs_rcrd", "vctrs_vctr")), TRUE))
    }
    if (all(vapply(args, standard, TRUE))) {
        ## Resolve the common type and draw compatibility before casting, but
        ## retain each operand's existing draw count rather than expanding it.
        ptype <- vec_ptype_common(!!!args)
        nd <- n_draw(ptype)
        type <- as.vector(field(ptype, "data"))
        args <- lapply(args, function(x) {
            target <- ptype_rvec(if (is_rvec(x)) n_draw(x) else 1L, type)
            as.matrix(vec_cast(x, target))
        })
        n <- nrow(args[[1L]])
        nms <- if (!is.null(rownames(args[[1L]]))) rownames(args[[1L]]) else rownames(args[[2L]])
        if (ncol(args[[1L]]) != ncol(args[[2L]]))
            args <- lapply(args, function(m) if (ncol(m) == 1L) as.vector(m) else m)
        data <- Reduce(op, args)
        dim(data) <- c(n, nd)
        dimnames(data) <- if (is.null(nms)) NULL else list(nms, NULL)
    }
    else {
        args <- vec_cast_common(!!!args)
        args <- lapply(args, as.matrix)
        data <- Reduce(op, args)
    }
    .new_rvec_lgl(data)
}
