
#' Probability distributions
#'
#' Modified versions of standard probability
#' functions that take one or more rvecs
#' as arguments, and return rvecs.
#'
#' The `*_rvec` probability distribution functions
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' These rules are more restrictive than base R rules,
#' but are also more predictable. Base R style
#' recycling can be done through explicit
#' calls to [base::rep()],
#' [base::rep_len()], and [base::rep.int()],
#' all of which have methods for [rvecs][rvec()].
#'
#' @param n Number of draws.
#' @param x,q Vector of quantiles.
#' @param p Vector of probabilities.
#' @param mean,lambda Vector of means.
#' A standard R vector, or an [rvec][rvec()].
#' @param sd Vector of standard deviations.
#' A standard R vector, or an [rvec][rvec()].
#' @param log,log.p Whether to return
#' `log(p)` rather than `p`. Default is
#' `FALSE`.
#' @param lower.tail Whether to return
#' \eqn{P[X \le x]}, as opposed to
#' \eqn{P[X > x]}. Default is `TRUE`.
#'
#' @returns If any of the inputs is an
#' [rvec][rvec()], then the return value is
#' an [rvec][rvec()]. Otherwise it is a
#' standard R vector.
#'
#' @seealso Base R functions
#' - [stats::dnorm()] Normal distribution.
#' - [stats::dpois()] Poisson distribution.
#'
#' @examples
#' x_rv <- rvec(list(c(-0.8, 1.3),
#'                   c(-9.1, 8.7)))
#' mean_rv <- rvec(list(c(-1, 1),
#'                      c(-10, 10)))
#' sd_rv <- rvec(list(c(0.2, 20)))
#' x_rv
#' mean_rv
#' sd_rv
#'
#' ## densities: all arguments rvecs
#' dnorm_rvec(x = x_rv, mean = mean_rv, sd = sd_rv)
#'
#' ## densities: 'x' is ordinary vector
#' dnorm_rvec(x = c(0, 2), mean = mean_rv, sd = sd_rv)
#'
#' ## ...which is equivalent to
#' c(dnorm_rvec(0, mean = mean_rv[1], sd = sd_rv),
#'   dnorm_rvec(2, mean = mean_rv[2], sd = sd_rv))
#'
#' ## random variates: mean is rvec, sd is rvec
#' rnorm_rvec(n = 2, mean = mean_rv, sd = sd_rv)
#'
#' ## random variates: mean is rvec, sd is scalar
#' rnorm_rvec(n = 2, mean = mean_rv, sd = 0.5)
#' @name rvec-distributions
NULL



## 'norm' ---------------------------------------------------------------------

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
dnorm_rvec <- function(x, mean = 0, sd = 1, log = FALSE) {
    check_flag(log)
    dnorm <- stats::dnorm
    args <- vec_recycle_common(x, mean, sd)
    x <- args[[1]]
    mean <- args[[2]]
    sd <- args[[3]]
    dist_rvec_3(fun = dnorm,
                arg1 = x,
                arg2 = mean,
                arg3 = sd,
                log = log)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
pnorm_rvec <- function(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    pnorm <- stats::pnorm
    args <- vec_recycle_common(q, mean, sd)
    q <- args[[1]]
    mean <- args[[2]]
    sd <- args[[3]]
    dist_rvec_3(fun = pnorm,
                arg1 = q,
                arg2 = mean,
                arg3 = sd,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
qnorm_rvec <- function(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    qnorm <- stats::qnorm
    args <- vec_recycle_common(p, mean, sd)
    p <- args[[1L]]
    mean <- args[[2L]]
    sd <- args[[3L]]
    dist_rvec_3(fun = qnorm,
                arg1 = p,
                arg2 = mean,
                arg3 = sd,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
rnorm_rvec <- function(n, mean = 0, sd = 1) {
    rnorm <- stats::rnorm
    mean <- vec_recycle(mean, size = n)
    sd <- vec_recycle(sd, size = n)
    n <- n_rdist(n = n, args = list(mean, sd))
    dist_rvec_2(fun = rnorm,
                arg1 = mean,
                arg2 = sd,
                n = n)
}


## 'pois' ---------------------------------------------------------------------


## Check as many inputs as possible, because
## errors that are thrown via 'fun(arg, ...)'
## are likely to be hard to interpret.

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
dpois_rvec <- function(x, lambda, log = FALSE) {
    check_flag(log)
    dpois <- stats::dpois
    args <- vec_recycle_common(x, lambda)
    x <- args[[1L]]
    lambda <- args[[2L]]
    dist_rvec_2(fun = dpois,
                arg1 = x,
                arg2 = lambda,
                log = log)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
ppois_rvec <- function(q, lambda, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    ppois <- stats::ppois
    args <- vec_recycle_common(q, lambda)
    q <- args[[1L]]
    lambda <- args[[2L]]
    dist_rvec_2(fun = ppois,
                arg1 = q,
                arg2 = lambda,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
qpois_rvec <- function(p, lambda, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    qpois <- stats::qpois
    args <- vec_recycle_common(p, lambda)
    p <- args[[1L]]
    lambda <- args[[2L]]
    dist_rvec_2(fun = qpois,
                arg1 = p,
                arg2 = lambda,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
rpois_rvec <- function(n, lambda) {
    rpois <- stats::rpois
    lambda <- vec_recycle(lambda, size = n)
    n <- n_rdist(n = n, args = list(lambda))
    dist_rvec_1(fun = rpois,
                arg = lambda,
                n = n)
}


## Helper functions -----------------------------------------------------------

## HAS_TESTS
#' Apply random-distribution function to an rvec:
#' function has one parameter
#'
#' Assume that 'arg' has already be recycled,
#' if necessary, to have the desired length.
#'
#' In practice random variate
#' functions are the
#' only distribution functions to
#' have one parameter.
#'
#' The 'n' argument
#' is not a parameter. It is passed in
#' via .... The calling function
#' is responsible for setting 'n'
#' to 'length(as.matrix(arg))'.
#'
#' @param fun The function to be applied
#' @param arg Parameter argument for the function
#' @param ... Other arguments passed to fun
#'
#' @returns If arg is an rvec, then an rvec.
#' Otherwise a numeric vector.
#'
#' @noRd
dist_rvec_1 <- function(fun, arg, ...) {
    nm_fun <- rlang::as_name(rlang::enquo(fun))
    is_arg_rvec <- is_rvec(arg)
    if (is_arg_rvec) {
        n_draw <- n_draw(arg)
        arg <- as.vector(as.matrix(arg))
    }
    ans <- tryCatch(fun(arg, ...),
                    error = function(e) e)
    if (inherits(ans, "error"))
        cli::cli_abort(c("Problem with call to function {.fun {nm_fun}}.",
                         i = ans$message))
    if (is_arg_rvec) {
        ans <- matrix(ans, ncol = n_draw)
        ans <- rvec(ans)
    }
    ans
}


## HAS_TESTS
#' Apply random-distribution function to an rvec:
#' function has two parameter
#'
#' Assume that 'arg1' and 'arg2' have
#' already been recycled,
#' if necessary, to have the required lengths.
#'
#' Note that in practice random variate
#' functions are the
#' only distribution functions to
#' have one parameter.
#'
#' If the functin is a random variate
#' function, then the 'n' argument
#' is  passed in via ....
#' The calling function
#' is responsible for setting 'n'
#' to 'length(as.matrix(arg))'.
#'
#' @param fun The function to be applied
#' @param arg1,arg2 Parameter arguments for the function
#' @param ... Other arguments passed to fun
#'
#' @returns If arg1 or arg2 is an rvec, then an rvec.
#' Otherwise a numeric vector.
#'
#' @noRd
dist_rvec_2 <- function(fun, arg1, arg2, ...) {
    nm_fun <- rlang::as_name(rlang::enquo(fun))
    nm_arg1 <- rlang::as_name(rlang::enquo(arg1))
    nm_arg2 <- rlang::as_name(rlang::enquo(arg2))
    is_rv_1 <- is_rvec(arg1)
    is_rv_2 <- is_rvec(arg2)
    is_rv <- is_rv_1 || is_rv_2
    if (is_rv) {
        if (is_rv_1 && is_rv_2)
            check_n_draw_equal(x = arg1,
                               y = arg2,
                               x_arg = nm_arg1,
                               y_arg = nm_arg2)
        if (is_rv_1)
            n_draw <- n_draw(arg1)
        else
            n_draw <- n_draw(arg2)
        if (is_rv_1)
            arg1 <- as.vector(as.matrix(arg1))
        else
            arg1 <- rep.int(arg1, times = n_draw)
        if (is_rv_2)
            arg2 <- as.vector(as.matrix(arg2))
        else
            arg2 <- rep.int(arg2, times = n_draw)
    }
    ans <- tryCatch(fun(arg1, arg2, ...),
                    error = function(e) e)
    if (inherits(ans, "error"))
        cli::cli_abort(c("Problem with call to function {.fun {nm_fun}}.",
                         i = ans$message))
    if (is_rv) {
        ans <- matrix(ans, ncol = n_draw)
        ans <- rvec(ans)
    }
    ans
}


## HAS_TESTS
#' Apply random-distribution function to an rvec:
#' function has three parameter
#'
#' Assume that 'arg1', 'arg2', 'arg3' have
#' already been recycled, if necessary,
#' to have the required lengths.
#'
#' Note that in practie random variate
#' functions are the
#' only distribution functions to
#' have one parameter.
#'
#' If the functin is a random variate
#' function, then the 'n' argument
#' is  passed in via ....
#' The calling function
#' is responsible for setting 'n'
#' to 'length(as.matrix(arg))'.
#'
#' @param fun The function to be applied
#' @param arg1,arg2,arg3 Parameter arguments for the function
#' @param ... Other arguments passed to fun
#'
#' @returns If arg1, arg2, or arg3 is an rvec,
#' then an rvec. Otherwise a numeric vector.
#'
#' @noRd
dist_rvec_3 <- function(fun, arg1, arg2, arg3, ...) {
    nm_fun <- rlang::as_name(rlang::enquo(fun))
    nm_arg1 <- rlang::as_name(rlang::enquo(arg1))
    nm_arg2 <- rlang::as_name(rlang::enquo(arg2))
    nm_arg3 <- rlang::as_name(rlang::enquo(arg3))
    is_rv_1 <- is_rvec(arg1)
    is_rv_2 <- is_rvec(arg2)
    is_rv_3 <- is_rvec(arg3)
    is_rv <- is_rv_1 || is_rv_2 || is_rv_3
    if (is_rv) {
        if (is_rv_1 && is_rv_2)
            check_n_draw_equal(x = arg1,
                               y = arg2,
                               x_arg = nm_arg1,
                               y_arg = nm_arg2)
        if (is_rv_1 && is_rv_3)
            check_n_draw_equal(x = arg1,
                               y = arg3,
                               x_arg = nm_arg1,
                               y_arg = nm_arg3)
        if (is_rv_2 && is_rv_3)
            check_n_draw_equal(x = arg2,
                               y = arg3,
                               x_arg = nm_arg2,
                               y_arg = nm_arg3)
        if (is_rv_1)
            n_draw <- n_draw(arg1)
        else if (is_rv_2)
            n_draw <- n_draw(arg2)
        else
            n_draw <- n_draw(arg3)
        if (is_rv_1)
            arg1 <- as.vector(as.matrix(arg1))
        else
            arg1 <- rep.int(arg1, times = n_draw)
        if (is_rv_2)
            arg2 <- as.vector(as.matrix(arg2))
        else
            arg2 <- rep.int(arg2, times = n_draw)
        if (is_rv_3)
            arg3 <- as.vector(as.matrix(arg3))
        else
            arg3 <- rep.int(arg3, times = n_draw)
    }
    ans <- tryCatch(fun(arg1, arg2, arg3, ...),
                    error = function(e) e)
    if (inherits(ans, "error"))
        cli::cli_abort(c("Problem with call to function {.fun {nm_fun}}.",
                         i = ans$message))
    if (is_rv) {
        ans <- matrix(ans, ncol = n_draw)
        ans <- rvec(ans)
    }
    ans
}




