
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
#' @param df,df1,df2 Degrees of freedom. Can be rvec.
#' @param lambda Vector of means. Can be rvec.
#' @param location Parameter for Cauchy distribution.
#' Can be rvec. Default is `0`.
#' @param mean Means of distribution. Can be rvec.
#' Default is `0`.
#' @param p Probabilities. Can be rvec.
#' @param prob Probability of
#' success in each trial. Can be rvec.
#' @param q Quantiles. Can be rvec.
#' @param rate Rates. Can be rvec.
#' @param scale Parameters
#' for Cauchy distribution.
#' Can be rvec. Default is `1`.
#' @param sd Standard deviations. Can be rvec.
#' Default is `1`.
#' @param shape1,shape2 Parameters
#' for beta distribution.
#' Non-negative. Can be rvecs.
#' @param size Number of trials. Can be rvec.
#' @param x Quantiles. Can be rvec.
#' @param n Number of draws. Cannot be rvec.
#' @param ncp Non-centrality parameter. Cannot be rvec.
#' Default is `0`.
#' @param log,log.p Whether to return
#' `log(p)` rather than `p`. Cannot be rvec. Default is
#' `FALSE`.
#' @param lower.tail Whether to return
#' \eqn{P[X \le x]}, as opposed to
#' \eqn{P[X > x]}. Cannot be rvec. Default is `TRUE`.
#' 
#'
#' @returns If any of the inputs is an
#' [rvec][rvec()], then an
#' an [rvec][rvec()]; otherwise an
#' ordinary R vector.
#'
#' @seealso Base R functions
#' - [stats::dbeta()] Beta distribution.
#' - [stats::dbinom()] Binomial distribution.
#' - [stats::dcauchy()] Cauchy distribution.
#' - [stats::dchisq()] Chi-squared distribution.
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


## 'beta' ---------------------------------------------------------------------

## The help for the base *beta functions notes that
## calling *beta with ncp = 0 can give a different
## results from calling *beta with ncp missing.

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
dbeta_rvec <- function(x, shape1, shape2, ncp = 0, log = FALSE) {
    check_nonneg_num_vector(ncp)
    check_flag(log)
    dbeta <- stats::dbeta
    args <- vec_recycle_common(x, shape1, shape2, ncp)
    x <- args[[1]]
    shape1 <- args[[2]]
    shape2 <- args[[3]]
    if (missing(ncp))
        dist_rvec_3(fun = dbeta,
                    arg1 = x,
                    arg2 = shape1,
                    arg3 = shape2,
                    log = log)
    else
        dist_rvec_3(fun = dbeta,
                    arg1 = x,
                    arg2 = shape1,
                    arg3 = shape2,
                    ncp = ncp,
                    log = log)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
pbeta_rvec <- function(q, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p = FALSE) {
    check_nonneg_num_vector(ncp)
    check_flag(lower.tail)
    check_flag(log.p)
    pbeta <- stats::pbeta
    args <- vec_recycle_common(q, shape1, shape2, ncp)
    q <- args[[1]]
    shape1 <- args[[2]]
    shape2 <- args[[3]]
    if (missing(ncp))
        dist_rvec_3(fun = pbeta,
                    arg1 = q,
                    arg2 = shape1,
                    arg3 = shape2,
                    lower.tail = lower.tail,
                    log.p = log.p)
    else
        dist_rvec_3(fun = pbeta,
                    arg1 = q,
                    arg2 = shape1,
                    arg3 = shape2,
                    ncp = ncp,
                    lower.tail = lower.tail,
                    log.p = log.p)        
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
qbeta_rvec <- function(p, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p = FALSE) {
    check_nonneg_num_vector(ncp)
    check_flag(lower.tail)
    check_flag(log.p)
    qbeta <- stats::qbeta
    args <- vec_recycle_common(p, shape1, shape2, ncp)
    p <- args[[1L]]
    shape1 <- args[[2L]]
    shape2 <- args[[3L]]
    if (missing(ncp))
        dist_rvec_3(fun = qbeta,
                    arg1 = p,
                    arg2 = shape1,
                    arg3 = shape2,
                    lower.tail = lower.tail,
                    log.p = log.p)
    else
        dist_rvec_3(fun = qbeta,
                    arg1 = p,
                    arg2 = shape1,
                    arg3 = shape2,
                    ncp = ncp,
                    lower.tail = lower.tail,
                    log.p = log.p)        
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
rbeta_rvec <- function(n, shape1, shape2, ncp = 0) {
    check_nonneg_num_vector(ncp)
    rbeta <- stats::rbeta
    shape1 <- vec_recycle(shape1, size = n)
    shape2 <- vec_recycle(shape2, size = n)
    ncp <- vec_recycle(ncp, size = n)
    n <- n_rdist(n = n, args = list(shape1, shape2))
    if (missing(ncp))
        dist_rvec_2(fun = rbeta,
                    arg1 = shape1,
                    arg2 = shape2,
                    n = n)
    else
        dist_rvec_2(fun = rbeta,
                    arg1 = shape1,
                    arg2 = shape2,
                    n = n,
                    ncp = ncp)
}


## 'binom' ---------------------------------------------------------------------

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
dbinom_rvec <- function(x, size, prob, log = FALSE) {
    check_flag(log)
    dbinom <- stats::dbinom
    args <- vec_recycle_common(x, size, prob)
    x <- args[[1]]
    size <- args[[2]]
    prob <- args[[3]]
    dist_rvec_3(fun = dbinom,
                arg1 = x,
                arg2 = size,
                arg3 = prob,
                log = log)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
pbinom_rvec <- function(q, size, prob, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    pbinom <- stats::pbinom
    args <- vec_recycle_common(q, size, prob)
    q <- args[[1]]
    size <- args[[2]]
    prob <- args[[3]]
    dist_rvec_3(fun = pbinom,
                arg1 = q,
                arg2 = size,
                arg3 = prob,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
qbinom_rvec <- function(p, size, prob, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    qbinom <- stats::qbinom
    args <- vec_recycle_common(p, size, prob)
    p <- args[[1L]]
    size <- args[[2L]]
    prob <- args[[3L]]
    dist_rvec_3(fun = qbinom,
                arg1 = p,
                arg2 = size,
                arg3 = prob,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
rbinom_rvec <- function(n, size, prob) {
    rbinom <- stats::rbinom
    size <- vec_recycle(size, size = n)
    prob <- vec_recycle(prob, size = n)
    n <- n_rdist(n = n, args = list(size, prob))
    dist_rvec_2(fun = rbinom,
                arg1 = size,
                arg2 = prob,
                n = n)
}


## 'cauchy' ---------------------------------------------------------------------

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
dcauchy_rvec <- function(x, location = 0, scale = 1, log = FALSE) {
    check_flag(log)
    dcauchy <- stats::dcauchy
    args <- vec_recycle_common(x, location, scale)
    x <- args[[1]]
    location <- args[[2]]
    scale <- args[[3]]
    dist_rvec_3(fun = dcauchy,
                arg1 = x,
                arg2 = location,
                arg3 = scale,
                log = log)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
pcauchy_rvec <- function(q, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    pcauchy <- stats::pcauchy
    args <- vec_recycle_common(q, location, scale)
    q <- args[[1]]
    location <- args[[2]]
    scale <- args[[3]]
    dist_rvec_3(fun = pcauchy,
                arg1 = q,
                arg2 = location,
                arg3 = scale,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
qcauchy_rvec <- function(p, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    qcauchy <- stats::qcauchy
    args <- vec_recycle_common(p, location, scale)
    p <- args[[1L]]
    location <- args[[2L]]
    scale <- args[[3L]]
    dist_rvec_3(fun = qcauchy,
                arg1 = p,
                arg2 = location,
                arg3 = scale,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
rcauchy_rvec <- function(n, location = 0, scale = 1) {
    rcauchy <- stats::rcauchy
    location <- vec_recycle(location, size = n)
    scale <- vec_recycle(scale, size = n)
    n <- n_rdist(n = n, args = list(location, scale))
    dist_rvec_2(fun = rcauchy,
                arg1 = location,
                arg2 = scale,
                n = n)
}


## 'chisq' ---------------------------------------------------------------------

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
dchisq_rvec <- function(x, df, ncp = 0, log = FALSE) {
    check_nonneg_num_vector(ncp)
    check_flag(log)
    dchisq <- stats::dchisq
    args <- vec_recycle_common(x, df, ncp)
    x <- args[[1L]]
    df <- args[[2L]]
    if (missing(ncp))
        dist_rvec_2(fun = dchisq,
                    arg1 = x,
                    arg2 = df,
                    log = log)
    else
        dist_rvec_2(fun = dchisq,
                    arg1 = x,
                    arg2 = df,
                    ncp = ncp,
                    log = log)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
pchisq_rvec <- function(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE) {
    check_nonneg_num_vector(ncp)
    check_flag(lower.tail)
    check_flag(log.p)
    pchisq <- stats::pchisq
    args <- vec_recycle_common(q, df, ncp)
    q <- args[[1L]]
    df <- args[[2L]]
    if (missing(ncp))
        dist_rvec_2(fun = pchisq,
                    arg1 = q,
                    arg2 = df,
                    lower.tail = lower.tail,
                    log.p = log.p)
    else
        dist_rvec_2(fun = pchisq,
                    arg1 = q,
                    arg2 = df,
                    ncp = ncp,
                    lower.tail = lower.tail,
                    log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
qchisq_rvec <- function(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE) {
    check_nonneg_num_vector(ncp)
    check_flag(lower.tail)
    check_flag(log.p)
    qchisq <- stats::qchisq
    args <- vec_recycle_common(p, df, ncp)
    p <- args[[1L]]
    df <- args[[2L]]
    if (missing(ncp))
        dist_rvec_2(fun = qchisq,
                    arg1 = p,
                    arg2 = df,
                    lower.tail = lower.tail,
                    log.p = log.p)
    else
        dist_rvec_2(fun = qchisq,
                    arg1 = p,
                    arg2 = df,
                    ncp = ncp,
                    lower.tail = lower.tail,
                    log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
rchisq_rvec <- function(n, df, ncp = 0) {
    check_nonneg_num_vector(ncp)
    rchisq <- stats::rchisq
    df <- vec_recycle(df, size = n)
    ncp <- vec_recycle(ncp, size = n)
    n <- n_rdist(n = n, args = list(df, ncp))
    if (missing(ncp))
        dist_rvec_1(fun = rchisq,
                    arg = df,
                    n = n)
    else
        dist_rvec_1(fun = rchisq,
                    arg = df,
                    ncp = ncp,
                    n = n)
}


## 'exp' ----------------------------------------------------------------------

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
dexp_rvec <- function(x, rate = 1, log = FALSE) {
    check_flag(log)
    dexp <- stats::dexp
    args <- vec_recycle_common(x, rate)
    x <- args[[1L]]
    rate <- args[[2L]]
    dist_rvec_2(fun = dexp,
                arg1 = x,
                arg2 = rate,
                log = log)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
pexp_rvec <- function(q, rate = 1, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    pexp <- stats::pexp
    args <- vec_recycle_common(q, rate)
    q <- args[[1L]]
    rate <- args[[2L]]
    dist_rvec_2(fun = pexp,
                arg1 = q,
                arg2 = rate,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
qexp_rvec <- function(p, rate = 1, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    qexp <- stats::qexp
    args <- vec_recycle_common(p, rate)
    p <- args[[1L]]
    rate <- args[[2L]]
    dist_rvec_2(fun = qexp,
                arg1 = p,
                arg2 = rate,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
rexp_rvec <- function(n, rate = 1) {
    rexp <- stats::rexp
    rate <- vec_recycle(rate, size = n)
    n <- n_rdist(n = n, args = list(rate))
    dist_rvec_1(fun = rexp,
                arg = rate,
                n = n)
}


## 'f' ---------------------------------------------------------------------

## The help for the base *f functions notes that
## calling *f with ncp = 0 can give a different
## results from calling *f with ncp missing.

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
df_rvec <- function(x, df1, df2, ncp = 0, log = FALSE) {
    check_nonneg_num_vector(ncp)
    check_flag(log)
    df <- stats::df
    args <- vec_recycle_common(x, df1, df2, ncp)
    x <- args[[1]]
    df1 <- args[[2]]
    df2 <- args[[3]]
    if (missing(ncp))
        dist_rvec_3(fun = df,
                    arg1 = x,
                    arg2 = df1,
                    arg3 = df2,
                    log = log)
    else
        dist_rvec_3(fun = df,
                    arg1 = x,
                    arg2 = df1,
                    arg3 = df2,
                    ncp = ncp,
                    log = log)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
pf_rvec <- function(q, df1, df2, ncp = 0, lower.tail = TRUE, log.p = FALSE) {
    check_nonneg_num_vector(ncp)
    check_flag(lower.tail)
    check_flag(log.p)
    pf <- stats::pf
    args <- vec_recycle_common(q, df1, df2, ncp)
    q <- args[[1]]
    df1 <- args[[2]]
    df2 <- args[[3]]
    if (missing(ncp))
        dist_rvec_3(fun = pf,
                    arg1 = q,
                    arg2 = df1,
                    arg3 = df2,
                    lower.tail = lower.tail,
                    log.p = log.p)
    else
        dist_rvec_3(fun = pf,
                    arg1 = q,
                    arg2 = df1,
                    arg3 = df2,
                    ncp = ncp,
                    lower.tail = lower.tail,
                    log.p = log.p)        
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
qf_rvec <- function(p, df1, df2, ncp = 0, lower.tail = TRUE, log.p = FALSE) {
    check_nonneg_num_vector(ncp)
    check_flag(lower.tail)
    check_flag(log.p)
    qf <- stats::qf
    args <- vec_recycle_common(p, df1, df2, ncp)
    p <- args[[1L]]
    df1 <- args[[2L]]
    df2 <- args[[3L]]
    if (missing(ncp))
        dist_rvec_3(fun = qf,
                    arg1 = p,
                    arg2 = df1,
                    arg3 = df2,
                    lower.tail = lower.tail,
                    log.p = log.p)
    else
        dist_rvec_3(fun = qf,
                    arg1 = p,
                    arg2 = df1,
                    arg3 = df2,
                    ncp = ncp,
                    lower.tail = lower.tail,
                    log.p = log.p)        
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
rf_rvec <- function(n, df1, df2, ncp = 0) {
    check_nonneg_num_vector(ncp)
    rf <- stats::rf
    df1 <- vec_recycle(df1, size = n)
    df2 <- vec_recycle(df2, size = n)
    ncp <- vec_recycle(ncp, size = n)
    n <- n_rdist(n = n, args = list(df1, df2))
    if (missing(ncp))
        dist_rvec_2(fun = rf,
                    arg1 = df1,
                    arg2 = df2,
                    n = n)
    else
        dist_rvec_2(fun = rf,
                    arg1 = df1,
                    arg2 = df2,
                    n = n,
                    ncp = ncp)
}





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




