
#' Probability distributions
#'
#' Modified versions of standard probability
#' functions that can accommodate rvecs.
#' If any of arguments to the functions are
#' rvecs, then the functions return rvecs;
#' otherwise they return ordinary R vectors,
#  (with one exception, described below.)
#'
#' @section Creating an rvec by supplying a value for n_draw:
#'
#' The `r*_rvec` functions for generating random variates
#' include one argument that is not present in
#' base R functions: `n_draw`. If a value for `n_draw`
#' is supplied, then the return value is always an
#' rvec, even if none of the inputs are rvecs.
#' This is a convenient way to create
#' an rvec to use in a simulation. See below for an example.
#'
#' @section Recycling:
#' 
#' Unlike the base R distribution functions,
#' the rvec functions use [tidyverse][vctrs::vector_recycling_rules]
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
#' @section Multinomial:
#'
#' The multinomial distribution is the only
#' distribution described here that  has a
#' multivariate outcome. Base R and rvec
#' multinomial distribution functions both
#' behave differently from other distribution
#' functions:
#'
#' | Base R                           | rvec                                       |
#' |:---------------------------------|:-------------------------------------------|
#' | No `pmultinom() or `qmultinom()` | No `pmultinom_rvec() or `qmultinom_rvec()` |
#' | No recycling of arguments        | No recycling of arguments                  |
#' | `rmultinom()` returns matrix     | `rmultinom_rvec()` returns list if `n > 1` |
#'
#' @param df,df1,df2 Degrees of freedom. 
#' See [stats::dchisq()], [stats::df()], [stats::dt()].
#' Can be rvec.
#' @param k Number of balls drawn from urn.
#' See [stats::dhyper()]. Can be rvec.
#' @param lambda Vector of means.
#' See [stats::rpois()] Can be rvec.
#' @param location Parameter for Cauchy distribution.
#' Default is `0`. See [stats::dcauchy()]. Can be rvec.
#' @param log,log.p Whether to return
#' `log(p)` rather than `p`. Default is
#' `FALSE`. Can be rvec.
#' @param lower.tail Whether to return
#' \eqn{P[X \le x]}, as opposed to
#' \eqn{P[X > x]}. Default is `TRUE`.
#' Cannot be rvec. 
#' @param m The number of white balls in the urn.
#' See [stats::dhyper()]. Can be rvec. 
#' @param mean Mean of distribution. 
#' Default is `0`.  See [stats::dlnorm()].
#' Can be rvec.
#' @param meanlog Mean of distribution, on log scale.
#' Default is `0`. See [stats::dlnorm()].
#' Can be rvec.
#' @param min,max Lower and upper limits of
#' the uniform distribution. Defaults are
#' `0` and `1`.  See [stats::dunif()].
#' Can be rvec.
#' @param mu Mean for negative binomial distribution.
#' See [stats::dnbinom()]. Can be rvec.
#' @param n
#' - In functions other than `rhyper_rvec()`, `n` is the
#' length of random vector being created, and cannot be
#' an rvec.
#' - In `rhyper_rvec()`, `n` is the number of black balls
#' in the urn, and can be an rvec. See [stats::rhyper()].
#' @param nn The length of the random vector being created,
#' in a call to `rhyper_rvec()`. The equivalent of `n` in
#' other random variate functions. 
#' See [stats::rhyper()]. Cannot be an rvec.
#' @param n_draw Number of random draws, per observation,
#' in random vector being created. 
#' Optional. Cannot be rvec.
#' @param ncp Non-centrality parameter. 
#' Default is `0`. See [stats::dbeta()],
#' [stats::dchisq()], [stats::df()],
#' [stats::dt()]. Cannot be rvec.
#' @param p Probabilities. Can be rvec.
#' @param prob Probability of
#' success in each trial.
#' See [stats::dgeom()], [stats::dnbinom()].
#' Can be rvec.
#' @param q Quantiles. Can be rvec.
#' @param rate Rates. See [stats::dexp()],
#' [stats::dgamma()]. Can be rvec.
#' @param scale Parameter
#' for Cauchy distribution. Default is `1`.
#' See [stats::dcauchy()]. Can be rvec.
#' @param sd Standard deviation. 
#' Default is `1`. See [stats::dnorm()].
#' Can be rvec.
#' @param sdlog Standard deviation of distribution,
#' on log scale.
#' Default is `1`. See [stats::dlnorm()].
#' Can be rvec.
#' @param shape Parameter for gamma distribution.
#' See [stats::dgamma()], [stats::dweibull()].
#' Can be rvec.
#' @param shape1,shape2 Parameters
#' for beta distribution. Non-negative. 
#' See [stats::dbeta()]. Can be rvecs.
#' @param size Number of trials.
#' See [stats::dbinom()], [stats::dmultinom()],
#' [stats::dnbinom()]. Can be rvec.
#' @param x Quantiles. Can be rvec.
#'
#' @returns
#' If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]; otherwise an ordinary R vector.
#'
#' @seealso
#' - Equivalent functions in base R: [stats::distributions].
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
#'
#' ## create rvecs via the 'n_draw' argument,
#' ## and use to created a simulated 'y'
#' mu <- rnorm_rvec(n = 3,
#'                  mean = 2,
#'                  sd = 0.5,
#'                  n_draw = 1000)
#' sigma <- rgamma_rvec(n = 3,
#'                      shape = 1,
#'                      scale = 0.5,
#'                      n_draw = 1000)
#' y <- rnorm_rvec(n = 3,
#'                 mean = mu,
#'                 sd = sigma)
#' y
#'
#' ## multinomial distribution
#' size <- rvec(list(10:12))
#' prob <- c(0.1, 0.4, 0.2, 0.3)
#' x <- rmultinom_rvec(n = 1, size = size, prob = prob)
#' x
#' sum(x)
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
    ncp_not_supplied <- missing(ncp)
    check_nonneg_num_vector(ncp)
    check_flag(log)
    dbeta <- stats::dbeta
    args <- vec_recycle_common(x, shape1, shape2, ncp)
    x <- args[[1]]
    shape1 <- args[[2]]
    shape2 <- args[[3]]
    ncp <- args[[4L]]
    if (ncp_not_supplied)
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
    ncp_not_supplied <- missing(ncp)
    check_nonneg_num_vector(ncp)
    check_flag(lower.tail)
    check_flag(log.p)
    pbeta <- stats::pbeta
    args <- vec_recycle_common(q, shape1, shape2, ncp)
    q <- args[[1]]
    shape1 <- args[[2]]
    shape2 <- args[[3]]
    ncp <- args[[4L]]
    if (ncp_not_supplied)
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
    ncp_not_supplied <- missing(ncp)
    check_nonneg_num_vector(ncp)
    check_flag(lower.tail)
    check_flag(log.p)
    qbeta <- stats::qbeta
    args <- vec_recycle_common(p, shape1, shape2, ncp)
    p <- args[[1L]]
    shape1 <- args[[2L]]
    shape2 <- args[[3L]]
    ncp <- args[[4L]]
    if (ncp_not_supplied)
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
rbeta_rvec <- function(n, shape1, shape2, ncp = 0, n_draw = NULL) {
    ncp_not_supplied <- missing(ncp)
    check_nonneg_num_vector(ncp)
    rbeta <- stats::rbeta
    shape1 <- vec_recycle(shape1, size = n)
    shape2 <- vec_recycle(shape2, size = n)
    ncp <- vec_recycle(ncp, size = n)
    args <- list(shape1 = shape1, shape2 = shape2)
    if (!is.null(n_draw))
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
    n <- n_rdist(n = n, args = args)
    shape1 <- args[["shape1"]]
    shape2 <- args[["shape2"]]
    if (ncp_not_supplied)
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
rbinom_rvec <- function(n, size, prob, n_draw = NULL) {
    rbinom <- stats::rbinom
    size <- vec_recycle(size, size = n)
    prob <- vec_recycle(prob, size = n)
    args <- list(size = size, prob = prob)
    if (!is.null(n_draw))
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
    n <- n_rdist(n = n, args = args)
    size <- args[["size"]]
    prob <- args[["prob"]]
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
rcauchy_rvec <- function(n, location = 0, scale = 1, n_draw = NULL) {
    rcauchy <- stats::rcauchy
    location <- vec_recycle(location, size = n)
    scale <- vec_recycle(scale, size = n)
    args <- list(location = location,
                 scale = scale)
    if (!is.null(n_draw))
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
    n <- n_rdist(n = n, args = args)
    location <- args[["location"]]
    scale <- args[["scale"]]
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
    ncp_not_supplied <- missing(ncp)
    check_nonneg_num_vector(ncp)
    check_flag(log)
    dchisq <- stats::dchisq
    args <- vec_recycle_common(x, df, ncp)
    x <- args[[1L]]
    df <- args[[2L]]
    ncp <- args[[3L]]
    if (ncp_not_supplied)
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
    ncp_not_supplied <- missing(ncp)
    check_nonneg_num_vector(ncp)
    check_flag(lower.tail)
    check_flag(log.p)
    pchisq <- stats::pchisq
    args <- vec_recycle_common(q, df, ncp)
    q <- args[[1L]]
    df <- args[[2L]]
    ncp <- args[[3L]]
    if (ncp_not_supplied)
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
    ncp_not_supplied <- missing(ncp)
    check_nonneg_num_vector(ncp)
    check_flag(lower.tail)
    check_flag(log.p)
    qchisq <- stats::qchisq
    args <- vec_recycle_common(p, df, ncp)
    p <- args[[1L]]
    df <- args[[2L]]
    ncp <- args[[3L]]
    if (ncp_not_supplied)
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
rchisq_rvec <- function(n, df, ncp = 0, n_draw = NULL) {
    ncp_not_supplied <- missing(ncp)
    check_nonneg_num_vector(ncp)
    rchisq <- stats::rchisq
    df <- vec_recycle(df, size = n)
    ncp <- vec_recycle(ncp, size = n)
    args <- list(df = df)
    if (!is.null(n_draw))
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
    n <- n_rdist(n = n, args = args)
    df <- args[["df"]]
    if (ncp_not_supplied)
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
rexp_rvec <- function(n, rate = 1, n_draw = NULL) {
    rexp <- stats::rexp
    rate <- vec_recycle(rate, size = n)
    args <- list(rate = rate)
    if (!is.null(n_draw))
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
    n <- n_rdist(n = n, args = args)
    rate <- args[["rate"]]
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
    ncp_not_supplied <- missing(ncp)
    check_nonneg_num_vector(ncp)
    check_flag(log)
    df <- stats::df
    args <- vec_recycle_common(x, df1, df2, ncp)
    x <- args[[1L]]
    df1 <- args[[2L]]
    df2 <- args[[3L]]
    ncp <- args[[4L]]
    if (ncp_not_supplied)
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
    ncp_not_supplied <- missing(ncp)
    check_nonneg_num_vector(ncp)
    check_flag(lower.tail)
    check_flag(log.p)
    pf <- stats::pf
    args <- vec_recycle_common(q, df1, df2, ncp)
    q <- args[[1L]]
    df1 <- args[[2L]]
    df2 <- args[[3L]]
    ncp <- args[[4L]]
    if (ncp_not_supplied)
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
    ncp_not_supplied <- missing(ncp)
    check_nonneg_num_vector(ncp)
    check_flag(lower.tail)
    check_flag(log.p)
    qf <- stats::qf
    args <- vec_recycle_common(p, df1, df2, ncp)
    p <- args[[1L]]
    df1 <- args[[2L]]
    df2 <- args[[3L]]
    ncp <- args[[4L]]
    if (ncp_not_supplied)
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
rf_rvec <- function(n, df1, df2, ncp = 0, n_draw = NULL) {
    ncp_not_supplied <- missing(ncp)
    check_nonneg_num_vector(ncp)
    rf <- stats::rf
    df1 <- vec_recycle(df1, size = n)
    df2 <- vec_recycle(df2, size = n)
    ncp <- vec_recycle(ncp, size = n)
    args <- list(df1 = df1, df2 = df2)
    if (!is.null(n_draw))
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
    n <- n_rdist(n = n, args = args)
    df1 <- args[["df1"]]
    df2 <- args[["df2"]]
    if (ncp_not_supplied)
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


## 'gamma' --------------------------------------------------------------------

## Use 'rate' rather than 'scale' in call to 'dist_rvec_3'
## because 'rate' appears first in base R gamma functions

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
dgamma_rvec <- function(x, shape, rate = 1, scale = 1/rate, log = FALSE) {
    has_rate <- !missing(rate)
    has_scale <- !missing(scale)
    if (has_rate && has_scale)
        cli::cli_abort("Value supplied for {.arg rate} ({rate}) and for {.arg scale} ({scale})")
    if (!has_rate && has_scale)
        rate <- 1 / scale
    check_flag(log)
    dgamma <- stats::dgamma
    args <- vec_recycle_common(x, shape, rate)
    x <- args[[1]]
    shape <- args[[2]]
    rate <- args[[3]]
    dist_rvec_3(fun = dgamma,
                arg1 = x,
                arg2 = shape,
                arg3 = rate,
                log = log)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
pgamma_rvec <- function(q, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, log.p = FALSE) {
    has_rate <- !missing(rate)
    has_scale <- !missing(scale)
    if (has_rate && has_scale)
        cli::cli_abort("Value supplied for {.arg rate} ({rate}) and for {.arg scale} ({scale})")
    if (!has_rate && has_scale)
        rate <- 1 / scale
    check_flag(lower.tail)
    check_flag(log.p)
    pgamma <- stats::pgamma
    args <- vec_recycle_common(q, shape, rate)
    q <- args[[1]]
    shape <- args[[2]]
    rate <- args[[3]]
    dist_rvec_3(fun = pgamma,
                arg1 = q,
                arg2 = shape,
                arg3 = rate,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
qgamma_rvec <- function(p, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, log.p = FALSE) {
    has_rate <- !missing(rate)
    has_scale <- !missing(scale)
    if (has_rate && has_scale)
        cli::cli_abort("Value supplied for {.arg rate} ({rate}) and for {.arg scale} ({scale})")
    if (!has_rate && has_scale)
        rate <- 1 / scale
    check_flag(lower.tail)
    check_flag(log.p)
    qgamma <- stats::qgamma
    args <- vec_recycle_common(p, shape, rate)
    p <- args[[1L]]
    shape <- args[[2L]]
    rate <- args[[3L]]
    dist_rvec_3(fun = qgamma,
                arg1 = p,
                arg2 = shape,
                arg3 = rate,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
rgamma_rvec <- function(n, shape, rate = 1, scale = 1/rate, n_draw = NULL) {
    has_rate <- !missing(rate)
    has_scale <- !missing(scale)
    if (has_rate && has_scale)
        cli::cli_abort("Value supplied for {.arg rate} ({rate}) and for {.arg scale} ({scale})")
    if (!has_rate && has_scale)
        rate <- 1 / scale
    rgamma <- stats::rgamma
    shape <- vec_recycle(shape, size = n)
    rate <- vec_recycle(rate, size = n)
    args <- list(shape = shape, rate = rate)
    if (!is.null(n_draw))
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
    n <- n_rdist(n = n, args = args)
    shape <- args[["shape"]]
    rate <- args[["rate"]]
    dist_rvec_2(fun = rgamma,
                arg1 = shape,
                arg2 = rate,
                n = n)
}


## 'geom' ---------------------------------------------------------------------

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
dgeom_rvec <- function(x, prob, log = FALSE) {
    check_flag(log)
    dgeom <- stats::dgeom
    args <- vec_recycle_common(x, prob)
    x <- args[[1L]]
    prob <- args[[2L]]
    dist_rvec_2(fun = dgeom,
                arg1 = x,
                arg2 = prob,
                log = log)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
pgeom_rvec <- function(q, prob, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    pgeom <- stats::pgeom
    args <- vec_recycle_common(q, prob)
    q <- args[[1L]]
    prob <- args[[2L]]
    dist_rvec_2(fun = pgeom,
                arg1 = q,
                arg2 = prob,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
qgeom_rvec <- function(p, prob, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    qgeom <- stats::qgeom
    args <- vec_recycle_common(p, prob)
    p <- args[[1L]]
    prob <- args[[2L]]
    dist_rvec_2(fun = qgeom,
                arg1 = p,
                arg2 = prob,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
rgeom_rvec <- function(n, prob, n_draw = NULL) {
    rgeom <- stats::rgeom
    prob <- vec_recycle(prob, size = n)
    args <- list(prob = prob)
    if (!is.null(n_draw))
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
    n <- n_rdist(n = n, args = args)
    prob <- args[["prob"]]
    dist_rvec_1(fun = rgeom,
                arg = prob,
                n = n)
}


## 'hyper' --------------------------------------------------------------------

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
dhyper_rvec <- function(x, m, n, k, log = FALSE) {
    check_flag(log)
    dhyper <- stats::dhyper
    args <- vec_recycle_common(x, m, n, k)
    x <- args[[1]]
    m <- args[[2]]
    n <- args[[3]]
    k <- args[[4]]
    dist_rvec_4(fun = dhyper,
                arg1 = x,
                arg2 = m,
                arg3 = n,
                arg4 = k,
                log = log)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
phyper_rvec <- function(q, m, n, k, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    phyper <- stats::phyper
    args <- vec_recycle_common(q, m, n, k)
    q <- args[[1]]
    m <- args[[2]]
    n <- args[[3]]
    k <- args[[4]]
    dist_rvec_4(fun = phyper,
                arg1 = q,
                arg2 = m,
                arg3 = n,
                arg4 = k,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
qhyper_rvec <- function(p, m, n, k, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    qhyper <- stats::qhyper
    args <- vec_recycle_common(p, m, n, k)
    p <- args[[1L]]
    m <- args[[2L]]
    n <- args[[3L]]
    k <- args[[4L]]
    dist_rvec_4(fun = qhyper,
                arg1 = p,
                arg2 = m,
                arg3 = n,
                arg4 = k,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
rhyper_rvec <- function(nn, m, n, k, n_draw = NULL) {
    rhyper <- stats::rhyper
    m <- vec_recycle(m, size = nn)
    n <- vec_recycle(n, size = nn)
    k <- vec_recycle(k, size = nn)
    args <- list(m = m, n = n, k = k)
    if (!is.null(n_draw))
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
    nn <- n_rdist(n = nn, args = args)
    m <- args[["m"]]
    n <- args[["n"]]
    k <- args[["k"]]
    dist_rvec_3(fun = rhyper,
                arg1 = m,
                arg2 = n,
                arg3 = k,
                nn = nn)
}


## 'lnorm' ---------------------------------------------------------------------

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
dlnorm_rvec <- function(x, meanlog = 0, sdlog = 1, log = FALSE) {
    check_flag(log)
    dlnorm <- stats::dlnorm
    args <- vec_recycle_common(x, meanlog, sdlog)
    x <- args[[1]]
    meanlog <- args[[2]]
    sdlog <- args[[3]]
    dist_rvec_3(fun = dlnorm,
                arg1 = x,
                arg2 = meanlog,
                arg3 = sdlog,
                log = log)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
plnorm_rvec <- function(q, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    plnorm <- stats::plnorm
    args <- vec_recycle_common(q, meanlog, sdlog)
    q <- args[[1]]
    meanlog <- args[[2]]
    sdlog <- args[[3]]
    dist_rvec_3(fun = plnorm,
                arg1 = q,
                arg2 = meanlog,
                arg3 = sdlog,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
qlnorm_rvec <- function(p, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    qlnorm <- stats::qlnorm
    args <- vec_recycle_common(p, meanlog, sdlog)
    p <- args[[1L]]
    meanlog <- args[[2L]]
    sdlog <- args[[3L]]
    dist_rvec_3(fun = qlnorm,
                arg1 = p,
                arg2 = meanlog,
                arg3 = sdlog,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
rlnorm_rvec <- function(n, meanlog = 0, sdlog = 1, n_draw = NULL) {
    rlnorm <- stats::rlnorm
    meanlog <- vec_recycle(meanlog, size = n)
    sdlog <- vec_recycle(sdlog, size = n)
    args <- list(meanlog = meanlog, sdlog = sdlog)
    if (!is.null(n_draw))
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
    n <- n_rdist(n = n, args = args)
    meanlog <- args[["meanlog"]]
    sdlog <- args[["sdlog"]]
    dist_rvec_2(fun = rlnorm,
                arg1 = meanlog,
                arg2 = sdlog,
                n = n)
}


## 'multinom' -----------------------------------------------------------------

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
dmultinom_rvec <- function(x, size = NULL, prob, log = FALSE) {
    check_flag(log)
    dmultinom <- stats::dmultinom
    if (is.null(size))
        size <- sum(x)
    n_x <- length(x)
    n_p <- length(prob)
    if (n_x == 0L)
        cli::cli_abort("{.arg x} has length 0.")
    if (n_x != n_p)
        cli::cli_abort(c("{.arg x} and {.arg p} have different lengths.",
                         i = "{.arg x} has length {n_x}.",
                         i = "{.arg p} has length {n_p}."))
    if (length(size) != 1L)
        cli::cli_abort(c("{.arg size} does not have length 1.",
                         i = "{.arg size} has length {length(size)}."))
    is_rv_x <- is_rvec(x)
    is_rv_s <- is_rvec(size)
    is_rv_p <- is_rvec(prob)
    is_rv <- is_rv_x || is_rv_s || is_rv_p
    if (is_rv) {
        if (is_rv_x && !is_rv_s)
            cli::cli_abort(c("{.arg x} is an rvec, but {.arg size} is not.",
                             "{.arg size} has class {.cls {class(size)}}."))
        if (!is_rv_x && is_rv_s)
            cli::cli_abort(c("{.arg size} is an rvec, but {.arg x} is not.",
                             "{.arg x} has class {.cls {class(x)}}."))
        if (is_rv_x) {
            n_draw_xs <- n_draw_common(x = x,
                                       y = size,
                                       x_arg = "x",
                                       y_arg = "size")
            if (is_rv_p) {
                n_draw_xp <- n_draw_common(x = x,
                                           y = prob,
                                           x_arg = "x",
                                           y_arg = "prob")
                n_draw_sp <- n_draw_common(x = size,
                                           y = prob,
                                           x_arg = "size",
                                           y_arg = "prob")
                n_draw <- max(n_draw_xs, n_draw_xp, n_draw_sp)
            }
            else
                n_draw <- n_draw_xs
        }
        else
            n_draw <- n_draw(prob)
        if (is_rv_x) {
            check_not_rvec_chr(x, nm_arg = "x")
            x <- rvec_to_rvec_dbl(x, n_draw = n_draw)
            x <- as.matrix(x)
        }
        else
            x <- matrix(x, nrow = n_x, ncol = n_draw)
        if (is_rv_s) {
            check_not_rvec_chr(size, nm_arg = "size")
            size <- rvec_to_rvec_dbl(size, n_draw = n_draw)
            size <- as.vector(as.matrix(size))
        }
        else
            size <- rep.int(size, times = n_draw)
        if (is_rv_p) {
            check_not_rvec_chr(prob, nm_arg = "prob")
            prob <- rvec_to_rvec_dbl(prob, n_draw = n_draw)
            prob <- as.matrix(prob)
        }
        else
            prob <- matrix(prob, nrow = n_p, ncol = n_draw)
    }
    else {
        n_draw <- 1L
        x <- matrix(x, ncol = 1L)
        prob <- matrix(prob, ncol = 1L)
    }
    ans <- double(length = n_draw)
    for (i_draw in seq_len(n_draw)) {
        val <- tryCatch(dmultinom(x = x[, i_draw],
                                  size = size[[i_draw]],
                                  prob = prob[, i_draw],
                                  log = log),
                        error = function(e) e)
        if (inherits(val, "error"))
            cli::cli_abort(c("Problem with call to function {.fun dmultinom}:",
                             i = val$message))
        ans[[i_draw]] <- val
    }
    if (is_rv) {
        ans <- matrix(ans, nrow = 1L)
        ans <- rvec(ans)
    }
    ans
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
rmultinom_rvec <- function(n, size, prob, n_draw = NULL) {
    check_n(n)
    if (length(size) != 1L)
        cli::cli_abort("{.arg size} does not have length 1.")
    n_p <- length(prob)
    if (n_p == 0L)
        cli::cli_abort("{.arg prob} has length 0.")
    rmultinom <- stats::rmultinom
    if (is.null(n_draw)) {
        is_rv_s <- is_rvec(size)
        is_rv_p <- is_rvec(prob)
        if (is_rv_s && is_rv_p)
            n_draw <- n_draw_common(x = size,
                                    y = prob,
                                    x_arg = "size",
                                    y_arg = "prob")
        else if (!is_rv_s && is_rv_p)
            n_draw <- n_draw(prob)
        else if (is_rv_s && !is_rv_p)
            n_draw <- n_draw(size)
        else
            n_draw <- 1L
    }
    else {
        args <- list(size = size, prob = prob)
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
        size <- args[["size"]]
        prob <- args[["prob"]]
        is_rv_s <- TRUE
        is_rv_p <- TRUE
    }
    if (is_rv_s) {
        check_not_rvec_chr(size, nm_arg = "size")
        size <- rvec_to_rvec_dbl(size, n_draw = n_draw)
        size <- as.vector(as.matrix(size))
    }
    else
        size <- rep.int(size, times = n_draw)
    if (is_rv_p) {
        check_not_rvec_chr(prob, nm_arg = "prob")
        prob <- rvec_to_rvec_dbl(prob, n_draw = n_draw)
        prob <- as.matrix(prob)
    }
    else
        prob <- matrix(prob, nrow = n_p, ncol = n_draw)
    ans <- vector(mode = "list", length = n)
    for (i_ans in seq_along(ans)) {
        m <- matrix(nrow = n_p, ncol = n_draw)
        for (i_draw in seq_len(n_draw)) {
            val <- tryCatch(rmultinom(n = 1L,
                                      size = size[[i_draw]],
                                      prob = prob[, i_draw]),
                            error = function(e) e)
            if (inherits(val, "error"))
                cli::cli_abort(c("Problem with call to function {.fun rmultinom}:",
                                 i = val$message))
            m[, i_draw] <- val
        }
        ans[[i_ans]] <- m
    }
    if (is_rv_s || is_rv_p)
        ans <- lapply(ans, rvec)
    if (n == 1L)
        ans <- ans[[1L]]
    ans
}


## 'nbinom' -------------------------------------------------------------------

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
dnbinom_rvec <- function(x, size, prob, mu, log = FALSE) {
    check_flag(log)
    dnbinom <- stats::dnbinom
    has_prob <- !missing(prob)
    has_mu <- !missing(mu)
    if (has_prob && has_mu)
        cli::cli_abort("Values supplied for {.arg prob} and for {.arg mu}")
    if (!has_prob && !has_mu)
        cli::cli_abort("No value supplied for {.arg prob} or for {.arg mu}")
    if (has_prob) {
        args <- vec_recycle_common(x, size, prob)
        x <- args[[1]]
        size <- args[[2]]
        prob <- args[[3]]
    }
    else {
        args <- vec_recycle_common(x, size, mu)
        x <- args[[1]]
        size <- args[[2]]
        mu <- args[[3]]
        prob <- size / (size + mu)
    }
    dist_rvec_3(fun = dnbinom,
                arg1 = x,
                arg2 = size,
                arg3 = prob,
                log = log)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
pnbinom_rvec <- function(q, size, prob, mu, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    pnbinom <- stats::pnbinom
    has_prob <- !missing(prob)
    has_mu <- !missing(mu)
    if (has_prob && has_mu)
        cli::cli_abort("Values supplied for {.arg prob} and for {.arg mu}")
    if (!has_prob && !has_mu)
        cli::cli_abort("No value supplied for {.arg prob} or for {.arg mu}")
    if (has_prob) {
        args <- vec_recycle_common(q, size, prob)
        q <- args[[1]]
        size <- args[[2]]
        prob <- args[[3]]
    }
    else {
        args <- vec_recycle_common(q, size, mu)
        q <- args[[1]]
        size <- args[[2]]
        mu <- args[[3]]
        prob <- size / (size + mu)
    }
    dist_rvec_3(fun = pnbinom,
                arg1 = q,
                arg2 = size,
                arg3 = prob,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
qnbinom_rvec <- function(p, size, prob, mu, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    qnbinom <- stats::qnbinom
    has_prob <- !missing(prob)
    has_mu <- !missing(mu)
    if (has_prob && has_mu)
        cli::cli_abort("Values supplied for {.arg prob} and for {.arg mu}")
    if (!has_prob && !has_mu)
        cli::cli_abort("No value supplied for {.arg prob} or for {.arg mu}")
    if (has_prob) {
        args <- vec_recycle_common(p, size, prob)
        p <- args[[1]]
        size <- args[[2]]
        prob <- args[[3]]
    }
    else {
        args <- vec_recycle_common(p, size, mu)
        p <- args[[1]]
        size <- args[[2]]
        mu <- args[[3]]
        prob <- size / (size + mu)
    }
    dist_rvec_3(fun = qnbinom,
                arg1 = p,
                arg2 = size,
                arg3 = prob,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
rnbinom_rvec <- function(n, size, prob, mu, n_draw = NULL) {
    rnbinom <- stats::rnbinom
    has_prob <- !missing(prob)
    has_mu <- !missing(mu)
    if (has_prob && has_mu)
        cli::cli_abort("Values supplied for {.arg prob} and for {.arg mu}")
    if (!has_prob && !has_mu)
        cli::cli_abort("No value supplied for {.arg prob} or for {.arg mu}")
    size <- vec_recycle(size, size = n)
    if (has_prob) {
        prob <- vec_recycle(prob, size = n)
        args <- list(size = size, prob = prob)
    }
    else {
        mu <- vec_recycle(mu, size = n)
        args <- list(size = size, mu = mu)
    }
    if (!is.null(n_draw))
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
    n <- n_rdist(n = n, args = args)
    size <- args[["size"]]
    if (has_prob)
        prob <- args[["prob"]]
    else {
        mu <- args[["mu"]]
        prob <- size / (size + mu)
    }
    dist_rvec_2(fun = rnbinom,
                arg1 = size,
                arg2 = prob,
                n = n)
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
rnorm_rvec <- function(n, mean = 0, sd = 1, n_draw = NULL) {
    rnorm <- stats::rnorm
    mean <- vec_recycle(mean, size = n)
    sd <- vec_recycle(sd, size = n)
    args <- list(mean = mean, sd = sd)
    if (!is.null(n_draw))
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
    n <- n_rdist(n = n, args = args)
    mean <- args[["mean"]]
    sd <- args[["sd"]]
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
rpois_rvec <- function(n, lambda, n_draw = NULL) {
    rpois <- stats::rpois
    lambda <- vec_recycle(lambda, size = n)
    args <- list(lambda = lambda)
    if (!is.null(n_draw))
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
    n <- n_rdist(n = n, args = args)
    lambda <- args[["lambda"]]
    dist_rvec_1(fun = rpois,
                arg = lambda,
                n = n)
}


## 't' ------------------------------------------------------------------------

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
dt_rvec <- function(x, df, ncp = 0, log = FALSE) {
    ncp_not_supplied <- missing(ncp)
    check_nonneg_num_vector(ncp)
    check_flag(log)
    dt <- stats::dt
    args <- vec_recycle_common(x, df, ncp)
    x <- args[[1L]]
    df <- args[[2L]]
    ncp <- args[[3L]]
    if (ncp_not_supplied)
        dist_rvec_2(fun = dt,
                    arg1 = x,
                    arg2 = df,
                    log = log)
    else
        dist_rvec_2(fun = dt,
                    arg1 = x,
                    arg2 = df,
                    ncp = ncp,
                    log = log)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
pt_rvec <- function(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE) {
    ncp_not_supplied <- missing(ncp)
    check_nonneg_num_vector(ncp)
    check_flag(lower.tail)
    check_flag(log.p)
    pt <- stats::pt
    args <- vec_recycle_common(q, df, ncp)
    q <- args[[1L]]
    df <- args[[2L]]
    ncp <- args[[3L]]
    if (ncp_not_supplied)
        dist_rvec_2(fun = pt,
                    arg1 = q,
                    arg2 = df,
                    lower.tail = lower.tail,
                    log.p = log.p)
    else
        dist_rvec_2(fun = pt,
                    arg1 = q,
                    arg2 = df,
                    ncp = ncp,
                    lower.tail = lower.tail,
                    log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
qt_rvec <- function(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE) {
    ncp_not_supplied <- missing(ncp)
    check_nonneg_num_vector(ncp)
    check_flag(lower.tail)
    check_flag(log.p)
    qt <- stats::qt
    args <- vec_recycle_common(p, df, ncp)
    p <- args[[1L]]
    df <- args[[2L]]
    ncp <- args[[3L]]
    if (ncp_not_supplied)
        dist_rvec_2(fun = qt,
                    arg1 = p,
                    arg2 = df,
                    lower.tail = lower.tail,
                    log.p = log.p)
    else
        dist_rvec_2(fun = qt,
                    arg1 = p,
                    arg2 = df,
                    ncp = ncp,
                    lower.tail = lower.tail,
                    log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
rt_rvec <- function(n, df, ncp = 0, n_draw = NULL) {
    ncp_not_supplied <- missing(ncp)
    check_nonneg_num_vector(ncp)
    rt <- stats::rt
    df <- vec_recycle(df, size = n)
    ncp <- vec_recycle(ncp, size = n)
    args <- list(df = df)
    if (!is.null(n_draw))
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
    n <- n_rdist(n = n, args = args)
    df <- args[["df"]]
    if (ncp_not_supplied)
        dist_rvec_1(fun = rt,
                    arg = df,
                    n = n)
    else
        dist_rvec_1(fun = rt,
                    arg = df,
                    ncp = ncp,
                    n = n)
}


## 'unif' ---------------------------------------------------------------------

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
dunif_rvec <- function(x, min = 0, max = 1, log = FALSE) {
    check_flag(log)
    dunif <- stats::dunif
    args <- vec_recycle_common(x, min, max)
    x <- args[[1]]
    min <- args[[2]]
    max <- args[[3]]
    dist_rvec_3(fun = dunif,
                arg1 = x,
                arg2 = min,
                arg3 = max,
                log = log)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
punif_rvec <- function(q, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    punif <- stats::punif
    args <- vec_recycle_common(q, min, max)
    q <- args[[1]]
    min <- args[[2]]
    max <- args[[3]]
    dist_rvec_3(fun = punif,
                arg1 = q,
                arg2 = min,
                arg3 = max,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
qunif_rvec <- function(p, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    qunif <- stats::qunif
    args <- vec_recycle_common(p, min, max)
    p <- args[[1L]]
    min <- args[[2L]]
    max <- args[[3L]]
    dist_rvec_3(fun = qunif,
                arg1 = p,
                arg2 = min,
                arg3 = max,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
runif_rvec <- function(n, min = 0, max = 1, n_draw = NULL) {
    runif <- stats::runif
    min <- vec_recycle(min, size = n)
    max <- vec_recycle(max, size = n)
    args <- list(min = min, max = max)
    if (!is.null(n_draw))
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
    n <- n_rdist(n = n, args = args)
    min <- args[["min"]]
    max <- args[["max"]]
    dist_rvec_2(fun = runif,
                arg1 = min,
                arg2 = max,
                n = n)
}



## 'weibull' ------------------------------------------------------------------

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
dweibull_rvec <- function(x, shape, scale = 1, log = FALSE) {
    check_flag(log)
    dweibull <- stats::dweibull
    args <- vec_recycle_common(x, shape, scale)
    x <- args[[1]]
    shape <- args[[2]]
    scale <- args[[3]]
    dist_rvec_3(fun = dweibull,
                arg1 = x,
                arg2 = shape,
                arg3 = scale,
                log = log)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
pweibull_rvec <- function(q, shape, scale = 1, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    pweibull <- stats::pweibull
    args <- vec_recycle_common(q, shape, scale)
    q <- args[[1]]
    shape <- args[[2]]
    scale <- args[[3]]
    dist_rvec_3(fun = pweibull,
                arg1 = q,
                arg2 = shape,
                arg3 = scale,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
qweibull_rvec <- function(p, shape, scale = 1, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    qweibull <- stats::qweibull
    args <- vec_recycle_common(p, shape, scale)
    p <- args[[1L]]
    shape <- args[[2L]]
    scale <- args[[3L]]
    dist_rvec_3(fun = qweibull,
                arg1 = p,
                arg2 = shape,
                arg3 = scale,
                lower.tail = lower.tail,
                log.p = log.p)
}

## HAS_TESTS
#' @rdname rvec-distributions
#' @export
rweibull_rvec <- function(n, shape, scale = 1, n_draw = NULL) {
    rweibull <- stats::rweibull
    shape <- vec_recycle(shape, size = n)
    scale <- vec_recycle(scale, size = n)
    args <- list(shape = shape, scale = scale)
    if (!is.null(n_draw))
        args <- promote_args_to_rvec(args = args,
                                     n_draw = n_draw)
    n <- n_rdist(n = n, args = args)
    shape <- args[["shape"]]
    scale <- args[["scale"]]
    dist_rvec_2(fun = rweibull,
                arg1 = shape,
                arg2 = scale,
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
#' If the function is a random variate
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
            n_draw <- n_draw_common(x = arg1,
                                    y = arg2,
                                    x_arg = nm_arg1,
                                    y_arg = nm_arg2)
        else if (is_rv_1 && !is_rv_2)
            n_draw <- n_draw(arg1)
        else
            n_draw <- n_draw(arg2)
        if (is_rv_1) {
            check_not_rvec_chr(arg1, nm_arg = nm_arg1)
            arg1 <- rvec_to_rvec_dbl(x = arg1, n_draw = n_draw)
            arg1 <- as.vector(as.matrix(arg1))
        }
        else
            arg1 <- rep.int(arg1, times = n_draw)
        if (is_rv_2) {
            check_not_rvec_chr(arg2, nm_arg = nm_arg2)
            arg2 <- rvec_to_rvec_dbl(x = arg2, n_draw = n_draw)
            arg2 <- as.vector(as.matrix(arg2))
        }
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
            n_draw_12 <- n_draw_common(x = arg1,
                                       y = arg2,
                                       x_arg = nm_arg1,
                                       y_arg = nm_arg2)
        if (is_rv_1 && is_rv_3)
            n_draw_13 <- n_draw_common(x = arg1,
                                       y = arg3,
                                       x_arg = nm_arg1,
                                       y_arg = nm_arg3)
        if (is_rv_2 && is_rv_3)
            n_draw_23 <- n_draw_common(x = arg2,
                                       y = arg3,
                                       x_arg = nm_arg2,
                                       y_arg = nm_arg3)
        case <- c(is_rv_1, is_rv_2, is_rv_3)
        if (identical(case, c(TRUE, TRUE, TRUE)))
            n_draw <- max(n_draw_12, n_draw_13, n_draw_23)
        else if (identical(case, c(FALSE, TRUE, TRUE)))
            n_draw <- n_draw_23
        else if (identical(case, c(TRUE, FALSE, TRUE)))
            n_draw <- n_draw_13
        else if (identical(case, c(FALSE, FALSE, TRUE)))
            n_draw <- n_draw(arg3)
        else if (identical(case, c(TRUE, TRUE, FALSE)))
            n_draw <- n_draw_12
        else if (identical(case, c(FALSE, TRUE, FALSE)))
            n_draw <- n_draw(arg2)
        else if (identical(case, c(TRUE, FALSE, FALSE)))
            n_draw <- n_draw(arg1)
        if (is_rv_1) {
            check_not_rvec_chr(arg1, nm_arg = nm_arg1)
            arg1 <- rvec_to_rvec_dbl(arg1, n_draw = n_draw)
            arg1 <- as.vector(as.matrix(arg1))
        }
        else
            arg1 <- rep.int(arg1, times = n_draw)
        if (is_rv_2) {
            check_not_rvec_chr(arg2, nm_arg = nm_arg2)
            arg2 <- rvec_to_rvec_dbl(arg2, n_draw = n_draw)
            arg2 <- as.vector(as.matrix(arg2))
        }
        else
            arg2 <- rep.int(arg2, times = n_draw)
        if (is_rv_3) {
            check_not_rvec_chr(arg3, nm_arg = nm_arg3)
            arg3 <- rvec_to_rvec_dbl(arg3, n_draw = n_draw)
            arg3 <- as.vector(as.matrix(arg3))
        }
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


## HAS_TESTS
#' Apply random-distribution function to an rvec:
#' function has four parameters
#'
#' Assume that 'arg1', 'arg2', 'arg3', 'arg4' have
#' already been recycled, if necessary,
#' to have the required lengths.
#'
#' If the functin is a random variate
#' function, then the 'n' argument
#' is  passed in via ....
#' The calling function
#' is responsible for setting 'n'
#' to 'length(as.matrix(arg))'.
#'
#' The logic of how to handle combinations of
#' ordinary vectors and rvecs is tricky,
#' so the function uses brute force,
#' going through case by case.
#'
#' @param fun The function to be applied
#' @param arg1,arg2,arg3,arg4 Parameter arguments for the function
#' @param ... Other arguments passed to fun
#'
#' @returns If arg1, arg2, arg3, or arg4 is an rvec,
#' then an rvec. Otherwise a numeric vector.
#'
#' @noRd
dist_rvec_4 <- function(fun, arg1, arg2, arg3, arg4, ...) {
    nm_fun <- rlang::as_name(rlang::enquo(fun))
    nm_arg1 <- rlang::as_name(rlang::enquo(arg1))
    nm_arg2 <- rlang::as_name(rlang::enquo(arg2))
    nm_arg3 <- rlang::as_name(rlang::enquo(arg3))
    nm_arg4 <- rlang::as_name(rlang::enquo(arg4))
    is_rv_1 <- is_rvec(arg1)
    is_rv_2 <- is_rvec(arg2)
    is_rv_3 <- is_rvec(arg3)
    is_rv_4 <- is_rvec(arg4)
    is_rv <- is_rv_1 || is_rv_2 || is_rv_3 || is_rv_4
    if (is_rv) {
        if (is_rv_1 && is_rv_2)
            n_draw_12 <- n_draw_common(x = arg1,
                                       y = arg2,
                                       x_arg = nm_arg1,
                                       y_arg = nm_arg2)
        if (is_rv_1 && is_rv_3)
            n_draw_13 <- n_draw_common(x = arg1,
                                       y = arg3,
                                       x_arg = nm_arg1,
                                       y_arg = nm_arg3)
        if (is_rv_1 && is_rv_4)
            n_draw_14 <- n_draw_common(x = arg1,
                                       y = arg4,
                                       x_arg = nm_arg1,
                                       y_arg = nm_arg4)
        if (is_rv_2 && is_rv_3)
            n_draw_23 <- n_draw_common(x = arg2,
                                       y = arg3,
                                       x_arg = nm_arg2,
                                       y_arg = nm_arg3)
        if (is_rv_2 && is_rv_4)
            n_draw_24 <- n_draw_common(x = arg2,
                                       y = arg4,
                                       x_arg = nm_arg2,
                                       y_arg = nm_arg4)
        if (is_rv_3 && is_rv_4)
            n_draw_34 <- n_draw_common(x = arg3,
                                       y = arg4,
                                       x_arg = nm_arg4,
                                       y_arg = nm_arg4)
        case <- c(is_rv_1, is_rv_2, is_rv_3, is_rv_4)
        if (identical(case, c(TRUE, TRUE, TRUE, TRUE)))
            n_draw <- max(n_draw_12, n_draw_13, n_draw_14,
                          n_draw_23, n_draw_24,
                          n_draw_34)
        else if (identical(case, c(FALSE, TRUE, TRUE, TRUE)))
            n_draw <- max(n_draw_23, n_draw_24, n_draw_34)
        else if (identical(case, c(TRUE, FALSE, TRUE, TRUE)))
            n_draw <- max(n_draw_13, n_draw_14, n_draw_34)
        else if (identical(case, c(FALSE, FALSE, TRUE, TRUE)))
            n_draw <- n_draw_34
        else if (identical(case, c(TRUE, TRUE, FALSE, TRUE)))
            n_draw <- max(n_draw_12, n_draw_14, n_draw_24)
        else if (identical(case, c(FALSE, TRUE, FALSE, TRUE)))
            n_draw <- n_draw_24
        else if (identical(case, c(TRUE, FALSE, FALSE, TRUE)))
            n_draw <- n_draw_14
        else if (identical(case, c(FALSE, FALSE, FALSE, TRUE)))
            n_draw <- n_draw(arg4)
        else if (identical(case, c(TRUE, TRUE, TRUE, FALSE)))
            n_draw <- max(n_draw_12, n_draw_13, n_draw_23)
        else if (identical(case, c(FALSE, TRUE, TRUE, FALSE)))
            n_draw <- n_draw_23
        else if (identical(case, c(TRUE, FALSE, TRUE, FALSE)))
            n_draw <- n_draw_13
        else if (identical(case, c(FALSE, FALSE, TRUE, FALSE)))
            n_draw <- n_draw(arg3)
        else if (identical(case, c(TRUE, TRUE, FALSE, FALSE)))
            n_draw <- n_draw_12
        else if (identical(case, c(FALSE, TRUE, FALSE, FALSE)))
            n_draw <- n_draw(arg2)
        else if (identical(case, c(TRUE, FALSE, FALSE, FALSE)))
            n_draw <- n_draw(arg1)
        else
            cli::cli_abort("Internal error: invalid combinations of rvecs")
        if (is_rv_1) {
            check_not_rvec_chr(arg1, nm_arg = nm_arg1)
            arg1 <- rvec_to_rvec_dbl(arg1, n_draw = n_draw)
            arg1 <- as.vector(as.matrix(arg1))
        }
        else
            arg1 <- rep.int(arg1, times = n_draw)
        if (is_rv_2) {
            check_not_rvec_chr(arg2, nm_arg = nm_arg2)
            arg2 <- rvec_to_rvec_dbl(arg2, n_draw = n_draw)
            arg2 <- as.vector(as.matrix(arg2))
        }
        else
            arg2 <- rep.int(arg2, times = n_draw)
        if (is_rv_3) {
            check_not_rvec_chr(arg3, nm_arg = nm_arg3)
            arg3 <- rvec_to_rvec_dbl(arg3, n_draw = n_draw)
            arg3 <- as.vector(as.matrix(arg3))
        }
        else
            arg3 <- rep.int(arg3, times = n_draw)
        if (is_rv_4) {
            check_not_rvec_chr(arg4, nm_arg = nm_arg4)
            arg4 <- rvec_to_rvec_dbl(arg4, n_draw = n_draw)
            arg4 <- as.vector(as.matrix(arg4))
        }
        else
            arg4 <- rep.int(arg4, times = n_draw)
    }
    ans <- tryCatch(fun(arg1, arg2, arg3, arg4, ...),
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


