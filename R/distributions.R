
## 'beta' ---------------------------------------------------------------------

## The help for the base *beta functions notes that
## calling *beta with ncp = 0 can give a different
## results from calling *beta with ncp missing.

## HAS_TESTS
#' The Beta Distribution, Using Multiple Draws
#'
#' Density, distribution function,
#' quantile function and random generation for the
#' Beta distribution, modified to work with
#' rvecs.
#'
#' Functions `dbeta_rvec()`, `pbeta_rvec()`,
#' `pbeta_rvec()` and `rbeta_rvec()` work like
#' base R functions [dbeta()], [pbeta()],
#' [qbeta()], and [rbeta()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `rbeta_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' `dbeta_rvec()`, `pbeta_rvec()`,
#' `pbeta_rvec()` and `rbeta_rvec()`
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' @param log,log.p Whether to return results
#' on a log scale. Default is
#' `FALSE`. Cannot be an rvec.
#' @param lower.tail Whether to return
#' \eqn{P[X \le x]}, as opposed to
#' \eqn{P[X > x]}. Default is `TRUE`.
#' Cannot be an rvec. 
#' @param n The length of random vector being
#' created. Cannot be an rvec.
#' @param n_draw Number of random draws
#' in the random vector being
#' created. Cannot be an rvec.
#' @param ncp Non-centrality parameter. 
#' Default is `0`. Cannot be an rvec.
#' @param p Probabilities. Can be an rvec.
#' @param q Quantiles. Can be an rvec.
#' @param shape1,shape2 Parameters
#' for beta distribution. Non-negative. 
#' See [stats::dbeta()]. Can be an rvecs.
#' @param x Quantiles. Can be an rvec.
#'
#' @returns
#' - If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]
#' - Otherwise an ordinary R vector.
#'
#' @seealso
#' - [dbeta()]
#' - [pbeta()]
#' - [qbeta()]
#' - [rbeta()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(0, 0.25),
#'                c(0.5, 0.99)))
#' dbeta_rvec(x, shape1 = 1, shape2 = 1)
#' pbeta_rvec(x, shape1 = 1, shape2 = 1)
#'
#' rbeta_rvec(n = 2,
#'            shape = 1:2,
#'            shape2 = 1,
#'            n_draw = 1000)
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
#' @rdname dbeta_rvec
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
#' @rdname dbeta_rvec
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
#' @rdname dbeta_rvec
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
#' The Binomial Distribution, Using Multiple Draws
#'
#' Density, distribution function,
#' quantile function and random generation for the
#' binomial distribution, modified to work with
#' rvecs.
#'
#' Functions `dbinom_rvec()`, `pbinom_rvec()`,
#' `pbinom_rvec()` and `rbinom_rvec()` work like
#' base R functions [dbinom()], [pbinom()],
#' [qbinom()], and [rbinom()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `rbinom_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' `dbinom_rvec()`, `pbinom_rvec()`,
#' `pbinom_rvec()` and `rbinom_rvec()`
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' @inheritParams dbeta_rvec
#' @param prob Probability of success in each trial.
#' See [stats::dbinom()]. Can be an rvec.
#' @param size Number of trials.
#' See [stats::dbinom()]. Can be an rvec.
#'
#' @returns
#' - If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]
#' - Otherwise an ordinary R vector.
#'
#' @seealso
#' - [dbinom()]
#' - [pbinom()]
#' - [qbinom()]
#' - [rbinom()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(3, 8),
#'                c(0, 2)))
#' dbinom_rvec(x, size = 8, prob = 0.3)
#' pbinom_rvec(x, size = 8, prob = 0.3)
#'
#' rbinom_rvec(n = 2,
#'             size = 10,
#'             prob = c(0.7, 0.3),
#'             n_draw = 1000)
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
#' @rdname dbinom_rvec
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
#' @rdname dbinom_rvec
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
#' @rdname dbinom_rvec
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
#' The Cauchy Distribution, Using Multiple Draws
#'
#' Density, distribution function,
#' quantile function and random generation for the
#' Cauchy distribution, modified to work with
#' rvecs.
#'
#' Functions `dcauchy_rvec()`, `pcauchy_rvec()`,
#' `pcauchy_rvec()` and `rcauchy_rvec()` work like
#' base R functions [dcauchy()], [pcauchy()],
#' [qcauchy()], and [rcauchy()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `rcauchy_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' `dcauchy_rvec()`, `pcauchy_rvec()`,
#' `pcauchy_rvec()` and `rcauchy_rvec()`
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' @inheritParams dbeta_rvec
#' @param location Center of distribution.
#' Default is `0`.
#' See [stats::dcauchy()]. Can be an rvec.
#' @param scale Scale parameter.
#' Default is `1`.
#' See [stats::dcauchy()]. Can be an rvec.
#'
#' @returns
#' - If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]
#' - Otherwise an ordinary R vector.
#'
#' @seealso
#' - [dcauchy()]
#' - [pcauchy()]
#' - [qcauchy()]
#' - [rcauchy()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(3, -5.1),
#'                c(0, -2.3)))
#' dcauchy_rvec(x)
#' pcauchy_rvec(x)
#'
#' rcauchy_rvec(n = 2,
#'              location = c(-5, 5),
#'              n_draw = 1000)
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
#' @rdname dcauchy_rvec
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
#' @rdname dcauchy_rvec
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
#' @rdname dcauchy_rvec
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
#' The Chi-Squared Distribution, Using Multiple Draws
#'
#' Density, distribution function,
#' quantile function and random generation for the
#' chi-squared distribution, modified to work with
#' rvecs.
#'
#' Functions `dchisq_rvec()`, `pchisq_rvec()`,
#' `pchisq_rvec()` and `rchisq_rvec()` work like
#' base R functions [dchisq()], [pchisq()],
#' [qchisq()], and [rchisq()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `rchisq_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' `dchisq_rvec()`, `pchisq_rvec()`,
#' `pchisq_rvec()` and `rchisq_rvec()`
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' @inheritParams dbeta_rvec
#' @param df Degrees of freedom. 
#' See [stats::dchisq()].
#' Can be an rvec.
#'
#' @returns
#' - If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]
#' - Otherwise an ordinary R vector.
#'
#' @seealso
#' - [dchisq()]
#' - [pchisq()]
#' - [qchisq()]
#' - [rchisq()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(3, 5.1),
#'                c(0.1, 2.3)))
#' dchisq_rvec(x, df = 3)
#' pchisq_rvec(x, df = 3)
#'
#' rchisq_rvec(n = 2,
#'             df = 3:4,
#'             n_draw = 1000)
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
#' @rdname dchisq_rvec
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
#' @rdname dchisq_rvec
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
#' @rdname dchisq_rvec
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
#' The Exponential Distribution, Using Multiple Draws
#'
#' Density, distribution function,
#' quantile function and random generation for the
#' exponential distribution, modified to work with
#' rvecs.
#'
#' Functions `dexp_rvec()`, `pexp_rvec()`,
#' `pexp_rvec()` and `rexp_rvec()` work like
#' base R functions [dexp()], [pexp()],
#' [qexp()], and [rexp()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `rexp_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' `dexp_rvec()`, `pexp_rvec()`,
#' `pexp_rvec()` and `rexp_rvec()`
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' @inheritParams dbeta_rvec
#' @param rate Vector of rates.
#' See [stats::dexp()].
#' Can be an rvec.
#'
#' @returns
#' - If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]
#' - Otherwise an ordinary R vector.
#'
#' @seealso
#' - [dexp()]
#' - [pexp()]
#' - [qexp()]
#' - [rexp()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(3, 5.1),
#'                c(0.1, 2.3)))
#' dexp_rvec(x, rate = 1.5)
#' pexp_rvec(x, rate = 1.5)
#'
#' rexp_rvec(n = 2,
#'           rate = c(1.5, 4),
#'           n_draw = 1000)
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
#' @rdname dexp_rvec
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
#' @rdname dexp_rvec
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
#' @rdname dexp_rvec
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
#' The F Distribution, Using Multiple Draws
#'
#' Density, distribution function,
#' quantile function and random generation for the
#' F distribution, modified to work with
#' rvecs.
#'
#' Functions `df_rvec()`, `pf_rvec()`,
#' `pf_rvec()` and `rf_rvec()` work like
#' base R functions [df()], [pf()],
#' [qf()], and [rf()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `rf_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' `df_rvec()`, `pf_rvec()`,
#' `pf_rvec()` and `rf_rvec()`
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' @inheritParams dbeta_rvec
#' @param df1,df2 Degrees of freedom. 
#' See [stats::df()]. Can be rvecs.
#'
#' @returns
#' - If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]
#' - Otherwise an ordinary R vector.
#'
#' @seealso
#' - [df()]
#' - [pf()]
#' - [qf()]
#' - [rf()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(3, 5.1),
#'                c(0.1, 2.3)))
#' df_rvec(x, df1 = 1, df2 = 3)
#' pf_rvec(x, df1 = 1, df2 = 3)
#'
#' rf_rvec(n = 2, df1 = 1,df2 = 2:3, n_draw = 1000)
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
#' @rdname df_rvec
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
#' @rdname df_rvec
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
#' @rdname df_rvec
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
#' The Gamma Distribution, Using Multiple Draws
#'
#' Density, distribution function,
#' quantile function and random generation for the
#' gamma distribution, modified to work with
#' rvecs.
#'
#' Functions `dgamma_rvec()`, `pgamma_rvec()`,
#' `pgamma_rvec()` and `rgamma_rvec()` work like
#' base R functions [dgamma()], [pgamma()],
#' [qgamma()], and [rgamma()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `rgamma_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' `dgamma_rvec()`, `pgamma_rvec()`,
#' `pgamma_rvec()` and `rgamma_rvec()`
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' @inheritParams dbeta_rvec
#' @param shape Shape parameter.
#' See [stats::dgamma()]. Can be an rvec.
#' @param rate Rate parameter. See [stats::dgamma()].
#' Can be an rvec.
#' @param scale Scale parameter.
#' An alterative to `rate`. See [stats::dgamma()].
#' Can be an rvec.
#'
#' @returns
#' - If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]
#' - Otherwise an ordinary R vector.
#'
#' @seealso
#' - [dgamma()]
#' - [pgamma()]
#' - [qgamma()]
#' - [rgamma()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(3, 5.1),
#'                c(0.1, 2.3)))
#' dgamma_rvec(x, shape = 1)
#' pgamma_rvec(x, shape = 1)
#'
#' rgamma_rvec(n = 2,
#'             shape = 1,
#'             rate = c(0.5, 1),
#'             n_draw = 1000)
#' @export
dgamma_rvec <- function(x, shape, rate = 1, scale = 1/rate, log = FALSE) {
    has_rate <- !missing(rate)
    has_scale <- !missing(scale)
    if (has_rate && has_scale)
        cli::cli_abort("Value supplied for {.arg rate} and for {.arg scale}.")
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
#' @rdname dgamma_rvec
#' @export
pgamma_rvec <- function(q, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, log.p = FALSE) {
    has_rate <- !missing(rate)
    has_scale <- !missing(scale)
    if (has_rate && has_scale)
        cli::cli_abort("Value supplied for {.arg rate} and for {.arg scale}.")
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
#' @rdname dgamma_rvec
#' @export
qgamma_rvec <- function(p, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, log.p = FALSE) {
    has_rate <- !missing(rate)
    has_scale <- !missing(scale)
    if (has_rate && has_scale)
        cli::cli_abort("Value supplied for {.arg rate} and for {.arg scale}.")
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
#' @rdname dgamma_rvec
#' @export
rgamma_rvec <- function(n, shape, rate = 1, scale = 1/rate, n_draw = NULL) {
    has_rate <- !missing(rate)
    has_scale <- !missing(scale)
    if (has_rate && has_scale)
        cli::cli_abort("Value supplied for {.arg rate} and for {.arg scale}.")
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
#' The Geometric Distribution, Using Multiple Draws
#'
#' Density, distribution function,
#' quantile function and random generation for the
#' geometric distribution, modified to work with
#' rvecs.
#'
#' Functions `dgeom_rvec()`, `pgeom_rvec()`,
#' `pgeom_rvec()` and `rgeom_rvec()` work like
#' base R functions [dgeom()], [pgeom()],
#' [qgeom()], and [rgeom()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `rgeom_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' `dgeom_rvec()`, `pgeom_rvec()`,
#' `pgeom_rvec()` and `rgeom_rvec()`
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' @inheritParams dbeta_rvec
#' @param prob Probability of
#' success in each trial.
#' See [stats::dgeom()].
#' Can be an rvec.
#'
#' @returns
#' - If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]
#' - Otherwise an ordinary R vector.
#'
#' @seealso
#' - [dgeom()]
#' - [pgeom()]
#' - [qgeom()]
#' - [rgeom()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(3, 5),
#'                c(0, 2)))
#' dgeom_rvec(x, prob = 0.3)
#' pgeom_rvec(x, prob = 0.3)
#'
#' rgeom_rvec(n = 2,
#'            prob = c(0.5, 0.8),
#'            n_draw = 1000)
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
#' @rdname dgeom_rvec
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
#' @rdname dgeom_rvec
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
#' @rdname dgeom_rvec
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
#' The Hypergeometric Distribution, Using Multiple Draws
#'
#' Density, distribution function,
#' quantile function and random generation for the
#' hypergeometric distribution, modified to work with
#' rvecs.
#'
#' Functions `dhyper_rvec()`, `phyper_rvec()`,
#' `phyper_rvec()` and `rhyper_rvec()` work like
#' base R functions [dhyper()], [phyper()],
#' [qhyper()], and [rhyper()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `rhyper_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' `dhyper_rvec()`, `phyper_rvec()`,
#' `phyper_rvec()` and `rhyper_rvec()`
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' @inheritParams dbeta_rvec
#' @param k Number of balls drawn from urn.
#' See [stats::dhyper()]. Can be an rvec.
#' @param m Number of white balls in the urn.
#' See [stats::dhyper()]. Can be an rvec. 
#' @param n Number of black balls
#' in the urn. See [stats::rhyper()].
#' Can be an rvec. 
#' @param nn The length of the random vector
#' being created. The equivalent of `n` in
#' other random variate functions. 
#' See [stats::rhyper()]. Cannot be an rvec.
#'
#' @returns
#' - If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]
#' - Otherwise an ordinary R vector.
#'
#' @seealso
#' - [dhyper()]
#' - [phyper()]
#' - [qhyper()]
#' - [rhyper()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(3, 5),
#'                c(0, 2)))
#' dhyper_rvec(x, m = 6, n = 6, k = 5)
#' phyper_rvec(x, m = 6, n = 6, k = 5)
#'
#' rhyper_rvec(nn = 2,
#'             k = c(3, 5),
#'             m = 6,
#'             n = 6,
#'             n_draw = 1000)
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
#' @rdname dhyper_rvec
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
#' @rdname dhyper_rvec
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
#' @rdname dhyper_rvec
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
#' The Log-Normal Distribution, Using Multiple Draws
#'
#' Density, distribution function,
#' quantile function and random generation for the
#' log-normal distribution, modified to work with
#' rvecs.
#'
#' Functions `dlnorm_rvec()`, `plnorm_rvec()`,
#' `plnorm_rvec()` and `rlnorm_rvec()` work like
#' base R functions [dlnorm()], [plnorm()],
#' [qlnorm()], and [rlnorm()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `rlnorm_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' `dlnorm_rvec()`, `plnorm_rvec()`,
#' `plnorm_rvec()` and `rlnorm_rvec()`
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' @inheritParams dbeta_rvec
#' @param meanlog Mean of distribution, on log scale.
#' Default is `0`. See [stats::dlnorm()].
#' Can be an rvec.
#' @param sdlog Standard deviation of distribution,
#' on log scale. Default is `1`. See [stats::dlnorm()].
#' Can be an rvec.
#'
#' @returns
#' - If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]
#' - Otherwise an ordinary R vector.
#'
#' @seealso
#' - [dlnorm()]
#' - [plnorm()]
#' - [qlnorm()]
#' - [rlnorm()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(3.1, 5.7),
#'                c(0.2, 2.3)))
#' dlnorm_rvec(x)
#' plnorm_rvec(x)
#'
#' rlnorm_rvec(n = 2,
#'             meanlog = c(1, 3),
#'             n_draw = 1000)
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
#' @rdname dlnorm_rvec
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
#' @rdname dlnorm_rvec
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
#' @rdname dlnorm_rvec
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
#' The Multinomial Distribution, Using Multiple Draws
#'
#' Density function random generation for the
#' multinomial distribution, modified to work with
#' rvecs.
#'
#' Functions `dmultinom_rvec()`and
#' `rmultinom_rvec()` work like
#' base R functions [dmultinom()]
#' and [rmultinom()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `rmultinom_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' Like the base R functions [dmultinom()]
#' and [rmultinom(), `dmultinom_rvec()` and
#' `rmultinom_rvec()` do not recycle their arguments.
#' 
#' @inheritParams dbeta_rvec
#' @param size Total number of trials.
#' See [stats::dmultinom()].
#' Can be an rvec.
#' @param prob Numeric non-negative vector,
#' giving the probability of each outcome.
#' Internally normalized to sum to 1.
#' See [stats::dmultinom()].
#' Can be an rvec.
#' @param log Whether to return
#' `log(p)` rather than `p`. Default is
#' `FALSE`. Cannot be an rvec.
#'
#' @returns
#' - `dmultinom()`
#'     - If any of the arguments are rvecs,
#'     or if a value for `n_draw` is supplied,
#'     then an [rvec][rvec()]
#'     - Otherwise an ordinary R vector.
#' - `rmultinom()`
#'     - If `n` is 1, an rvec or
#'     ordinary R vector.
#'     - If `n` is greater than 1, a list
#'     of rvecs or ordinary R vectors
#'
#' @seealso
#' - [dmultinom()]
#' - [rmultinom()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(1, 4, 0),
#'                c(1, 0, 0),
#'                c(1, 0, 0),
#'                c(1, 0, 4)))
#' prob <- c(1/4, 1/4, 1/4, 1/4)
#' dmultinom_rvec(x = x, prob = prob)
#' rmultinom_rvec(n = 1,
#'                size = 100,
#'                prob = c(0.1, 0.4, 0.2, 0.3),
#'                n_draw = 1000)
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
#' @rdname dmultinom_rvec
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
#' The Negative Binomial Distribution, Using Multiple Draws
#'
#' Density, distribution function,
#' quantile function and random generation for the
#' negative binomial distribution, modified to work with
#' rvecs.
#'
#' Functions `dnbinom_rvec()`, `pnbinom_rvec()`,
#' `pnbinom_rvec()` and `rnbinom_rvec()` work like
#' base R functions [dnbinom()], [pnbinom()],
#' [qnbinom()], and [rnbinom()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `rnbinom_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' `dnbinom_rvec()`, `pnbinom_rvec()`,
#' `pnbinom_rvec()` and `rnbinom_rvec()`
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' @inheritParams dbeta_rvec
#' @param mu Mean value. See [stats::dnbinom()].
#' Can be an rvec.
#' @param prob Probability of success in each trial.
#' See [stats::dnbinom()]. Can be an rvec.
#' @param size Number of trials.
#' See [stats::dnbinom()]. Can be an rvec.
#'
#' @returns
#' - If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]
#' - Otherwise an ordinary R vector.
#'
#' @seealso
#' - [dnbinom()]
#' - [pnbinom()]
#' - [qnbinom()]
#' - [rnbinom()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(3, 5),
#'                c(0, 2)))
#' dnbinom_rvec(x, size = 6, prob = 0.2)
#' pnbinom_rvec(x, size = 6, prob = 0.2)
#'
#' rnbinom_rvec(n = 2,
#'              size = 2,
#'              mu = c(4, 8),
#'              n_draw = 1000)
#' @export
dnbinom_rvec <- function(x, size, prob, mu, log = FALSE) {
    check_flag(log)
    dnbinom <- stats::dnbinom
    has_prob <- !missing(prob)
    has_mu <- !missing(mu)
    if (has_prob && has_mu)
        cli::cli_abort("Value supplied for {.arg prob} and for {.arg mu}.")
    if (!has_prob && !has_mu)
        cli::cli_abort("No value supplied for {.arg prob} or for {.arg mu}.")
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
#' @rdname dnbinom_rvec
#' @export
pnbinom_rvec <- function(q, size, prob, mu, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    pnbinom <- stats::pnbinom
    has_prob <- !missing(prob)
    has_mu <- !missing(mu)
    if (has_prob && has_mu)
        cli::cli_abort("Value supplied for {.arg prob} and for {.arg mu}.")
    if (!has_prob && !has_mu)
        cli::cli_abort("No value supplied for {.arg prob} or for {.arg mu}.")
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
#' @rdname dnbinom_rvec
#' @export
qnbinom_rvec <- function(p, size, prob, mu, lower.tail = TRUE, log.p = FALSE) {
    check_flag(lower.tail)
    check_flag(log.p)
    qnbinom <- stats::qnbinom
    has_prob <- !missing(prob)
    has_mu <- !missing(mu)
    if (has_prob && has_mu)
        cli::cli_abort("Value supplied for {.arg prob} and for {.arg mu}.")
    if (!has_prob && !has_mu)
        cli::cli_abort("No value supplied for {.arg prob} or for {.arg mu}.")
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
#' @rdname dnbinom_rvec
#' @export
rnbinom_rvec <- function(n, size, prob, mu, n_draw = NULL) {
    rnbinom <- stats::rnbinom
    has_prob <- !missing(prob)
    has_mu <- !missing(mu)
    if (has_prob && has_mu)
        cli::cli_abort("Value supplied for {.arg prob} and for {.arg mu}.")
    if (!has_prob && !has_mu)
        cli::cli_abort("No value supplied for {.arg prob} or for {.arg mu}.")
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
#' The Normal Distribution, Using Multiple Draws
#'
#' Density, distribution function,
#' quantile function and random generation for the
#' normal distribution, modified to work with
#' rvecs.
#'
#' Functions `dnorm_rvec()`, `pnorm_rvec()`,
#' `pnorm_rvec()` and `rnorm_rvec()` work like
#' base R functions [dnorm()], [pnorm()],
#' [qnorm()], and [rnorm()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `rnorm_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' `dnorm_rvec()`, `pnorm_rvec()`,
#' `pnorm_rvec()` and `rnorm_rvec()`
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' @inheritParams dbeta_rvec
#' @param mean Mean of distribution. 
#' Default is `0`.  See [stats::dnorm()].
#' Can be an rvec.
#' @param sd Standard deviation. 
#' Default is `1`. See [stats::dnorm()].
#' Can be an rvec.
#'
#' @returns
#' - If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]
#' - Otherwise an ordinary R vector.
#'
#' @seealso
#' - [dnorm()]
#' - [pnorm()]
#' - [qnorm()]
#' - [rnorm()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(3.1, -5.4),
#'                c(0.1, 2.3)))
#' dnorm_rvec(x)
#' pnorm_rvec(x)
#'
#' rnorm_rvec(n = 2,
#'            mean = c(-3, 3),
#'            sd = c(2, 4),
#'            n_draw = 1000)
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
#' @rdname dnorm_rvec
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
#' @rdname dnorm_rvec
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
#' @rdname dnorm_rvec
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
#' The Poisson Distribution, Using Multiple Draws
#'
#' Density, distribution function,
#' quantile function and random generation for the
#' Poisson distribution, modified to work with
#' rvecs.
#'
#' Functions `dpois_rvec()`, `ppois_rvec()`,
#' `ppois_rvec()` and `rpois_rvec()` work like
#' base R functions [dpois()], [ppois()],
#' [qpois()], and [rpois()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `rpois_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' `dpois_rvec()`, `ppois_rvec()`,
#' `ppois_rvec()` and `rpois_rvec()`
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' @inheritParams dbeta_rvec
#' @param lambda Vector of means.
#' See [stats::rpois()]. Can be an rvec.
#'
#' @returns
#' - If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]
#' - Otherwise an ordinary R vector.
#'
#' @seealso
#' - [dpois()]
#' - [ppois()]
#' - [qpois()]
#' - [rpois()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(3, 5),
#'                c(1, 2)))
#' dpois_rvec(x, lambda = 3)
#' ppois_rvec(x, lambda = 3)
#'
#' rpois_rvec(n = 2,
#'            lambda = c(5, 10),
#'            n_draw = 1000)
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
#' @rdname dpois_rvec
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
#' @rdname dpois_rvec
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
#' @rdname dpois_rvec
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
#' Student t Distribution, Using Multiple Draws
#'
#' Density, distribution function,
#' quantile function and random generation for the
#' t distribution, modified to work with
#' rvecs.
#'
#' Functions `dt_rvec()`, `pt_rvec()`,
#' `pt_rvec()` and `rt_rvec()` work like
#' base R functions [dt()], [pt()],
#' [qt()], and [rt()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `rt_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' `dt_rvec()`, `pt_rvec()`,
#' `pt_rvec()` and `rt_rvec()`
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' @inheritParams dbeta_rvec
#' @param df Degrees of freedom. 
#' See [stats::dt()].
#' Can be an rvec.
#' @param ncp Non-centrality parameter. 
#' Default is `0`. See [stats::dt()].
#' Cannot be an rvec.
#'
#' @returns
#' - If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]
#' - Otherwise an ordinary R vector.
#'
#' @seealso
#' - [dt()]
#' - [pt()]
#' - [qt()]
#' - [rt()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(-3.2, 5.3),
#'                c(-1.6, 2)))
#' dt_rvec(x, df = 4)
#' pt_rvec(x, df = 4)
#'
#' rt_rvec(n = 2,
#'         df = c(3, 5),
#'         n_draw = 1000)
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
#' @rdname dt_rvec
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
#' @rdname dt_rvec
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
#' @rdname dt_rvec
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
#' Uniform Distribution, Using Multiple Draws
#'
#' Density, distribution function,
#' quantile function and random generation for the
#' uniform distribution, modified to work with
#' rvecs.
#'
#' Functions `dunif_rvec()`, `punif_rvec()`,
#' `punif_rvec()` and `runif_rvec()` work like
#' base R functions [dt()], [pt()],
#' [qt()], and [rt()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `runif_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' `dunif_rvec()`, `punif_rvec()`,
#' `punif_rvec()` and `runif_rvec()`
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' @inheritParams dbeta_rvec
#' @param min Lower limits. Default is `0`.
#' See [stats::dunif()]. Can be an rvec.
#' @param max Upper limited. Default is `1`.
#' See [stats::dunif()]. Can be an rvec.
#'
#' @returns
#' - If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]
#' - Otherwise an ordinary R vector.
#'
#' @seealso
#' - [dunif()]
#' - [punif()]
#' - [qunif()]
#' - [runif()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(0.2, 0.5),
#'                c(0.6, 0.7)))
#' dunif_rvec(x)
#' punif_rvec(x)
#'
#' runif_rvec(n = 2,
#'            min = c(0, 0.5),
#'            n_draw = 1000)
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
#' @rdname dunif_rvec
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
#' @rdname dunif_rvec
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
#' @rdname dunif_rvec
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
#' Weibull Distribution, Using Multiple Draws
#'
#' Density, distribution function,
#' quantile function and random generation for the
#' Weibull distribution, modified to work with
#' rvecs.
#'
#' Functions `dweibull_rvec()`, `pweibull_rvec()`,
#' `pweibull_rvec()` and `rweibull_rvec()` work like
#' base R functions [dt()], [pt()],
#' [qt()], and [rt()], except that
#' they accept rvecs as inputs. If any
#' input is an rvec, then the output will be too.
#' Function `rweibull_rvec()` also returns an
#' rvec if a value for `n_draw` is supplied.
#'
#' `dweibull_rvec()`, `pweibull_rvec()`,
#' `pweibull_rvec()` and `rweibull_rvec()`
#' use [tidyverse][vctrs::vector_recycling_rules]
#' vector recycling rules:
#' - Vectors of length 1 are recycled
#' - All other vectors must have the same size
#'
#' @inheritParams dbeta_rvec
#' @param scale Scale parameter. See [stats::dweibull()]
#' Default is `1`. Can be an rvec.
#' @param shape Shape parameter. See [stats::dweibull()].
#' Can be an rvec.
#'
#' @returns
#' - If any of the arguments are rvecs,
#' or if a value for `n_draw` is supplied,
#' then an [rvec][rvec()]
#' - Otherwise an ordinary R vector.
#'
#' @seealso
#' - [dweibull()]
#' - [pweibull()]
#' - [qweibull()]
#' - [rweibull()]
#' - [stats::distributions].
#'
#' @examples
#' x <- rvec(list(c(3.2, 4.5),
#'                c(7.6, 0.7)))
#' dweibull_rvec(x, shape = 2)
#' pweibull_rvec(x, shape = 2)
#'
#' rweibull_rvec(n = 2,
#'               shape = c(2, 3),
#'               n_draw = 1000)
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
#' @rdname dweibull_rvec
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
#' @rdname dweibull_rvec
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
#' @rdname dweibull_rvec
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
#' If the function is a random variate
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
            cli::cli_abort("Internal error: invalid combinations of rvecs") # nocov
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


