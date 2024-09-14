
## 'beta' ---------------------------------------------------------------------

test_that("'dbeta_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    shape1 <- rvec(m)
    shape2 <- rvec(2 * m)
    ans_obtained <- dbeta_rvec(x = x, shape1 = shape1, shape2 = shape2, log = TRUE)
    ans_expected <- rvec(matrix(dbeta(x = x, shape1 = m, shape2 = 2 * m, log = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dbeta_rvec' works with valid input - ncp nonzero", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    shape1 <- rvec(m)
    shape2 <- rvec(2 * m)
    ans_obtained <- dbeta_rvec(x = x, shape1 = shape1, shape2 = shape2, ncp = 0.5, log = TRUE)
    ans_expected <- rvec(matrix(dbeta(x = x, shape1 = m, shape2 = 2 * m, ncp = 0.5, log = TRUE),
                                nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'pbeta_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    shape1 <- rvec(m)
    shape2 <- rvec(2 * m)
    ans_obtained <- pbeta_rvec(q, shape1, shape2, log.p = TRUE)
    ans_expected <- rvec(matrix(pbeta(q = q, shape1 = m, shape2 = 2 * m, log.p = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'pbeta_rvec' works with valid input - ncp nonzero", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    shape1 <- rvec(m)
    shape2 <- rvec(2 * m)
    ans_obtained <- pbeta_rvec(q, shape1, shape2, ncp = 0.5, log.p = TRUE)
    ans_expected <- rvec(matrix(pbeta(q = q, shape1 = m, shape2 = 2 * m, ncp = 0.5, log.p = TRUE),
                                nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qbeta_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)/6
    p <- rvec(m)
    shape1 <- rvec(m)
    shape2 <- 3
    ans_obtained <- qbeta_rvec(p, shape1, shape2)
    ans_expected <- rvec(matrix(qbeta(p = m, shape1 = m, shape2 = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qbeta_rvec' works with valid input - ncp nonzero", {
    m <- matrix(1:6, nr = 2)/6
    p <- rvec(m)
    shape1 <- rvec(m)
    shape2 <- 3
    ans_obtained <- qbeta_rvec(p, shape1, shape2, ncp = 0.5)
    ans_expected <- rvec(matrix(qbeta(p = m, shape1 = m, shape2 = 3, ncp = 0.5), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rbeta_rvec' works with valid input - n_draw is NULL", {
    m <- matrix(1:6, nr = 2)
    shape1 <- rvec(m)
    shape2 <- 3
    set.seed(0)
    ans_obtained <- rbeta_rvec(n = 2, shape1, shape2)
    set.seed(0)
    ans_expected <- rvec(matrix(rbeta(n = 6, shape1 = m, shape2 = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rbeta_rvec' works with valid input - n_draw is NULL, ncp nonzero", {
    m <- matrix(1:6, nr = 2)
    shape1 <- rvec(m)
    shape2 <- 3
    set.seed(0)
    ans_obtained <- rbeta_rvec(n = 2, shape1, shape2, ncp = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rbeta(n = 6, shape1 = m, shape2 = 3, ncp = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rbeta_rvec' works with valid input - n_draw specified", {
    shape1 <- 1:2
    shape2 <- 3
    set.seed(0)
    ans_obtained <- rbeta_rvec(n = 2, shape1, shape2, ncp = 3, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rbeta(n = 6, shape1 = 1:2, shape2 = 3, ncp = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


## 'binom' ---------------------------------------------------------------------

test_that("'dbinom_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    size <- rvec(m)
    prob <- rvec(m/7)
    ans_obtained <- dbinom_rvec(x = x, size = size, prob = prob, log = TRUE)
    ans_expected <- rvec(matrix(dbinom(x = x, size = m, prob = m/7, log = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'pbinom_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    size <- rvec(m)
    prob <- 0.5
    ans_obtained <- pbinom_rvec(q, size, prob, log.p = TRUE)
    ans_expected <- rvec(matrix(pbinom(q = q, size = m, prob = 0.5, log.p = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qbinom_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)/6
    p <- rvec(m)
    size <- rvec(m)
    prob <- rvec(0.1 * m)
    ans_obtained <- qbinom_rvec(p, size, prob)
    ans_expected <- rvec(matrix(qbinom(m, size = m, prob = 0.1 * m), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rbinom_rvec' works with valid input - n_draw is NULL", {
    m <- matrix(1:6, nr = 2)
    size <- rvec(m)
    prob <- 0.23
    set.seed(0)
    ans_obtained <- rbinom_rvec(n = 2, size, prob)
    set.seed(0)
    ans_expected <- rvec(matrix(rbinom(n = 6, size = m, prob = 0.23), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rbinom_rvec' works with valid input - n_draw is 3", {
    m <- matrix(1:6, nr = 2)
    size <- rvec(m)
    prob <- 0.23
    set.seed(0)
    ans_obtained <- rbinom_rvec(n = 2, size, prob, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rbinom(n = 6, size = m, prob = 0.23), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


## 'cauchy' -------------------------------------------------------------------

test_that("'dcauchy_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2) - 3
    x <- 2:1
    location <- rvec(m)
    scale <- rvec(m) + 3
    ans_obtained <- dcauchy_rvec(x = x, location = location, scale = scale, log = TRUE)
    ans_expected <- rvec(matrix(dcauchy(x = x, location = m,
                                        scale = m + 3, log = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'pcauchy_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2) / 10
    q <- 2:1
    location <- rvec(m)
    scale <- 5
    ans_obtained <- pcauchy_rvec(q, location, scale, log.p = TRUE)
    ans_expected <- rvec(matrix(pcauchy(q = q,
                                        location = m, scale = 5, log.p = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qcauchy_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    p <- rvec(m) / 7
    location <- rvec(m)
    scale <- rvec(abs(0.1 * m))
    ans_obtained <- qcauchy_rvec(p, location, scale)
    ans_expected <- rvec(matrix(qcauchy(p = m / 7, location = m,
                                        scale = abs(0.1 * m)), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rcauchy_rvec' works with valid input - n_draw is NULL", {
    m <- matrix(1:6, nr = 2)
    location <- rvec(m)
    scale <- 10
    set.seed(0)
    ans_obtained <- rcauchy_rvec(n = 2, location, scale)
    set.seed(0)
    ans_expected <- rvec(matrix(rcauchy(n = 6, location = m, scale = 10), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rcauchy_rvec' works with valid input - n_draw is non-NULL", {
    location <- 1:2
    scale <- 10
    set.seed(0)
    ans_obtained <- rcauchy_rvec(n = 2, location, scale, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rcauchy(n = 6, location = 1:2, scale = 10), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


## 'chisq' ---------------------------------------------------------------------

test_that("'dchisq_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    df <- rvec(m)
    x <- 2:1
    df <- rvec(m)
    ans_obtained <- dchisq_rvec(x, df)
    ans_expected <- rvec(matrix(dchisq(x = x, df = m, log = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dchisq_rvec' works with valid input - ncp supplied", {
    m <- matrix(1:6, nr = 2)
    df <- rvec(m)
    x <- 2:1
    df <- rvec(m)
    ans_obtained <- dchisq_rvec(x, df, ncp = 0.001)
    ans_expected <- rvec(matrix(dchisq(x = x, df = m, ncp = 0.001, log = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'pchisq_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    df <- rvec(m)
    q <- 2:1
    df <- rvec(m)
    ans_obtained <- pchisq_rvec(q, df, lower.tail = FALSE)
    ans_expected <- rvec(matrix(pchisq(q, df = m, lower.tail = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'pchisq_rvec' works with valid input - ncp supplied", {
    m <- matrix(1:6, nr = 2)
    df <- rvec(m)
    q <- 2:1
    df <- rvec(m)
    ans_obtained <- pchisq_rvec(q, df, lower.tail = FALSE, ncp = 0.3)
    ans_expected <- rvec(matrix(pchisq(q, df = m, lower.tail = FALSE, ncp = 0.3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qchisq_rvec' works with valid input", {
    m <- matrix(seq(0.1, 0.6, 0.1), nr = 2)
    p <- rvec(m)
    df <- c(2.1, 0.8)
    ans_obtained <- qchisq_rvec(p, df, lower.tail = FALSE)
    ans_expected <- rvec(matrix(qchisq(m, df = df, lower.tail = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qchisq_rvec' works with valid input - ncp supplied", {
    m <- matrix(seq(0.1, 0.6, 0.1), nr = 2)
    p <- rvec(m)
    df <- c(2.1, 0.8)
    ans_obtained <- qchisq_rvec(p, df, lower.tail = FALSE, ncp = 0.3)
    ans_expected <- rvec(matrix(qchisq(m, df = df, lower.tail = FALSE, ncp = 0.3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rchisq_rvec' works with valid input - n_draw is NULL", {
    m <- matrix(seq(2.1, 2.6, 0.1), nr = 2)
    df <- rvec(m)
    set.seed(0)
    ans_obtained <- rchisq_rvec(2, df)
    set.seed(0)
    ans_expected <- rvec(matrix(rchisq(6, df = m), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rchisq_rvec' works with valid input - n_draw supplied", {
    df <- 3:4
    set.seed(0)
    ans_obtained <- rchisq_rvec(2, df, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rchisq(6, df = df), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rchisq_rvec' works with valid input - n_draw supplied, ncp supplied", {
    df <- 3:4
    set.seed(0)
    ans_obtained <- rchisq_rvec(2, df, n_draw = 3, ncp = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rchisq(6, df = df, ncp = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


## 'exp' ----------------------------------------------------------------------

test_that("'dexp_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    rate <- rvec(m)
    x <- 2:1
    rate <- rvec(m)
    ans_obtained <- dexp_rvec(x, rate)
    ans_expected <- rvec(matrix(dexp(x = x, rate = m, log = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'pexp_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    rate <- rvec(m)
    q <- 2:1
    rate <- rvec(m)
    ans_obtained <- pexp_rvec(q, rate, lower.tail = FALSE)
    ans_expected <- rvec(matrix(pexp(q, rate = m, lower.tail = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qexp_rvec' works with valid input", {
    m <- matrix(seq(0.1, 0.6, 0.1), nr = 2)
    p <- rvec(m)
    rate <- c(2.1, 0.8)
    ans_obtained <- qexp_rvec(p, rate, lower.tail = FALSE)
    ans_expected <- rvec(matrix(qexp(m, rate = rate, lower.tail = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rexp_rvec' works with valid input - n_draw is NULL", {
    m <- matrix(seq(2.1, 2.6, 0.1), nr = 2)
    rate <- rvec(m)
    set.seed(0)
    ans_obtained <- rexp_rvec(2, rate)
    set.seed(0)
    ans_expected <- rvec(matrix(rexp(6, rate = m), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rexp_rvec' works with valid input - n_draw is supplied", {
    m <- matrix(seq(2.1, 2.6, 0.1), nr = 2)
    rate <- rvec(m)
    set.seed(0)
    ans_obtained <- rexp_rvec(2, rate, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rexp(6, rate = m), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


## 'f' ---------------------------------------------------------------------

test_that("'df_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    df1 <- rvec(m)
    df2 <- rvec(2 * m)
    ans_obtained <- df_rvec(x = x, df1 = df1, df2 = df2, log = TRUE)
    ans_expected <- rvec(matrix(df(x = x, df1 = m, df2 = 2 * m, log = TRUE), nr = 2))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'df_rvec' works with valid input - ncp supplied", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    df1 <- rvec(m)
    df2 <- rvec(2 * m)
    ans_obtained <- df_rvec(x = x, df1 = df1, df2 = df2, ncp = 0.5, log = TRUE)
    ans_expected <- rvec(matrix(df(x = x, df1 = m, df2 = 2 * m, ncp = 0.5, log = TRUE), nr = 2))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'pf_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    df1 <- rvec(m)
    df2 <- rvec(2 * m)
    ans_obtained <- pf_rvec(q, df1, df2, log.p = TRUE)
    ans_expected <- rvec(matrix(pf(q = q, df1 = m, df2 = 2 * m, log.p = TRUE), nr = 2))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'pf_rvec' works with valid input, ncp supplied", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    df1 <- rvec(m)
    df2 <- rvec(2 * m)
    ans_obtained <- pf_rvec(q, df1, df2, ncp = 1, log.p = TRUE)
    ans_expected <- rvec(matrix(pf(q = q, df1 = m, df2 = 2 * m, ncp = 1, log.p = TRUE), nr = 2))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qf_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)/6
    p <- rvec(m)
    df1 <- rvec(m)
    df2 <- 3
    ans_obtained <- qf_rvec(p, df1, df2)
    ans_expected <- rvec(matrix(qf(p = m, df1 = m, df2 = 3), nr = 2))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qf_rvec' works with valid input, ncp supplied", {
    m <- matrix(1:6, nr = 2)/6
    p <- rvec(m)
    df1 <- rvec(m)
    df2 <- 3
    ans_obtained <- qf_rvec(p, df1, df2, ncp = 3)
    ans_expected <- rvec(matrix(qf(p = m, df1 = m, df2 = 3, ncp = 3), nr = 2))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'rf_rvec' works with valid input - n_draw is NULL", {
    m <- matrix(1:6, nr = 2)
    df1 <- rvec(m)
    df2 <- 3
    set.seed(0)
    ans_obtained <- rf_rvec(n = 2, df1, df2, ncp = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rf(n = 6, df1 = m, df2 = 3, ncp = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rf_rvec' works with valid input - n_draw supplied", {
    df1 <- 2:1
    df2 <- 3
    set.seed(0)
    ans_obtained <- rf_rvec(n = 2, df1, df2, ncp = 3, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rf(n = 6, df1 = 2:1, df2 = 3, ncp = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rf_rvec' works with valid input - n_draw supplied, ncp not supplied", {
    df1 <- 2:1
    df2 <- 3
    set.seed(0)
    ans_obtained <- rf_rvec(n = 2, df1, df2, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rf(n = 6, df1 = 2:1, df2 = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


## 'gamma' ---------------------------------------------------------------------

test_that("'dgamma_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    shape <- rvec(m)
    rate <- rvec(2 * m)
    ans_obtained <- dgamma_rvec(x = x, shape = shape, rate = rate, log = TRUE)
    ans_expected <- rvec(matrix(dgamma(x = x, shape = m, rate = 2 * m, log = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
    ans_obtained_scale <- dgamma_rvec(x = x, shape = shape, scale = 1 / rate, log = TRUE)
    expect_equal(ans_obtained_scale, ans_obtained)    
})

test_that("'dgamma_rvec' throws correct error when rate and scale both supplied", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    shape <- rvec(m)
    rate <- rvec(2 * m)
    scale <- 1 / rate
    expect_error(dgamma_rvec(x = x, shape = shape, scale = scale, rate = rate, log = TRUE),
                 "Value supplied for `rate` and for `scale`.")
})

test_that("'pgamma_rvec' works with valid input - scale", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    shape <- rvec(m)
    scale <- rvec(2 * m)
    ans_obtained <- pgamma_rvec(q, shape = shape, scale = scale, log.p = TRUE)
    ans_expected <- rvec(matrix(pgamma(q = q, shape = m, scale = 2 * m, log.p = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'pgamma_rvec' works with valid input - rate", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    shape <- rvec(m)
    scale <- rvec(2 * m)
    ans_obtained <- pgamma_rvec(q, shape = shape, rate = 1 / scale, log.p = TRUE)
    ans_expected <- rvec(matrix(pgamma(q = q, shape = m, rate = 1 / (2 * m), log.p = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'pgamma_rvec' throws error when rate and scale both supplied", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    shape <- rvec(m)
    scale <- rvec(2 * m)
    expect_error(pgamma_rvec(q, shape = shape, scale = scale, rate = 1 / scale, log.p = TRUE),
                 "Value supplied for `rate` and for `scale`.")
})

test_that("'qgamma_rvec' works with valid input - rate supplied", {
    m <- matrix(1:6, nr = 2)/6
    p <- rvec(m)
    shape <- rvec(m)
    rate <- 3
    ans_obtained <- qgamma_rvec(p, shape, rate = rate)
    ans_expected <- rvec(matrix(qgamma(p = m, shape = m, rate = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qgamma_rvec' works with valid input - scale supplied", {
    m <- matrix(1:6, nr = 2)/6
    p <- rvec(m)
    shape <- rvec(m)
    scale <- 3
    ans_obtained <- qgamma_rvec(p, shape, scale = scale)
    ans_expected <- rvec(matrix(qgamma(p = m, shape = m, scale = scale), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qgamma_rvec' throws error when rate and scale both supplied", {
    m <- matrix(1:6, nr = 2)
    p <- 0.3
    shape <- rvec(m)
    scale <- rvec(2 * m)
    expect_error(qgamma_rvec(p, shape = shape, scale = scale, rate = 1 / scale, log.p = TRUE),
                 "Value supplied for `rate` and for `scale`.")
})

test_that("'rgamma_rvec' works with valid input - n_draw is NULL", {
    m <- matrix(1:6, nr = 2)
    shape <- rvec(m)
    rate <- 3
    set.seed(0)
    ans_obtained <- rgamma_rvec(n = 2, shape = shape, rate = rate)
    set.seed(0)
    ans_expected <- rvec(matrix(rgamma(n = 6, shape = m, rate = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rgamma_rvec' works with valid input - n_draw is supplied", {
    m <- matrix(1:6, nr = 2)
    shape <- rvec(m)
    scale <- 3
    set.seed(0)
    ans_obtained <- rgamma_rvec(n = 2, shape = shape, scale = scale, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rgamma(n = 6, shape = m, scale = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qgamma_rvec' throws error when rate and scale both supplied", {
    m <- matrix(1:6, nr = 2)
    shape <- rvec(m)
    scale <- rvec(2 * m)
    expect_error(rgamma_rvec(3, shape = shape, scale = scale, rate = 1 / scale),
                 "Value supplied for `rate` and for `scale`.")
})


## 'geom' ---------------------------------------------------------------------

test_that("'dgeom_rvec' works with valid input", {
    m <- 0.1 * matrix(1:6, nr = 2)
    prob <- rvec(m)
    x <- 2:1
    prob <- rvec(m)
    ans_obtained <- dgeom_rvec(x, prob)
    ans_expected <- rvec(matrix(dgeom(x = x, prob = m, log = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'pgeom_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)/6
    prob <- rvec(m)
    q <- 2:1
    prob <- rvec(m)
    ans_obtained <- pgeom_rvec(q, prob, lower.tail = FALSE)
    ans_expected <- rvec(matrix(pgeom(q, prob = m, lower.tail = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qgeom_rvec' works with valid input", {
    m <- matrix(seq(0.1, 0.6, 0.1), nr = 2)
    p <- rvec(m)
    prob <- c(0.1, 0.8)
    ans_obtained <- qgeom_rvec(p, prob, lower.tail = FALSE)
    ans_expected <- rvec(matrix(qgeom(m, prob = prob, lower.tail = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rgeom_rvec' works with valid input - no n_draw", {
    m <- matrix(seq(0.1, 0.6, 0.1), nr = 2)
    prob <- rvec(m)
    set.seed(0)
    ans_obtained <- rgeom_rvec(2, prob)
    set.seed(0)
    ans_expected <- rvec(matrix(rgeom(6, prob = m), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rgeom_rvec' works with valid input - n_draw, non-rvec input", {
    prob <- (1:3)/4
    set.seed(0)
    ans_obtained <- rgeom_rvec(3, prob, n_draw = 2)
    set.seed(0)
    ans_expected <- rvec(matrix(rgeom(6, prob = rep(prob, 2)), nr = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rgeom_rvec' works with valid input - n_draw, rvec input", {
    m <- matrix(seq(0.1, 0.6, 0.1), nr = 3)
    prob <- rvec(m)
    set.seed(0)
    ans_obtained <- rgeom_rvec(3, prob, n_draw = 2)
    set.seed(0)
    ans_expected <- rvec(matrix(rgeom(6, prob = m), nr = 3))
    expect_identical(ans_obtained, ans_expected)
})


## 'hyper' --------------------------------------------------------------------

test_that("'dhyper_rvec' works with valid input", {
    mm <- matrix(1:6, nr = 2)
    m <- rvec(mm)
    n <- 1
    k <- 2
    x <- rvec(2:1)
    ans_obtained <- dhyper_rvec(x = x, m = m, n = n, k = k, log = TRUE)
    ans_expected <- rvec(matrix(dhyper(x = rep(2:1, 3),
                                       m = mm,
                                       n = n,
                                       k = k,
                                       log = TRUE),
                                nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'phyper_rvec' works with valid input", {
    mm <- matrix(1:6, nr = 2)
    m <- rvec(mm)
    n <- 2
    k <- rvec(mm)
    q <- 2:1
    ans_obtained <- phyper_rvec(q, m = m, n = n, k = k, log.p = TRUE)
    ans_expected <- rvec(matrix(phyper(q = q,
                                       m = mm,
                                       n = n,
                                       k = mm,
                                       log.p = TRUE),
                                nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qhyper_rvec' works with valid input", {
    mm <- matrix(1:6, nr = 2)
    p <- rvec(mm) / 7
    m <- rvec(mm)
    n <- rvec(mm)
    k <- 1:2
    ans_obtained <- qhyper_rvec(p, m = m, n = n, k = k)
    ans_expected <- rvec(matrix(qhyper(mm/7, m = mm, n = mm, k = 1:2), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rhyper_rvec' works with valid input - n_draw is NULL", {
    mm <- matrix(1:6, nr = 2)
    m <- rvec(mm)
    n <- rvec(mm)
    k <- 1:2
    set.seed(0)
    ans_obtained <- rhyper_rvec(nn = 2, m = m, n = n, k = k)
    set.seed(0)
    ans_expected <- rvec(matrix(rhyper(nn = 6, m = mm, n = mm, k = k), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rhyper_rvec' works with valid input - n_draw is supplied", {
    mm <- matrix(1:6, nr = 2)
    m <- rvec(mm)
    n <- 1:2
    k <- 2:1
    set.seed(0)
    ans_obtained <- rhyper_rvec(nn = 2, m, n, k, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rhyper(nn = 6, m = mm, n = n, k = k), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


## 'lnorm' --------------------------------------------------------------------

test_that("'dlnorm_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    meanlog <- rvec(m)
    sdlog <- rvec(2 * m)
    ans_obtained <- dlnorm_rvec(x = x, meanlog = meanlog, sdlog = sdlog, log = TRUE)
    ans_expected <- rvec(matrix(dlnorm(x = x, meanlog = m, sdlog = 2 * m, log = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'plnorm_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    meanlog <- rvec(m)
    sdlog <- rvec(2 * m)
    ans_obtained <- plnorm_rvec(q, meanlog, sdlog, log.p = TRUE)
    ans_expected <- rvec(matrix(plnorm(q = q, meanlog = m, sdlog = 2 * m, log.p = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qlnorm_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    p <- rvec(m)/6
    meanlog <- rvec(m)
    sdlog <- 3
    ans_obtained <- qlnorm_rvec(p, meanlog, sdlog)
    ans_expected <- rvec(matrix(qlnorm(p = m/6, meanlog = m, sdlog = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rlnorm_rvec' works with valid input - n_draw is NULL", {
    m <- matrix(1:6, nr = 2)
    meanlog <- rvec(m)
    sdlog <- 3
    set.seed(0)
    ans_obtained <- rlnorm_rvec(n = 2, meanlog, sdlog)
    set.seed(0)
    ans_expected <- rvec(matrix(rlnorm(n = 6, meanlog = m, sdlog = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rlnorm_rvec' works with valid input - n_draw is supplied", {
    m <- matrix(1:6, nr = 2)
    meanlog <- rvec(m)
    sdlog <- 3
    set.seed(0)
    ans_obtained <- rlnorm_rvec(n = 2, meanlog, sdlog, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rlnorm(n = 6, meanlog = m, sdlog = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


## 'multinom' -----------------------------------------------------------------

test_that("'dmultinom_rvec' works with valid input - x, size rvec", {
    x <- rvec(1:3)
    size <- rvec(matrix(6, nr = 1, nc = 2))
    prob <- 3:1
    ans_obtained <- dmultinom_rvec(x = x, size = size, prob = prob, log = TRUE)
    ans_expected <- double(2)
    ans_expected[[1]] <- dmultinom(x = 1:3, size = 6, prob = prob, log = TRUE)
    ans_expected[[2]] <- dmultinom(x = 1:3, size = 6, prob = prob, log = TRUE)
    ans_expected <- rvec(matrix(ans_expected, nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dmultinom_rvec' works with valid input - size not supplied", {
    x <- rvec(1:3)
    prob <- rvec(cbind(3:1, 4:2))
    ans_obtained <- dmultinom_rvec(x = x, prob = prob, log = TRUE)
    ans_expected <- double(2)
    ans_expected[[1]] <- dmultinom(x = 1:3, prob = 3:1, log = TRUE)
    ans_expected[[2]] <- dmultinom(x = 1:3, prob = 4:2, log = TRUE)
    ans_expected <- rvec(matrix(ans_expected, nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'dmultinom_rvec' works with valid input - prob is rvec", {
    m <- matrix(1:6, nc = 2)
    x <- 1:3
    size <- 6
    prob <- rvec(m/7)
    ans_obtained <- dmultinom_rvec(x = x, size = size, prob = prob, log = TRUE)
    ans_expected <- double(2)
    ans_expected[[1]] <- dmultinom(x = x, size = size, prob = m[,1]/7, log = TRUE)
    ans_expected[[2]] <- dmultinom(x = x, size = size, prob = m[,2]/7, log = TRUE)
    ans_expected <- rvec(matrix(ans_expected, nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dmultinom_rvec' works with valid input - no rvecs", {
    x <- 1:3
    size <- 6
    prob <- 3:1
    ans_obtained <- dmultinom_rvec(x = x, size = size, prob = prob, log = TRUE)
    ans_expected <- dmultinom(x = x, size = size, prob = prob, log = TRUE)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dmultinom' throws expected error when 'x' has length 0", {
    x <- rvec(matrix(0, nr = 0, nc = 1))
    size <- rvec(matrix(6, nr = 1, nc = 2))
    prob <- 3:1
    expect_error(dmultinom_rvec(x = x, size = size, prob = prob, log = TRUE),
                 "`x` has length 0.")
})

test_that("'dmultinom' throws expected error when 'x' and 'p' have different lengths", {
    x <- rvec(matrix(1:6, nr = 2))
    size <- rvec(matrix(6, nr = 1, nc = 2))
    prob <- 3:1
    expect_error(dmultinom_rvec(x = x, size = size, prob = prob, log = TRUE),
                 "`x` and `p` have different lengths.")
})

test_that("'dmultinom' throws expected error when 'size' does not have length 1", {
    x <- rvec(matrix(1:6, nr = 2))
    size <- x
    prob <- 3:2
    expect_error(dmultinom_rvec(x = x, size = size, prob = prob, log = TRUE),
                 "`size` does not have length 1.")
})

test_that("'dmultinom' throws expected error when 'x' is rvec but 'size' is not", {
    x <- rvec(matrix(1:6, nr = 2))
    size <- 1
    prob <- 3:2
    expect_error(dmultinom_rvec(x = x, size = size, prob = prob, log = TRUE),
                 "`x` is an rvec, but `size` is not.")
})

test_that("'dmultinom' throws expected error when 'x' is rvec but 'size' is not", {
    x  <- 4:5
    size <- rvec(matrix(1:2, nr = 1))
    prob <- 3:2
    expect_error(dmultinom_rvec(x = x, size = size, prob = prob, log = TRUE),
                 "`size` is an rvec, but `x` is not.")
})

test_that("'dmultinom' throws expected error when 'x' and 'size' inconsistent", {
    x  <- rvec(4:5)
    size <- rvec(matrix(1:2, nr = 1))
    prob <- 3:2
    expect_error(dmultinom_rvec(x = x, size = size, prob = prob),
                 "Problem with call to function `dmultinom\\(\\)`:")
})

test_that("'rmultinom_rvec' works with valid input - n_draw is NULL, size is rvec", {
    m <- matrix(4:5, nr = 1)
    size <- rvec(m)
    prob <- 3:1
    set.seed(0)
    ans_obtained <- rmultinom_rvec(n = 1, size, prob)
    set.seed(0)
    ans_expected <- rvec(cbind(rmultinom(n = 1, size = m[1], prob = prob),
                               rmultinom(n = 1, size = m[2], prob = prob)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rmultinom_rvec' works with valid input - n_draw is NULL, prob is rvec", {
    size <- 10
    m <- matrix(11:16, nr = 3)
    prob <- rvec(m)
    set.seed(0)
    ans_obtained <- rmultinom_rvec(n = 1, size, prob)
    set.seed(0)
    ans_expected <- rvec(cbind(rmultinom(n = 1, size = 10, prob = m[,1]),
                               rmultinom(n = 1, size = 10, prob = m[,2])))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rmultinom_rvec' works with valid input - n_draw is NULL, size is rvec, prob is rvec", {
    size <- rvec(matrix(10:11, nr = 1))
    m <- matrix(11:16, nr = 3)
    prob <- rvec(m)
    set.seed(0)
    ans_obtained <- rmultinom_rvec(n = 1, size, prob)
    set.seed(0)
    ans_expected <- rvec(cbind(rmultinom(n = 1, size = 10, prob = m[,1]),
                               rmultinom(n = 1, size = 11, prob = m[,2])))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rmultinom_rvec' works with valid input - n_draw is NULL, size not rvec, prob not rvec", {
    size <- 10
    prob <- 1:3
    set.seed(0)
    ans_obtained <- rmultinom_rvec(n = 1, size, prob)
    set.seed(0)
    ans_expected <- rmultinom(n = 1, size = size, prob = prob)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rmultinom_rvec' works with valid input - n_draw is NULL, size is rvec, prob is rvec, n = 2", {
    size <- rvec(matrix(10:11, nr = 1))
    m <- matrix(11:16, nr = 3)
    prob <- rvec(m)
    set.seed(0)
    ans_obtained <- rmultinom_rvec(n = 2, size, prob)
    set.seed(0)
    ans_expected <- vector(mode = "list", length = 2)
    ans_expected[[1]] <- rvec(cbind(rmultinom(n = 1, size = 10, prob = m[,1]),
                                    rmultinom(n = 1, size = 11, prob = m[,2])))
    ans_expected[[2]] <- rvec(cbind(rmultinom(n = 1, size = 10, prob = m[,1]),
                                    rmultinom(n = 1, size = 11, prob = m[,2])))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rmultinom_rvec' works with valid input - n_draw is 3, size, prob not rvec", {
    size <- 10
    prob <- 1:2
    set.seed(0)
    ans_obtained <- rmultinom_rvec(n = 1, size, prob, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(cbind(rmultinom(n = 1, size = size, prob = prob),
                               rmultinom(n = 1, size = size, prob = prob),
                               rmultinom(n = 1, size = size, prob = prob)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rmultinom_rvec' works with valid input - n_draw is 3, prob is rvec", {
    size <- 10
    m <- matrix(1:6, nr = 2)
    prob <- rvec(m)
    set.seed(0)
    ans_obtained <- rmultinom_rvec(n = 1, size, prob, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(cbind(rmultinom(n = 1, size = size, prob = m[,1]),
                               rmultinom(n = 1, size = size, prob = m[,2]),
                               rmultinom(n = 1, size = size, prob = m[,3])))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rmultinom' throws expected error when 'size' does not have length 1", {
    size <- rvec(matrix(1:6, nr = 2))
    prob <- 1:2
    expect_error(rmultinom_rvec(n = 1, size = size, prob = prob),
                 "`size` does not have length 1.")
})

test_that("'rmultinom' throws expected error when 'size' does not have length 1", {
    size <- 10
    prob <- rvec(matrix(0, nrow = 0, nc = 3))
    expect_error(rmultinom_rvec(n = 1, size = size, prob = prob),
                 "`prob` has length 0.")
})

test_that("'rmultinom' throws expected error when 'prob' negative", {
    size <- 10
    prob <- rvec(matrix(c(3, -1, 1), nrow = 3, nc = 1))
    expect_error(rmultinom_rvec(n = 1, size = size, prob = prob),
                 "Problem with call to function `rmultinom\\(\\)`:")
})


## 'nbinom' -------------------------------------------------------------------

test_that("'dnbinom_rvec' works with valid input - prob supplied", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    size <- rvec(m)
    prob <- rvec(m/7)
    ans_obtained <- dnbinom_rvec(x = x, size = size, prob = prob, log = TRUE)
    ans_expected <- rvec(matrix(dnbinom(x = x, size = m, prob = m/7, log = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dnbinom_rvec' works with valid input - mu supplied", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    size <- rvec(m)
    mu <- 0.5 * rvec(m)
    ans_obtained <- dnbinom_rvec(x = x, size = size, mu = mu, log = TRUE)
    ans_expected <- rvec(matrix(dnbinom(x = x, size = m,
                                        mu = 0.5 * m, log = TRUE), nr = 2))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'dnbinom_rvec' throws correct error if prob and mu both supplied", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    size <- rvec(m)
    mu <- 0.5 * rvec(m)
    expect_error(dnbinom_rvec(x = x, size = size, prob = 0.3, mu = mu, log = TRUE),
                 "Value supplied for `prob` and for `mu`.")
})

test_that("'dnbinom_rvec' throws correct error if neighter prob nor mu supplied", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    size <- rvec(m)
    expect_error(dnbinom_rvec(x = x, size = size, log = TRUE),
                 "No value supplied for `prob` or for `mu`.")
})

test_that("'pnbinom_rvec' works with valid input - prob supplied", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    size <- rvec(m)
    prob <- 0.5
    ans_obtained <- pnbinom_rvec(q, size, prob, log.p = TRUE)
    ans_expected <- rvec(matrix(pnbinom(q = q, size = m, prob = 0.5, log.p = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'pnbinom_rvec' works with valid input - mu supplied", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    size <- rvec(m)
    mu <- 2
    ans_obtained <- pnbinom_rvec(q, size, mu = mu, log.p = TRUE)
    ans_expected <- rvec(matrix(pnbinom(q = q, size = m, mu  = mu, log.p = TRUE), nr = 2))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'dnbinom_rvec' throws correct error if prob and mu both supplied", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    size <- rvec(m)
    mu <- 0.5 * rvec(m)
    expect_error(pnbinom_rvec(q = q, size = size, prob = 0.3, mu = mu, log = TRUE),
                 "Value supplied for `prob` and for `mu`.")
})

test_that("'pnbinom_rvec' throws correct error if neighter prob nor mu supplied", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    size <- rvec(m)
    expect_error(pnbinom_rvec(q = q, size = size, log = TRUE),
                 "No value supplied for `prob` or for `mu`.")
})

test_that("'qnbinom_rvec' works with valid input - prob supplied", {
    m <- matrix(1:6, nr = 2)
    p <-rvec(m) / 7
    size <- rvec(m)
    prob <- rvec(0.1 * m)
    ans_obtained <- qnbinom_rvec(p, size, prob)
    ans_expected <- rvec(matrix(qnbinom(p = m / 7, size = m, prob = 0.1 * m), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qnbinom_rvec' works with valid input - mu supplied", {
    m <- matrix(1:6, nr = 2)
    p <- rvec(m) / 7
    size <- rvec(m)
    mu <- rvec(1.3 * m)
    ans_obtained <- qnbinom_rvec(p, size, mu = mu)
    ans_expected <- rvec(matrix(qnbinom(p = m / 7, size = m, mu = 1.3 * m), nr = 2))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qnbinom_rvec' throws correct error if prob and mu both supplied", {
    m <- matrix(1:6, nr = 2)
    p = rvec(m) / 7
    size <- rvec(m)
    mu <- 0.5 * rvec(m)
    expect_error(qnbinom_rvec(p = p, size = size, prob = 0.3, mu = mu, log = TRUE),
                 "Value supplied for `prob` and for `mu`.")
})

test_that("'qnbinom_rvec' throws correct error if neighter prob nor mu supplied", {
    m <- matrix(1:6, nr = 2)
    p <- 0.5
    size <- rvec(m)
    expect_error(qnbinom_rvec(p = p, size = size, log = TRUE),
                 "No value supplied for `prob` or for `mu`.")
})

test_that("'rnbinom_rvec' works with valid input - n_draw is NULL, prob supplied", {
    m <- matrix(1:6, nr = 2)
    size <- rvec(m)
    prob <- 0.23
    set.seed(0)
    ans_obtained <- rnbinom_rvec(n = 2, size, prob)
    set.seed(0)
    ans_expected <- rvec(matrix(rnbinom(n = 6, size = m, prob = 0.23), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rnbinom_rvec' works with valid input - n_draw is NULL, mu supplied", {
    m <- matrix(1:6, nr = 2)
    size <- rvec(m)
    mu <- 2.23
    set.seed(0)
    ans_obtained <- rnbinom_rvec(n = 2, size, mu = mu)
    set.seed(0)
    ans_expected <- rvec_int(matrix(rnbinom(n = 6, size = m, mu = mu), nr = 2))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'rnbinom_rvec' works with valid input - n_draw is 3, prob supplied", {
    m <- matrix(1:6, nr = 2)
    size <- rvec(m)
    prob <- 0.23
    set.seed(0)
    ans_obtained <- rnbinom_rvec(n = 2, size, prob, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rnbinom(n = 6, size = m, prob = 0.23), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rnbinom_rvec' works with valid input - n_draw is 3, mu supplied", {
    m <- matrix(1:6, nr = 2)
    size <- rvec(m)
    mu <- 0.5
    set.seed(0)
    ans_obtained <- rnbinom_rvec(n = 2, size, mu = mu, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec_int(matrix(rnbinom(n = 6, size = m, mu = mu), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rnbinom_rvec' throws correct error if prob and mu both supplied", {
    m <- matrix(1:6, nr = 2)
    size <- rvec(m)
    mu <- 0.5 * rvec(m)
    expect_error(rnbinom_rvec(n = 2, size = size, prob = 0.3, mu = mu),
                 "Value supplied for `prob` and for `mu`.")
})

test_that("'rnbinom_rvec' throws correct error if neighter prob nor mu supplied", {
    m <- matrix(1:6, nr = 2)
    p <- 0.5
    size <- rvec(m)
    expect_error(rnbinom_rvec(n = 2, size = size),
                 "No value supplied for `prob` or for `mu`.")
})


## 'norm' ---------------------------------------------------------------------

test_that("'dnorm_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    mean <- rvec(m)
    sd <- rvec(2 * m)
    ans_obtained <- dnorm_rvec(x = x, mean = mean, sd = sd, log = TRUE)
    ans_expected <- rvec(matrix(dnorm(x = x, mean = m, sd = 2 * m, log = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'pnorm_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    mean <- rvec(m)
    sd <- rvec(2 * m)
    ans_obtained <- pnorm_rvec(q, mean, sd, log.p = TRUE)
    ans_expected <- rvec(matrix(pnorm(q = q, mean = m, sd = 2 * m, log.p = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qnorm_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    p <- rvec(m) / 7
    mean <- rvec(m)
    sd <- 3
    ans_obtained <- qnorm_rvec(p, mean, sd)
    ans_expected <- rvec(matrix(qnorm(p = m / 7, mean = m, sd = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rnorm_rvec' works with valid input - n_draw is NULL", {
    m <- matrix(1:6, nr = 2)
    mean <- rvec(m)
    sd <- 3
    set.seed(0)
    ans_obtained <- rnorm_rvec(n = 2, mean, sd)
    set.seed(0)
    ans_expected <- rvec(matrix(rnorm(n = 6, mean = m, sd = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rnorm_rvec' works with valid input - n_draw is supplied", {
    m <- matrix(1:6, nr = 2)
    mean <- rvec(m)
    sd <- 3
    set.seed(0)
    ans_obtained <- rnorm_rvec(n = 2, mean, sd, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rnorm(n = 6, mean = m, sd = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


## 'pois' ---------------------------------------------------------------------

test_that("'dpois_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    lambda <- rvec(m)
    x <- 2:1
    lambda <- rvec(m)
    ans_obtained <- dpois_rvec(x, lambda)
    ans_expected <- rvec(matrix(dpois(x = x, lambda = m, log = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'ppois_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    lambda <- rvec(m)
    q <- 2:1
    lambda <- rvec(m)
    ans_obtained <- ppois_rvec(q, lambda, lower.tail = FALSE)
    ans_expected <- rvec(matrix(ppois(q, lambda = m, lower.tail = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qpois_rvec' works with valid input", {
    m <- matrix(seq(0.1, 0.6, 0.1), nr = 2)
    p <- rvec(m)
    lambda <- c(2.1, 0.8)
    ans_obtained <- qpois_rvec(p, lambda, lower.tail = FALSE)
    ans_expected <- rvec(matrix(qpois(m, lambda = lambda, lower.tail = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rpois_rvec' works with valid input - no n_draw", {
    m <- matrix(seq(2.1, 2.6, 0.1), nr = 2)
    lambda <- rvec(m)
    set.seed(0)
    ans_obtained <- rpois_rvec(2, lambda)
    set.seed(0)
    ans_expected <- rvec(matrix(rpois(6, lambda = m), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rpois_rvec' works with valid input - n_draw, non-rvec input", {
    lambda <- 1:3
    set.seed(0)
    ans_obtained <- rpois_rvec(3, lambda, n_draw = 2)
    set.seed(0)
    ans_expected <- rvec(matrix(rpois(6, lambda = rep(lambda, 2)), nr = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rpois_rvec' works with valid input - n_draw, rvec input", {
    m <- matrix(seq(2.1, 2.6, 0.1), nr = 3)
    lambda <- rvec(m)
    set.seed(0)
    ans_obtained <- rpois_rvec(3, lambda, n_draw = 2)
    set.seed(0)
    ans_expected <- rvec(matrix(rpois(6, lambda = m), nr = 3))
    expect_identical(ans_obtained, ans_expected)
})


## 't' ------------------------------------------------------------------------

test_that("'dt_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    df <- rvec(m)
    x <- 2:1
    df <- rvec(m)
    ans_obtained <- dt_rvec(x, df)
    ans_expected <- rvec(matrix(dt(x = x, df = m, log = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dt_rvec' works with valid input - ncp supplied", {
    m <- matrix(1:6, nr = 2)
    df <- rvec(m)
    x <- 2:1
    df <- rvec(m)
    ans_obtained <- dt_rvec(x, df, ncp = 0.001)
    ans_expected <- rvec(matrix(dt(x = x, df = m, ncp = 0.001, log = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'pt_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    df <- rvec(m)
    q <- 2:1
    df <- rvec(m)
    ans_obtained <- pt_rvec(q, df, lower.tail = FALSE)
    ans_expected <- rvec(matrix(pt(q, df = m, lower.tail = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'pt_rvec' works with valid input - ncp supplied", {
    m <- matrix(1:6, nr = 2)
    df <- rvec(m)
    q <- 2:1
    df <- rvec(m)
    ans_obtained <- pt_rvec(q, df, ncp = 0.1, lower.tail = FALSE)
    ans_expected <- rvec(matrix(pt(q, df = m, ncp = 0.1, lower.tail = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qt_rvec' works with valid input", {
    m <- matrix(seq(0.1, 0.6, 0.1), nr = 2)
    p <- rvec(m)
    df <- c(2.1, 0.8)
    ans_obtained <- qt_rvec(p, df, lower.tail = FALSE)
    ans_expected <- rvec(matrix(qt(m, df = df, lower.tail = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qt_rvec' works with valid input - ncp supplied", {
    m <- matrix(seq(0.1, 0.6, 0.1), nr = 2)
    p <- rvec(m)
    df <- c(2.1, 0.8)
    ans_obtained <- qt_rvec(p, df, ncp = 1, lower.tail = FALSE)
    ans_expected <- rvec(matrix(qt(m, df = df, ncp = 1, lower.tail = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rt_rvec' works with valid input - n_draw not supplied", {
    m <- matrix(1:6, nr = 2)
    df <- rvec(m)
    set.seed(0)
    ans_obtained <- rt_rvec(2, df)
    set.seed(0)
    ans_expected <- rvec(matrix(rt(6, df = m), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rt_rvec' works with valid input - n_draw supplied", {
    df <- 3:4
    set.seed(0)
    ans_obtained <- rt_rvec(2, df, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rt(6, df = df), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rt_rvec' works with valid input - n_draw, ncp supplied", {
    df <- 3:4
    set.seed(0)
    ans_obtained <- rt_rvec(2, df, ncp = 2, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rt(6, df = df, ncp = 2), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


## 'unif' ---------------------------------------------------------------------

test_that("'dunif_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    min <- rvec(m)
    max <- rvec(2 * m)
    ans_obtained <- dunif_rvec(x = x, min = min, max = max, log = TRUE)
    ans_expected <- rvec(matrix(dunif(x = x, min = m, max = 2 * m, log = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'punif_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    min <- rvec(m)
    max <- rvec(2 * m)
    ans_obtained <- punif_rvec(q, min, max, log.p = TRUE)
    ans_expected <- rvec(matrix(punif(q = q, min = m, max = 2 * m, log.p = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qunif_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    p <- rvec(m) / 7
    min <- rvec(m)
    max <- 7
    ans_obtained <- qunif_rvec(p, min, max)
    ans_expected <- rvec(matrix(qunif(p = m / 7, min = m, max = 7), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'runif_rvec' works with valid input - n_draw is NULL", {
    m <- matrix(1:6, nr = 2)
    min <- rvec(m)
    max <- min + 1
    set.seed(0)
    ans_obtained <- runif_rvec(n = 2, min, max)
    set.seed(0)
    ans_expected <- rvec(matrix(runif(n = 6, min = m, max = m + 1), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'runif_rvec' works with valid input - n_draw is supplied", {
    m <- matrix(1:6, nr = 2)
    min <- rvec(m)
    max <- 7
    set.seed(0)
    ans_obtained <- runif_rvec(n = 2, min, max, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(runif(n = 6, min = m, max = 7), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


## 'weibull' ---------------------------------------------------------------------

test_that("'dweibull_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    shape <- rvec(m)
    scale <- rvec(2 * m)
    ans_obtained <- dweibull_rvec(x = x, shape = shape, scale = scale, log = TRUE)
    ans_expected <- rvec(matrix(dweibull(x = x, shape = m, scale = 2 * m, log = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'pweibull_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    shape <- rvec(m)
    scale <- rvec(2 * m)
    ans_obtained <- pweibull_rvec(q, shape, scale, log.p = TRUE)
    ans_expected <- rvec(matrix(pweibull(q = q, shape = m, scale = 2 * m, log.p = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qweibull_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    p <- rvec(m) / 7
    shape <- rvec(m)
    scale <- 7
    ans_obtained <- qweibull_rvec(p, shape, scale)
    ans_expected <- rvec(matrix(qweibull(p = m / 7, shape = m, scale = 7), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rweibull_rvec' works with valid input - n_draw is NULL", {
    m <- matrix(1:6, nr = 2)
    shape <- rvec(m)
    scale <- shape + 1
    set.seed(0)
    ans_obtained <- rweibull_rvec(n = 2, shape, scale)
    set.seed(0)
    ans_expected <- rvec(matrix(rweibull(n = 6, shape = m, scale = m + 1), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rweibull_rvec' works with valid input - n_draw is supplied", {
    m <- matrix(1:6, nr = 2)
    shape <- rvec(m)
    scale <- 7
    set.seed(0)
    ans_obtained <- rweibull_rvec(n = 2, shape, scale, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rweibull(n = 6, shape = m, scale = 7), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


## 'dist_rvec_1' --------------------------------------------------------------

test_that("'dist_rvec_1' works with valid rvec input", {
    m <- matrix(1:6, nr = 2)
    lambda <- rvec(m)
    set.seed(0)
    ans_obtained <- dist_rvec_1(fun = rpois, arg = lambda, n = 6)
    set.seed(0)
    ans_expected <- rvec(matrix(rpois(n = 6, lambda = 1:6), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_1' works with valid non-rvec input", {
    set.seed(0)
    ans_obtained <- dist_rvec_1(fun = rpois, arg = 1:6, n = 6)
    set.seed(0)
    ans_expected <- rpois(n = 6, lambda = 1:6)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_1' throws appropriate error with invalid inputs", {
    m <- matrix(letters[1:6], nr = 2)
    lambda <- rvec(m)
    expect_error(dist_rvec_1(fun = rpois, arg = lambda, n = 6),
                 "Problem with call to function `rpois\\(\\)`.")
})

test_that("'dist_rvec_1' gives appropriate warning with NAs", {
    m <- matrix(c(1:5, NA), nr = 2)
    lambda <- rvec(m)
    expect_warning(dist_rvec_1(fun = rpois, arg = lambda, n = 6),
                   "NAs produced")
})


## 'dist_rvec_2' --------------------------------------------------------------

test_that("'dist_rvec_2' works with valid rvec input - density, both args rvecs", {
    m <- matrix(1:6, nr = 2)
    x <- rvec(m)
    lambda <- rvec(m + 1)
    ans_obtained <- dist_rvec_2(fun = dpois, arg1 = x, arg2 = lambda)
    ans_expected <- rvec(matrix(dpois(x = m, lambda = m + 1), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_2' works with valid rvec input - density, arg1 numeric", {
    m <- matrix(1:6, nr = 2)
    x <- 1:2
    lambda <- rvec(m)
    ans_obtained <- dist_rvec_2(fun = dpois, arg1 = x, arg2 = lambda)
    ans_expected <- rvec(matrix(dpois(x = rep(1:2, 3), lambda = m), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_2' works with valid rvec input - density, arg2 numeric", {
    m <- matrix(1:6, nr = 2)
    x <- rvec(m)
    lambda <- 1:2
    ans_obtained <- dist_rvec_2(fun = dpois, arg1 = x, arg2 = lambda)
    ans_expected <- rvec(matrix(dpois(x = m, lambda = rep(1:2, 3)), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_2' works with valid rvec input - density, arg1, arg2 numeric", {
    x <- 2:1
    lambda <- 1:2
    ans_obtained <- dist_rvec_2(fun = dpois, arg1 = x, arg2 = lambda)
    ans_expected <- dpois(x = 2, lambda = lambda)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_2' throws appropriate error with invalid inputs", {
    m <- matrix(letters[1:6], nr = 2)
    x <- rvec(m)
    lambda <- c(1, 1)
    expect_error(dist_rvec_2(fun = dpois, arg1 = x, arg2 = lambda),
                 "`x` has class")
})

test_that("'dist_rvec_2' throws appropriate error with error in fun", {
    m <- matrix(1:6, nr = 2)
    x <- rvec(m)
    lambda <- c(1, "a")
    expect_error(dist_rvec_2(fun = dpois, arg1 = x, arg2 = lambda),
                 "Non-numeric argument")
})

test_that("'dist_rvec_2' warns about NAs", {
    m <- matrix(1:6, nr = 2)
    x <- rvec(m)
    lambda <- c(1, -1)
    expect_warning(dist_rvec_2(fun = dpois, arg1 = x, arg2 = lambda),
                 "NAs produced")
})


## 'dist_rvec_3' --------------------------------------------------------------

test_that("'dist_rvec_3' works with valid rvec input - density, all args rvecs", {
    m <- matrix(1:6, nr = 2)
    x <- rvec(m)
    mean <- rvec(m + 1)
    sd <- rvec(m + 3)
    set.seed(0)
    ans_obtained <- dist_rvec_3(fun = dnorm, arg1 = x, arg2 = mean, arg3 = sd)
    set.seed(0)
    ans_expected <- rvec(matrix(dnorm(x = m, mean = m + 1, sd = m + 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_3' works with valid rvec input - density, arg2 numeric", {
    m <- matrix(1:6, nr = 2)
    x <- rvec(m)
    mean <- 1:2
    sd <- rvec(m + 3)
    set.seed(0)
    ans_obtained <- dist_rvec_3(fun = dnorm, arg1 = x, arg2 = mean, arg3 = sd)
    set.seed(0)
    ans_expected <- rvec(matrix(dnorm(x = m, mean = rep(1:2, times = 3), sd = m + 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_3' works with valid rvec input - density, arg1, arg2 numeric", {
    x <- 3:4
    mean <- 1:2
    m <- matrix(1:6, nr = 2)
    sd <- rvec(m)
    set.seed(0)
    ans_obtained <- dist_rvec_3(fun = dnorm, arg1 = x, arg2 = mean, arg3 = sd)
    set.seed(0)
    ans_expected <- rvec(matrix(dnorm(x = rep(3:4, times = 3),
                                      mean = rep(1:2, times = 3),
                                      sd = m),
                                nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


test_that("'dist_rvec_3' works with valid rvec input - density, arg1 numeric", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    mean <- rvec(m + 1)
    sd <- rvec(m + 3)
    set.seed(0)
    ans_obtained <- dist_rvec_3(fun = dnorm, arg1 = x, arg2 = mean, arg3 = sd)
    set.seed(0)
    ans_expected <- rvec(matrix(dnorm(x = x, mean = m + 1, sd = m + 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


test_that("'dist_rvec_3' works with valid rvec input - density, arg1, arg2, arg3 numeric", {
    x <- 2:1
    mean <- 1:2
    sd <- 1:2
    ans_obtained <- dist_rvec_3(fun = dnorm, arg1 = x, arg2 = mean, arg3 = sd, log = TRUE)
    ans_expected <- dnorm(x = x, mean = mean, sd = sd, log = TRUE)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_3' throws appropriate error with invalid inputs", {
    m <- matrix(1:6, nr = 2)
    x <- rvec(m)
    mean <- 1:2
    sd  <- c("a", 0)
    expect_error(dist_rvec_3(fun = dnorm, arg1 = x, arg2 = mean, arg3 = sd),
                 "Problem with call to function `dnorm\\(\\)`.")
})

test_that("'dist_rvec_3' warns about NAs", {
    m <- matrix(c(1:5, NA), nr = 2)
    x <- rvec(m)
    mean <- 1:2
    sd  <- c(0.1, -0.4)
    expect_warning(dist_rvec_3(fun = dnorm, arg1 = x, arg2 = mean, arg3 = sd),
                   "NAs produced")
})


## 'dist_rvec_4' --------------------------------------------------------------

test_that("'dist_rvec_4' works with valid rvec input - density, all args rvecs", {
    y <- matrix(1:6, nr = 2)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4(fun = dhyper, arg1 = yy, arg2 = yy,
                                arg3 = zz, arg4 = zz)
    ans_expected <- rvec(matrix(dhyper(y, y, z, z), nrow = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4' works with valid rvec input - density, args 2,3,4 rvecs", {
    y <- matrix(1:6, nr = 2)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4(fun = dhyper, arg1 = z, arg2 = yy,
                                arg3 = zz, arg4 = zz)
    ans_expected <- rvec(matrix(dhyper(x = z, m = y, n = z, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4' works with valid rvec input - density, args 1,3,4 rvecs", {
    y <- matrix(1:6, nr = 2)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4(fun = dhyper, arg1 = yy, arg2 = z,
                                arg3 = zz, arg4 = zz)
    ans_expected <- rvec(matrix(dhyper(x = y, m = z, n = z, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4' works with valid rvec input - density, args 1,2,4 rvecs", {
    y <- matrix(1:6, nr = 2)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4(fun = dhyper, arg1 = yy, arg2 = yy,
                                arg3 = z, arg4 = zz)
    ans_expected <- rvec(matrix(dhyper(x = y, m = y, n = z, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4' works with valid rvec input - density, args 1,2,3 rvecs", {
    y <- matrix(1:6, nr = 2)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4(fun = dhyper, arg1 = yy, arg2 = yy,
                                arg3 = zz, arg4 = z)
    ans_expected <- rvec(matrix(dhyper(x = y, m = y, n = z, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4' works with valid rvec input - density, args 3,4 rvecs", {
    y <- matrix(1:6, nr = 2)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4(fun = dhyper, arg1 = z, arg2 = z,
                                arg3 = yy, arg4 = yy)
    ans_expected <- rvec(matrix(dhyper(x = z, m = z, n = y, k = y), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4' works with valid rvec input - density, args 2,4 rvecs", {
    y <- matrix(1, nr = 2, nc = 3)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4(fun = dhyper, arg1 = z, arg2 = zz,
                                arg3 = z, arg4 = yy)
    ans_expected <- rvec(matrix(dhyper(x = z, m = z, n = z, k = y), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4' works with valid rvec input - density, args 1,4 rvecs", {
    y <- matrix(1, nr = 2, nc = 3)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4(fun = dhyper, arg1 = yy, arg2 = z,
                                arg3 = z, arg4 = yy)
    ans_expected <- rvec(matrix(dhyper(x = y, m = z, n = z, k = y), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4' works with valid rvec input - density, args 1,3 rvecs", {
    y <- matrix(1, nr = 2, nc = 3)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4(fun = dhyper, arg1 = yy, arg2 = z,
                                arg3 = zz, arg4 = z)
    ans_expected <- rvec(matrix(dhyper(x = y, m = z, n = z, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4' works with valid rvec input - density, args 2,3 rvecs", {
    y <- matrix(1, nr = 2, nc = 3)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4(fun = dhyper, arg1 = z, arg2 = yy,
                                arg3 = zz, arg4 = z)
    ans_expected <- rvec(matrix(dhyper(x = z, m = y, n = z, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4' works with valid rvec input - density, arg 4 rvec", {
    y <- matrix(1, nr = 2, nc = 3)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4(fun = dhyper, arg1 = z, arg2 = z,
                                arg3 = z, arg4 = yy)
    ans_expected <- rvec(matrix(dhyper(x = z, m = z, n = z, k = y), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4' works with valid rvec input - density, arg 3 rvec", {
    y <- matrix(1, nr = 2, nc = 3)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4(fun = dhyper, arg1 = z, arg2 = z,
                                arg3 = yy, arg4 = z)
    ans_expected <- rvec(matrix(dhyper(x = z, m = z, n = y, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4' works with valid rvec input - density, arg 2 rvec", {
    y <- matrix(1, nr = 2, nc = 3)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4(fun = dhyper, arg1 = z, arg2 = yy,
                                arg3 = z, arg4 = z)
    ans_expected <- rvec(matrix(dhyper(x = z, m = y, n = z, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4' works with valid rvec input - density, arg 1 rvec", {
    y <- matrix(1, nr = 2, nc = 3)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4(fun = dhyper, arg1 = yy, arg2 = z,
                                arg3 = z, arg4 = z)
    ans_expected <- rvec(matrix(dhyper(x = y, m = z, n = z, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4' throws appropriate error with invalid inputs", {
    m <- matrix(1:6, nr = 2)
    x <- rvec(m)
    n <- 1:2
    k <- as.character(1:2)
    expect_error(dist_rvec_4(fun = dhyper, arg1 = x, arg2 = m,
                             arg3 = n, arg4 = "k"),
                 "Problem with call to function `dhyper\\(\\)`.")
})

test_that("'dist_rvec_4' warns about NAs", {
  m <- matrix(c(1:5, NA), nr = 2)
  x <- rvec(m)
  n <- 1:2
  k <- c(1, -2)
  expect_warning(dist_rvec_4(fun = dhyper, arg1 = x, arg2 = m,
                             arg3 = n, arg4 = k),
                 "NAs produced")
})

