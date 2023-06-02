
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

test_that("'pbeta_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    shape1 <- rvec(m)
    shape2 <- rvec(2 * m)
    ans_obtained <- pbeta_rvec(q, shape1, shape2, log.p = TRUE)
    ans_expected <- rvec(matrix(pbeta(q = q, shape1 = m, shape2 = 2 * m, log.p = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qbeta_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    q <- rvec(m)
    shape1 <- rvec(m)
    shape2 <- 3
    ans_obtained <- pbeta_rvec(q, shape1, shape2)
    ans_expected <- rvec(matrix(pbeta(q = m, shape1 = m, shape2 = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rbeta_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    shape1 <- rvec(m)
    shape2 <- 3
    set.seed(0)
    ans_obtained <- rbeta_rvec(n = 2, shape1, shape2, ncp = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rbeta(n = 6, shape1 = m, shape2 = 3, ncp = 3), nr = 2))
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
    m <- matrix(1:6, nr = 2)
    q <- 0.5 * rvec(m)
    size <- rvec(m)
    prob <- rvec(0.1 * m)
    ans_obtained <- pbinom_rvec(q, size, prob)
    ans_expected <- rvec(matrix(pbinom(q = 0.5 * m, size = m, prob = 0.1 * m), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rbinom_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    size <- rvec(m)
    prob <- 0.23
    set.seed(0)
    ans_obtained <- rbinom_rvec(n = 2, size, prob)
    set.seed(0)
    ans_expected <- rvec(matrix(rbinom(n = 6, size = m, prob = 0.23), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


## 'binom' ---------------------------------------------------------------------

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
    m <- -1 * matrix(1:6, nr = 2)
    q <- 0.5 * rvec(m)
    location <- rvec(m)
    scale <- rvec(abs(0.1 * m))
    ans_obtained <- pcauchy_rvec(q, location, scale)
    ans_expected <- rvec(matrix(pcauchy(q = 0.5 * m, location = m,
                                        scale = abs(0.1 * m)), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rcauchy_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    location <- rvec(m)
    scale <- 10
    set.seed(0)
    ans_obtained <- rcauchy_rvec(n = 2, location, scale)
    set.seed(0)
    ans_expected <- rvec(matrix(rcauchy(n = 6, location = m, scale = 10), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


## 'chisq' ---------------------------------------------------------------------

test_that("'dchisq_rvec' works with valid input", {
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

test_that("'qchisq_rvec' works with valid input", {
    m <- matrix(seq(0.1, 0.6, 0.1), nr = 2)
    p <- rvec(m)
    df <- c(2.1, 0.8)
    ans_obtained <- qchisq_rvec(p, df, lower.tail = FALSE)
    ans_expected <- rvec(matrix(qchisq(m, df = df, lower.tail = FALSE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rchisq_rvec' works with valid input", {
    m <- matrix(seq(2.1, 2.6, 0.1), nr = 2)
    df <- rvec(m)
    set.seed(0)
    ans_obtained <- rchisq_rvec(2, df)
    set.seed(0)
    ans_expected <- rvec(matrix(rchisq(6, df = m), nr = 2))
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

test_that("'rexp_rvec' works with valid input", {
    m <- matrix(seq(2.1, 2.6, 0.1), nr = 2)
    rate <- rvec(m)
    set.seed(0)
    ans_obtained <- rexp_rvec(2, rate)
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
    expect_identical(ans_obtained, ans_expected)
})

test_that("'pf_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    q <- 2:1
    df1 <- rvec(m)
    df2 <- rvec(2 * m)
    ans_obtained <- pf_rvec(q, df1, df2, log.p = TRUE)
    ans_expected <- rvec(matrix(pf(q = q, df1 = m, df2 = 2 * m, log.p = TRUE), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'qf_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    q <- rvec(m)
    df1 <- rvec(m)
    df2 <- 3
    ans_obtained <- pf_rvec(q, df1, df2)
    ans_expected <- rvec(matrix(pf(q = m, df1 = m, df2 = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rf_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    df1 <- rvec(m)
    df2 <- 3
    set.seed(0)
    ans_obtained <- rf_rvec(n = 2, df1, df2, ncp = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(rf(n = 6, df1 = m, df2 = 3, ncp = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
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
    q <- rvec(m)
    mean <- rvec(m)
    sd <- 3
    ans_obtained <- pnorm_rvec(q, mean, sd)
    ans_expected <- rvec(matrix(pnorm(q = m, mean = m, sd = 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rnorm_rvec' works with valid input", {
    m <- matrix(1:6, nr = 2)
    mean <- rvec(m)
    sd <- 3
    set.seed(0)
    ans_obtained <- rnorm_rvec(n = 2, mean, sd)
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

test_that("'rpois_rvec' works with valid input", {
    m <- matrix(seq(2.1, 2.6, 0.1), nr = 2)
    lambda <- rvec(m)
    set.seed(0)
    ans_obtained <- rpois_rvec(2, lambda)
    set.seed(0)
    ans_expected <- rvec(matrix(rpois(6, lambda = m), nr = 2))
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
    lambda <- 1:2
    expect_error(dist_rvec_2(fun = dpois, arg1 = x, arg2 = lambda),
                 "Problem with call to function `dpois\\(\\)`.")
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
    m <- matrix(letters[1:6], nr = 2)
    x <- rvec(m)
    mean <- 1:2
    sd  <- 1:2
    expect_error(dist_rvec_3(fun = dnorm, arg1 = x, arg2 = mean, arg3 = sd),
                 "Problem with call to function `dnorm\\(\\)`.")
})






