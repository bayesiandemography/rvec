
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


test_that("Beta functions preserve alignment and omitted ncp", {
    for (kind in c("d", "p", "q")) {
        fun <- get(paste0(kind, "beta_rvec"))
        base_fun <- get(paste0(kind, "beta"), envir = asNamespace("stats"))
        for (log in c(FALSE, TRUE)) {
            values <- c(0, 0.1, 0.3, 0.7, 0.9, 1)
            if (kind == "q" && log)
                values <- log(values)
            x <- matrix(values, nrow = 2L)
            cases <- list(list(x, matrix(c(0.5, 3), ncol = 1L), matrix(c(1, 2, 3), nrow = 1L)),
                          list(x[, 1L, drop = FALSE], matrix(1:6, 2L), matrix(2)),
                          list(x[1L, , drop = FALSE], matrix(2), matrix(1:6, 2L)))
            for (args in cases) {
                full <- lapply(args, function(m)
                    m[rep(seq_len(nrow(m)), length.out = 2L),
                      rep(seq_len(ncol(m)), length.out = 3L), drop = FALSE])
                for (extra in list(list(), list(ncp = 0), list(ncp = c(0, 2)))) {
                    for (lower in c(FALSE, TRUE)) {
                        flags <- if (kind == "d") list(log = log) else list(lower.tail = lower, log.p = log)
                        expected <- rvec(do.call(base_fun, c(full, extra, flags)))
                        expect_identical(do.call(fun, c(lapply(args, rvec), extra, flags)), expected)
                    }
                }
            }
        }
    }
})

test_that("'rbeta_rvec' preserves draw order and RNG state with and without ncp", {
    parameters <- list(c(0.5, 3), rvec(c(0.5, 3)),
                       rvec(matrix(c(0.5, 1, 3), nrow = 1L)),
                       rvec(matrix(c(0.5, 1, 2, 3, 4, 5), nrow = 2L)))
    for (shape1 in parameters) for (shape2 in parameters) {
        for (n_draw in list(NULL, 3L)) {
            args <- list(shape1, shape2)
            if (!is.null(n_draw) && any(vapply(args, function(x) is_rvec(x) && n_draw(x) != n_draw, TRUE)))
                next
            draws <- if (!is.null(n_draw)) n_draw else max(vapply(args, function(x) if (is_rvec(x)) n_draw(x) else 1L, 1L))
            full <- lapply(args, function(x) {
                m <- if (is_rvec(x)) as.matrix(x) else matrix(x, ncol = 1L)
                m[rep(seq_len(nrow(m)), length.out = 2L),
                  rep(seq_len(ncol(m)), length.out = draws), drop = FALSE]
            })
            for (extra in list(list(), list(ncp = 0), list(ncp = c(0, 2)))) {
                set.seed(42)
                expected <- as.double(do.call(rbeta, c(list(n = 2L * draws), full, extra)))
                if (any(vapply(args, is_rvec, TRUE)) || !is.null(n_draw))
                    expected <- rvec(matrix(expected, nrow = 2L))
                state_expected <- .Random.seed
                next_expected <- rbeta(5L, 2, 3)
                set.seed(42)
                expect_identical(do.call(rbeta_rvec, c(list(n = 2L, shape1 = shape1, shape2 = shape2, n_draw = n_draw), extra)), expected)
                expect_identical(.Random.seed, state_expected)
                expect_identical(rbeta(5L, 2, 3), next_expected)
            }
        }
    }
})

test_that("Beta functions preserve empty outputs and validation", {
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    set.seed(42)
    state_before <- .Random.seed
    for (extra in list(list(), list(ncp = 0), list(ncp = 2))) {
        for (fun in list(dbeta_rvec, pbeta_rvec, qbeta_rvec)) {
            expect_identical(do.call(fun, c(list(numeric(), 2, 3), extra)), numeric())
            expect_identical(do.call(fun, c(list(empty, 2, 3), extra)), empty)
            expect_identical(do.call(fun, c(list(0.5, empty, 3), extra)), empty)
            expect_identical(do.call(fun, c(list(0.5, 2, empty), extra)), empty)
            expect_warning(do.call(fun, c(list(NA_real_, 2, 3), extra)), "NAs produced")
            expect_warning(do.call(fun, c(list(0.5, -1, 3), extra)), "NAs produced")
            expect_error(do.call(fun, c(list(rvec(0.5), rvec(matrix(1:4, 2L)), rvec(matrix(1:6, 2L))), extra)), "Can't align")
        }
        expect_identical(do.call(rbeta_rvec, c(list(0L, 2, 3), extra)), numeric())
        expect_identical(do.call(rbeta_rvec, c(list(0L, 2, 3, n_draw = 3L), extra)), empty)
        expect_identical(do.call(rbeta_rvec, c(list(0L, empty, 3), extra)), empty)
        expect_error(do.call(rbeta_rvec, c(list(2L, rvec(1:2), 3, n_draw = 3L), extra)), "has 1 draws")
        expect_error(do.call(rbeta_rvec, c(list(2L, rvec(matrix(1:4, 2L)), rvec(matrix(1:6, 2L))), extra)), "Can't align")
        expect_error(do.call(rbeta_rvec, c(list(2L, "a", 3, n_draw = 3L), extra)), "must not be a character vector")
    }
    for (fun in list(dbeta_rvec, pbeta_rvec, qbeta_rvec, rbeta_rvec)) {
        for (ncp in list(-1, NA_real_, "a", rvec(1)))
            expect_error(fun(2L, 2, 3, ncp = ncp))
    }
    expect_identical(.Random.seed, state_before)
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
    ans_expected <- rvec(matrix(as.double(rbinom(n = 6, size = m, prob = 0.23)),
                                nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rbinom_rvec' works with valid input - n_draw is 3", {
    m <- matrix(1:6, nr = 2)
    size <- rvec(m)
    prob <- 0.23
    set.seed(0)
    ans_obtained <- rbinom_rvec(n = 2, size, prob, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(matrix(as.double(rbinom(n = 6, size = m, prob = 0.23)),
                                          nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


test_that("binomial density, probability, and quantile functions preserve alignment", {
    for (kind in c("d", "p", "q")) {
        fun <- get(paste0(kind, "binom_rvec"))
        base_fun <- get(paste0(kind, "binom"), envir = asNamespace("stats"))
        for (log in c(FALSE, TRUE)) {
            values <- if (kind == "q") c(0, 0.1, 0.3, 0.7, 0.9, 1) else c(0, 1, 2, 3, 10, Inf)
            if (kind == "q" && log)
                values <- log(values)
            x <- matrix(values, nrow = 2L)
            size <- matrix(c(0, 10), ncol = 1L)
            prob <- matrix(c(0, 0.3, 1), nrow = 1L)
            cases <- list(list(x, size, prob),
                          list(x[, 1L, drop = FALSE], matrix(1:6, 2L), prob),
                          list(x[1L, , drop = FALSE], size, matrix(c(0, 0.1, 0.3, 0.5, 0.9, 1), 2L)))
            for (args in cases) {
                full <- lapply(args, function(m)
                    m[rep(seq_len(nrow(m)), length.out = 2L),
                      rep(seq_len(ncol(m)), length.out = 3L), drop = FALSE])
                for (lower in c(FALSE, TRUE)) {
                    flags <- if (kind == "d") list(log = log) else list(lower.tail = lower, log.p = log)
                    expected <- rvec(do.call(base_fun, c(full, flags)))
                    expect_identical(do.call(fun, c(lapply(args, rvec), flags)), expected)
                }
            }
        }
    }
})

test_that("'rbinom_rvec' preserves boundary values, double output, and RNG state", {
    sizes <- list(c(0, 10), rvec(c(0, 10)), rvec(matrix(c(0, 5, 10), nrow = 1L)),
                  rvec(matrix(c(0, 1, 2, 5, 10, 20), nrow = 2L)), c(3e9, 10))
    for (size in sizes) {
        for (prob in list(0.3, c(0, 1), rvec(matrix(c(0, 0.1, 0.3, 0.5, 0.9, 1), 2L)))) {
            draws <- max(if (is_rvec(size)) n_draw(size) else 1L,
                         if (is_rvec(prob)) n_draw(prob) else 1L)
            full <- lapply(list(size, prob), function(x) {
                m <- if (is_rvec(x)) as.matrix(x) else matrix(x, ncol = 1L)
                m[rep(seq_len(nrow(m)), length.out = 2L),
                  rep(seq_len(ncol(m)), length.out = draws), drop = FALSE]
            })
            set.seed(42)
            expected <- as.double(rbinom(2L * draws, full[[1L]], full[[2L]]))
            if (is_rvec(size) || is_rvec(prob))
                expected <- rvec(matrix(expected, nrow = 2L))
            state_expected <- .Random.seed
            next_expected <- rbinom(5L, 10, 0.3)
            set.seed(42)
            expect_identical(rbinom_rvec(2L, size, prob), expected)
            expect_identical(.Random.seed, state_expected)
            expect_identical(rbinom(5L, 10, 0.3), next_expected)
        }
    }
    set.seed(42)
    expected <- rvec(matrix(as.double(rbinom(6L, c(0, 10), 0.3)), nrow = 2L))
    state_expected <- .Random.seed
    set.seed(42)
    expect_identical(rbinom_rvec(2L, c(0, 10), 0.3, n_draw = 3L), expected)
    expect_identical(.Random.seed, state_expected)
})

test_that("binomial functions preserve empty outputs and validation", {
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    set.seed(42)
    state_before <- .Random.seed
    for (fun in list(dbinom_rvec, pbinom_rvec, qbinom_rvec)) {
        expect_identical(fun(numeric(), 10, 0.3), numeric())
        expect_identical(fun(empty, 10, 0.3), empty)
        expect_identical(fun(0, empty, 0.3), empty)
        expect_identical(fun(0, 10, empty), empty)
        expect_warning(fun(NA_real_, 10, 0.3), "NAs produced")
        expect_warning(fun(0, 10, -0.1), "NAs produced")
        expect_error(fun(1:2, 1:3, 0.3), "Can't recycle")
    }
    expect_identical(rbinom_rvec(0L, 10, 0.3), numeric())
    expect_identical(rbinom_rvec(0L, 10, 0.3, n_draw = 3L), empty)
    expect_identical(rbinom_rvec(0L, empty, 0.3), empty)
    expect_error(rbinom_rvec(2L, rvec(1:2), 0.3, n_draw = 3L), "has 1 draws")
    expect_error(rbinom_rvec(2L, rvec(matrix(1:4, 2L)), rvec(matrix(rep(0.3, 6), 2L))), "Can't align")
    expect_error(rbinom_rvec(2L, "a", 0.3, n_draw = 3L), "must not be a character vector")
    expect_error(rbinom_rvec(2L, 10, "a", n_draw = 3L), "must not be a character vector")
    expect_identical(.Random.seed, state_before)
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


test_that("Cauchy density, probability, and quantile functions preserve alignment", {
    for (kind in c("d", "p", "q")) {
        fun <- get(paste0(kind, "cauchy_rvec"))
        base_fun <- get(paste0(kind, "cauchy"), envir = asNamespace("stats"))
        for (log in c(FALSE, TRUE)) {
            values <- if (kind == "q") c(0, 0.1, 0.3, 0.7, 0.9, 1) else c(-Inf, -2, 0, 1, 3, Inf)
            if (kind == "q" && log)
                values <- log(values)
            x <- matrix(values, nrow = 2L)
            location <- matrix(c(-1, 2), ncol = 1L)
            scale <- matrix(c(0.5, 1, 2), nrow = 1L)
            cases <- list(list(x, location, scale),
                          list(x[, 1L, drop = FALSE], matrix(1:6, 2L), scale),
                          list(x[1L, , drop = FALSE], location, matrix(1:6 / 3, 2L)))
            for (args in cases) {
                full <- lapply(args, function(m)
                    m[rep(seq_len(nrow(m)), length.out = 2L),
                      rep(seq_len(ncol(m)), length.out = 3L), drop = FALSE])
                for (lower in c(FALSE, TRUE)) {
                    flags <- if (kind == "d") list(log = log) else list(lower.tail = lower, log.p = log)
                    expected <- rvec(do.call(base_fun, c(full, flags)))
                    expect_identical(do.call(fun, c(lapply(args, rvec), flags)), expected)
                }
            }
        }
    }
})

test_that("'rcauchy_rvec' preserves draw order, degenerate values, and RNG state", {
    parameters <- list(c(-1, 2), rvec(c(-1, 2)), rvec(matrix(c(-1, 0, 2), nrow = 1L)),
                       rvec(matrix(c(-1, 0, 2, 1, 3, 4), nrow = 2L)))
    for (location in parameters) {
        for (scale in list(1, c(0, 2), rvec(matrix(c(0, 1, 2, 0, 3, 4), 2L)))) {
            draws <- max(if (is_rvec(location)) n_draw(location) else 1L,
                         if (is_rvec(scale)) n_draw(scale) else 1L)
            full <- lapply(list(location, scale), function(x) {
                m <- if (is_rvec(x)) as.matrix(x) else matrix(x, ncol = 1L)
                m[rep(seq_len(nrow(m)), length.out = 2L),
                  rep(seq_len(ncol(m)), length.out = draws), drop = FALSE]
            })
            set.seed(42)
            expected <- rcauchy(2L * draws, full[[1L]], full[[2L]])
            if (is_rvec(location) || is_rvec(scale))
                expected <- rvec(matrix(expected, nrow = 2L))
            state_expected <- .Random.seed
            next_expected <- rcauchy(5L)
            set.seed(42)
            expect_identical(rcauchy_rvec(2L, location, scale), expected)
            expect_identical(.Random.seed, state_expected)
            expect_identical(rcauchy(5L), next_expected)
        }
    }
    set.seed(42)
    expected <- rvec(matrix(rcauchy(6L, c(-1, 2), 1), nrow = 2L))
    state_expected <- .Random.seed
    set.seed(42)
    expect_identical(rcauchy_rvec(2L, c(-1, 2), 1, n_draw = 3L), expected)
    expect_identical(.Random.seed, state_expected)
})

test_that("Cauchy functions preserve empty outputs and validation", {
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    set.seed(42)
    state_before <- .Random.seed
    for (fun in list(dcauchy_rvec, pcauchy_rvec, qcauchy_rvec)) {
        expect_identical(fun(numeric()), numeric())
        expect_identical(fun(empty), empty)
        expect_identical(fun(0.5, location = empty), empty)
        expect_identical(fun(0.5, scale = empty), empty)
        expect_warning(fun(NA_real_), "NAs produced")
        expect_warning(fun(0.5, scale = -1), "NAs produced")
        expect_error(fun(1:2, location = 1:3), "Can't recycle")
    }
    expect_identical(rcauchy_rvec(0L), numeric())
    expect_identical(rcauchy_rvec(0L, n_draw = 3L), empty)
    expect_identical(rcauchy_rvec(0L, location = empty), empty)
    expect_error(rcauchy_rvec(2L, location = rvec(1:2), n_draw = 3L), "has 1 draws")
    expect_error(rcauchy_rvec(2L, location = rvec(matrix(1:4, 2L)), scale = rvec(matrix(1:6, 2L))), "Can't align")
    expect_error(rcauchy_rvec(2L, location = "a", n_draw = 3L), "must not be a character vector")
    expect_error(rcauchy_rvec(2L, scale = "a", n_draw = 3L), "must not be a character vector")
    expect_identical(.Random.seed, state_before)
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


test_that("Chi-squared functions preserve alignment and omitted ncp", {
    for (kind in c("d", "p", "q")) {
        fun <- get(paste0(kind, "chisq_rvec"))
        base_fun <- get(paste0(kind, "chisq"), envir = asNamespace("stats"))
        for (log in c(FALSE, TRUE)) {
            values <- if (kind == "q") c(0, 0.1, 0.3, 0.7, 0.9, 1) else c(-Inf, -2, 0, 1, 3, Inf)
            if (kind == "q" && log)
                values <- log(values)
            x <- matrix(values, nrow = 2L)
            cases <- list(list(x, matrix(c(0.5, 3), ncol = 1L)),
                          list(x[, 1L, drop = FALSE], matrix(1:6, 2L)),
                          list(x[1L, , drop = FALSE], matrix(c(1, 3), ncol = 1L)))
            for (args in cases) {
                full <- lapply(args, function(m)
                    m[rep(seq_len(nrow(m)), length.out = 2L),
                      rep(seq_len(ncol(m)), length.out = 3L), drop = FALSE])
                for (extra in list(list(), list(ncp = 0), list(ncp = c(0, 2)))) {
                    for (lower in c(FALSE, TRUE)) {
                        flags <- if (kind == "d") list(log = log) else list(lower.tail = lower, log.p = log)
                        expected <- rvec(do.call(base_fun, c(full, extra, flags)))
                        expect_identical(do.call(fun, c(lapply(args, rvec), extra, flags)), expected)
                    }
                }
            }
        }
    }
})

test_that("'rchisq_rvec' preserves draw order and RNG state with and without ncp", {
    parameters <- list(c(0, 3), rvec(c(0, 3)),
                       rvec(matrix(c(0, 1, 3), nrow = 1L)),
                       rvec(matrix(c(0, 1, 2, 3, 4, 5), nrow = 2L)))
    for (df in parameters) {
        for (n_draw in list(NULL, 3L)) {
            if (is_rvec(df) && !is.null(n_draw) && n_draw(df) != n_draw)
                next
            draws <- if (!is.null(n_draw)) n_draw else if (is_rvec(df)) n_draw(df) else 1L
            m <- if (is_rvec(df)) as.matrix(df) else matrix(df, ncol = 1L)
            full <- m[rep(seq_len(nrow(m)), length.out = 2L),
                      rep(seq_len(ncol(m)), length.out = draws), drop = FALSE]
            for (extra in list(list(), list(ncp = 0), list(ncp = c(0, 2)))) {
                set.seed(42)
                expected <- do.call(rchisq, c(list(n = 2L * draws, df = full), extra))
                if (is_rvec(df) || !is.null(n_draw))
                    expected <- rvec(matrix(expected, nrow = 2L))
                state_expected <- .Random.seed
                next_expected <- rchisq(5L, df = 3)
                set.seed(42)
                expect_identical(do.call(rchisq_rvec, c(list(n = 2L, df = df, n_draw = n_draw), extra)), expected)
                expect_identical(.Random.seed, state_expected)
                expect_identical(rchisq(5L, df = 3), next_expected)
            }
        }
    }
})

test_that("Chi-squared functions preserve empty outputs and validation", {
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    set.seed(42)
    state_before <- .Random.seed
    for (extra in list(list(), list(ncp = 0), list(ncp = 2))) {
        for (fun in list(dchisq_rvec, pchisq_rvec, qchisq_rvec)) {
            expect_identical(do.call(fun, c(list(numeric(), df = 2), extra)), numeric())
            expect_identical(do.call(fun, c(list(empty, df = 2), extra)), empty)
            expect_identical(do.call(fun, c(list(0.5, df = empty), extra)), empty)
            expect_warning(do.call(fun, c(list(NA_real_, df = 2), extra)), "NAs produced")
            expect_warning(do.call(fun, c(list(0.5, df = -1), extra)), "NAs produced")
            expect_error(do.call(fun, c(list(1:2, df = 1:3), extra)), "Can't recycle")
        }
        expect_identical(do.call(rchisq_rvec, c(list(n = 0L, df = 2), extra)), numeric())
        expect_identical(do.call(rchisq_rvec, c(list(n = 0L, df = 2, n_draw = 3L), extra)), empty)
        expect_identical(do.call(rchisq_rvec, c(list(n = 0L, df = empty), extra)), empty)
        expect_error(do.call(rchisq_rvec, c(list(n = 2L, df = rvec(1:2), n_draw = 3L), extra)), "has 1 draws")
        expect_error(do.call(rchisq_rvec, c(list(n = 2L, df = "a", n_draw = 3L), extra)))
    }
    for (fun in list(dchisq_rvec, pchisq_rvec, qchisq_rvec, rchisq_rvec)) {
        for (ncp in list(-1, NA_real_, "a", rvec(1)))
            expect_error(fun(2L, df = 2, ncp = ncp))
    }
    expect_identical(.Random.seed, state_before)
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


test_that("F functions preserve alignment and omitted ncp", {
    for (kind in c("d", "p", "q")) {
        fun <- get(paste0(kind, "f_rvec"))
        base_fun <- get(paste0(kind, "f"), envir = asNamespace("stats"))
        for (log in c(FALSE, TRUE)) {
            values <- c(0, 0.1, 0.3, 0.7, 0.9, 1)
            if (kind == "q" && log)
                values <- log(values)
            x <- matrix(values, nrow = 2L)
            cases <- list(list(x, matrix(c(0.5, 3), ncol = 1L), matrix(c(1, 2, 3), nrow = 1L)),
                          list(x[, 1L, drop = FALSE], matrix(1:6, 2L), matrix(2)),
                          list(x[1L, , drop = FALSE], matrix(2), matrix(1:6, 2L)))
            for (args in cases) {
                full <- lapply(args, function(m)
                    m[rep(seq_len(nrow(m)), length.out = 2L),
                      rep(seq_len(ncol(m)), length.out = 3L), drop = FALSE])
                for (extra in list(list(), list(ncp = 0), list(ncp = c(0, 2)))) {
                    for (lower in c(FALSE, TRUE)) {
                        flags <- if (kind == "d") list(log = log) else list(lower.tail = lower, log.p = log)
                        expected <- rvec(do.call(base_fun, c(full, extra, flags)))
                        expect_identical(do.call(fun, c(lapply(args, rvec), extra, flags)), expected)
                    }
                }
            }
        }
    }
})

test_that("'rf_rvec' preserves draw order and RNG state with and without ncp", {
    parameters <- list(c(0.5, 3), rvec(c(0.5, 3)),
                       rvec(matrix(c(0.5, 1, 3), nrow = 1L)),
                       rvec(matrix(c(0.5, 1, 2, 3, 4, 5), nrow = 2L)))
    for (df1 in parameters) for (df2 in parameters) {
        for (n_draw in list(NULL, 3L)) {
            args <- list(df1, df2)
            if (!is.null(n_draw) && any(vapply(args, function(x) is_rvec(x) && n_draw(x) != n_draw, TRUE)))
                next
            draws <- if (!is.null(n_draw)) n_draw else max(vapply(args, function(x) if (is_rvec(x)) n_draw(x) else 1L, 1L))
            full <- lapply(args, function(x) {
                m <- if (is_rvec(x)) as.matrix(x) else matrix(x, ncol = 1L)
                m[rep(seq_len(nrow(m)), length.out = 2L),
                  rep(seq_len(ncol(m)), length.out = draws), drop = FALSE]
            })
            for (extra in list(list(), list(ncp = 0), list(ncp = c(0, 2)))) {
                set.seed(42)
                expected <- as.double(do.call(rf, c(list(n = 2L * draws), full, extra)))
                if (any(vapply(args, is_rvec, TRUE)) || !is.null(n_draw))
                    expected <- rvec(matrix(expected, nrow = 2L))
                state_expected <- .Random.seed
                next_expected <- rf(5L, 2, 3)
                set.seed(42)
                expect_identical(do.call(rf_rvec, c(list(n = 2L, df1 = df1, df2 = df2, n_draw = n_draw), extra)), expected)
                expect_identical(.Random.seed, state_expected)
                expect_identical(rf(5L, 2, 3), next_expected)
            }
        }
    }
})

test_that("F functions preserve empty outputs and validation", {
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    set.seed(42)
    state_before <- .Random.seed
    for (extra in list(list(), list(ncp = 0), list(ncp = 2))) {
        for (fun in list(df_rvec, pf_rvec, qf_rvec)) {
            expect_identical(do.call(fun, c(list(numeric(), 2, 3), extra)), numeric())
            expect_identical(do.call(fun, c(list(empty, 2, 3), extra)), empty)
            expect_identical(do.call(fun, c(list(0.5, empty, 3), extra)), empty)
            expect_identical(do.call(fun, c(list(0.5, 2, empty), extra)), empty)
            expect_warning(do.call(fun, c(list(NA_real_, 2, 3), extra)), "NAs produced")
            expect_warning(do.call(fun, c(list(0.5, -1, 3), extra)), "NAs produced")
            expect_error(do.call(fun, c(list(rvec(0.5), rvec(matrix(1:4, 2L)), rvec(matrix(1:6, 2L))), extra)), "Can't align")
        }
        expect_identical(do.call(rf_rvec, c(list(0L, 2, 3), extra)), numeric())
        expect_identical(do.call(rf_rvec, c(list(0L, 2, 3, n_draw = 3L), extra)), empty)
        expect_identical(do.call(rf_rvec, c(list(0L, empty, 3), extra)), empty)
        expect_error(do.call(rf_rvec, c(list(2L, rvec(1:2), 3, n_draw = 3L), extra)), "has 1 draws")
        expect_error(do.call(rf_rvec, c(list(2L, rvec(matrix(1:4, 2L)), rvec(matrix(1:6, 2L))), extra)), "Can't align")
        expect_error(do.call(rf_rvec, c(list(2L, "a", 3, n_draw = 3L), extra)), "must not be a character vector")
    }
    for (fun in list(df_rvec, pf_rvec, qf_rvec, rf_rvec)) {
        for (ncp in list(-1, NA_real_, "a", rvec(1)))
            expect_error(fun(2L, 2, 3, ncp = ncp))
    }
    expect_identical(.Random.seed, state_before)
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

test_that("'dgamma_rvec' aligns all three arguments with rate or scale", {
    cases <- expand.grid(x_rows = c(1L, 2L), x_draws = c(1L, 3L),
                         shape_rows = c(1L, 2L), shape_draws = c(1L, 3L),
                         rate_rows = c(1L, 2L), rate_draws = c(1L, 3L))
    for (i in seq_len(nrow(cases))) {
        case <- cases[i, ]
        x <- matrix(seq_len(case$x_rows * case$x_draws) / 2, nrow = case$x_rows)
        shape <- matrix(seq_len(case$shape_rows * case$shape_draws) / 3,
                         nrow = case$shape_rows)
        rate <- matrix(seq_len(case$rate_rows * case$rate_draws) / 7,
                        nrow = case$rate_rows)
        n <- max(nrow(x), nrow(shape), nrow(rate))
        draws <- max(ncol(x), ncol(shape), ncol(rate))
        full <- lapply(list(x, shape, rate), function(m)
            m[rep(seq_len(nrow(m)), length.out = n),
              rep(seq_len(ncol(m)), length.out = draws), drop = FALSE])
        for (log in c(FALSE, TRUE)) {
            expected <- rvec(dgamma(full[[1L]], full[[2L]], rate = full[[3L]], log = log))
            expect_identical(dgamma_rvec(rvec(x), rvec(shape), rate = rvec(rate), log = log), expected)
            expected_scale <- rvec(dgamma(full[[1L]], full[[2L]], rate = 1 / full[[3L]], log = log))
            expect_identical(dgamma_rvec(rvec(x), rvec(shape), scale = rvec(rate), log = log), expected_scale)
        }
    }
})

test_that("'dgamma_rvec' preserves ordinary, mixed, and empty inputs", {
    for (x in list(c(a = 0.5, b = 2), 1:2, c(TRUE, FALSE))) {
        expect_identical(dgamma_rvec(x, 2, rate = 3), as.double(dgamma(x, 2, rate = 3)))
        m <- matrix(rep(x, 3L), nrow = 2L)
        expect_identical(dgamma_rvec(rvec(m), 2, rate = 3), rvec(dgamma(m, 2, rate = 3)))
        expect_identical(dgamma_rvec(1, rvec(m), rate = 3), rvec(dgamma(1, m, rate = 3)))
        expect_identical(dgamma_rvec(1, 2, rate = rvec(m)), rvec(dgamma(1, 2, rate = m)))
    }
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    expect_identical(dgamma_rvec(numeric(), 2), numeric())
    expect_identical(dgamma_rvec(empty, 2), empty)
    expect_identical(dgamma_rvec(1, empty), empty)
    expect_identical(dgamma_rvec(1, 2, rate = empty), empty)
})

test_that("'dgamma_rvec' preserves warnings and checks every draw-count pair", {
    set.seed(42)
    state_before <- .Random.seed
    one <- rvec(c(1, 2))
    two <- rvec(matrix(1:4, nrow = 2L))
    three <- rvec(matrix(1:6, nrow = 2L))
    expect_error(dgamma_rvec(one, two, rate = three), "Can't align rvec `shape`")
    expect_error(dgamma_rvec(two, one, rate = three), "Can't align rvec `x`")
    expect_error(dgamma_rvec(two, three, rate = one), "Can't align rvec `x`")
    expect_error(dgamma_rvec(1:2, 1:3), "Can't recycle")
    expect_error(dgamma_rvec(rvec("a"), 2), "`x` has class")
    expect_error(dgamma_rvec(1, rvec("a")), "`shape` has class")
    expect_error(dgamma_rvec(1, 2, rate = rvec("a")), "`rate` has class")
    expect_error(dgamma_rvec("a", 2), "Problem with call to function")
    expect_warning(dgamma_rvec(NA_real_, 2), "NAs produced")
    expect_warning(dgamma_rvec(1, -1), "NAs produced")
    expect_identical(.Random.seed, state_before)
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

test_that("'pgamma_rvec' and 'qgamma_rvec' preserve tails, log probabilities, and scale", {
    for (kind in c("p", "q")) {
        fun <- if (kind == "p") pgamma_rvec else qgamma_rvec
        base_fun <- if (kind == "p") pgamma else qgamma
        values <- if (kind == "p") c(0, 0.1, 1, 2, 10, Inf) else c(0, 0.1, 0.3, 0.7, 0.9, 1)
        for (lower in c(FALSE, TRUE)) {
            for (log in c(FALSE, TRUE)) {
                input <- if (kind == "q" && log) log(values) else values
                m <- matrix(input, nrow = 2L)
                shape <- matrix(c(0.5, 2), ncol = 1L)
                rate <- matrix(c(0.3, 0.7, 1.1), nrow = 1L)
                cases <- list(list(m, shape, rate),
                              list(m[, 1L, drop = FALSE], matrix(1:6, nrow = 2L), rate),
                              list(m[1L, , drop = FALSE], shape, matrix(1:6 / 7, nrow = 2L)))
                for (args in cases) {
                    full <- lapply(args, function(x)
                        x[rep(seq_len(nrow(x)), length.out = 2L),
                          rep(seq_len(ncol(x)), length.out = 3L), drop = FALSE])
                    for (parameter in c("rate", "scale")) {
                        expected_rate <- if (parameter == "rate") full[[3L]] else 1 / full[[3L]]
                        expected <- rvec(base_fun(full[[1L]], full[[2L]], rate = expected_rate,
                                                   lower.tail = lower, log.p = log))
                        call_args <- list(rvec(args[[1L]]), shape = rvec(args[[2L]]),
                                          lower.tail = lower, log.p = log)
                        call_args[[parameter]] <- rvec(args[[3L]])
                        expect_identical(do.call(fun, call_args), expected)
                    }
                }
                expect_identical(fun(input, 2, rate = 3, lower.tail = lower, log.p = log),
                                 as.double(base_fun(input, 2, rate = 3, lower.tail = lower, log.p = log)))
            }
        }
    }
})

test_that("'pgamma_rvec' and 'qgamma_rvec' preserve empty outputs and validation", {
    set.seed(42)
    state_before <- .Random.seed
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    for (fun in list(pgamma_rvec, qgamma_rvec)) {
        expect_identical(fun(numeric(), 2), numeric())
        expect_identical(fun(empty, 2), empty)
        expect_identical(fun(0.5, empty), empty)
        expect_identical(fun(0.5, 2, rate = empty), empty)
        expect_warning(fun(rvec(NA_real_), 2), "NAs produced")
        expect_warning(fun(0.5, -1), "NAs produced")
        expect_error(fun(c(0.1, 0.9), 1:3), "Can't recycle")
        expect_error(fun(rvec(c(0.1, 0.9)), rvec(matrix(1:4, 2)), rate = rvec(matrix(1:6, 2))),
                     "Can't align rvec `shape`")
        expect_error(fun(0.5, rvec("a")), "`shape` has class")
        expect_error(fun(0.5, 2, rate = rvec("a")), "`rate` has class")
        expect_error(fun(0.5, 2, lower.tail = NA))
        expect_error(fun(0.5, 2, log.p = NA))
    }
    expect_warning(qgamma_rvec(1.1, 2), "NAs produced")
    expect_identical(.Random.seed, state_before)
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

test_that("'rgamma_rvec' recycles a single draw in either parameter", {
    one <- matrix(c(2, 3), ncol = 1)
    three <- matrix(1:6, nrow = 2)
    for (parameters in list(list(one, three), list(three, one))) {
        shape <- parameters[[1L]]
        rate <- parameters[[2L]]
        set.seed(42)
        obtained <- rgamma_rvec(n = 2, shape = rvec(shape), rate = rvec(rate))
        state_obtained <- .Random.seed
        set.seed(42)
        expected <- rvec(matrix(rgamma(n = 6, shape = shape, rate = rate),
                                 nrow = 2))
        expect_identical(obtained, expected)
        expect_identical(state_obtained, .Random.seed)
    }
})

test_that("'rgamma_rvec' rejects incompatible draw counts before drawing", {
    shape <- rvec(matrix(1:4, nrow = 2))
    rate <- rvec(matrix(1:6, nrow = 2))
    set.seed(42)
    state_before <- .Random.seed
    expect_error(rgamma_rvec(n = 2, shape = shape, rate = rate),
                 "Can't align rvec")
    expect_identical(.Random.seed, state_before)
})

test_that("'rgamma_rvec' retains strict checking of explicit n_draw", {
    shape <- rvec(matrix(c(2, 3), ncol = 1))
    rate <- rvec(matrix(1:6, nrow = 2))
    expect_error(rgamma_rvec(n = 2, shape = shape, rate = rate, n_draw = 3),
                 "`n_draw` is 3 but `shape` has 1 draws", fixed = TRUE)
})

test_that("'rgamma_rvec' aligns rvec observations and draws", {
    cases <- expand.grid(shape_rows = c(1L, 2L), shape_draws = c(1L, 3L),
                         rate_rows = c(1L, 2L), rate_draws = c(1L, 3L))
    for (i in seq_len(nrow(cases))) {
        case <- cases[i, ]
        shape <- matrix(seq_len(case$shape_rows * case$shape_draws) / 2,
                         nrow = case$shape_rows)
        rate <- matrix(seq_len(case$rate_rows * case$rate_draws),
                        nrow = case$rate_rows)
        draws <- max(ncol(shape), ncol(rate))
        shape_full <- shape[rep(seq_len(nrow(shape)), length.out = 2L),
                            rep(seq_len(ncol(shape)), length.out = draws), drop = FALSE]
        rate_full <- rate[rep(seq_len(nrow(rate)), length.out = 2L),
                          rep(seq_len(ncol(rate)), length.out = draws), drop = FALSE]
        set.seed(42)
        expected <- rvec(matrix(rgamma(2L * draws, shape_full, rate_full), nrow = 2L))
        state_expected <- .Random.seed
        set.seed(42)
        obtained <- rgamma_rvec(2L, rvec(shape), rvec(rate))
        expect_identical(obtained, expected)
        expect_identical(.Random.seed, state_expected)
    }
})

test_that("'rgamma_rvec' preserves ordinary, integer, and logical inputs", {
    for (shape in list(c(0.5, 2), 1:2, c(TRUE, FALSE))) {
        set.seed(42)
        expected <- rgamma(6L, shape, rate = 3)
        state_expected <- .Random.seed
        set.seed(42)
        obtained <- rgamma_rvec(2L, shape, rate = 3, n_draw = 3L)
        expect_identical(obtained, rvec(matrix(expected, nrow = 2L)))
        expect_identical(.Random.seed, state_expected)
        set.seed(42)
        obtained <- rgamma_rvec(2L, rvec(matrix(shape, nrow = 2L, ncol = 3L)), rate = 3)
        expect_identical(obtained, rvec(matrix(expected, nrow = 2L)))
        expect_identical(.Random.seed, state_expected)
    }
    set.seed(42)
    expected <- rgamma(2L, c(0.5, 2), rate = 3)
    set.seed(42)
    expect_identical(rgamma_rvec(2L, c(0.5, 2), rate = 3), expected)
})

test_that("'rgamma_rvec' preserves empty output dimensions without drawing", {
    set.seed(42)
    state_before <- .Random.seed
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    expect_identical(rgamma_rvec(0L, shape = 2), numeric())
    expect_identical(rgamma_rvec(0L, shape = 2, n_draw = 3L), empty)
    expect_identical(rgamma_rvec(0L, shape = empty), empty)
    expect_identical(.Random.seed, state_before)
})

test_that("'rgamma_rvec' preserves warnings and rejects invalid inputs", {
    shape <- c(NA_real_, NaN, -1, 0, 2)
    set.seed(42)
    expected <- suppressWarnings(rgamma(10L, shape, rate = 3))
    state_expected <- .Random.seed
    set.seed(42)
    expect_warning(obtained <- rgamma_rvec(5L, shape, rate = 3, n_draw = 2L),
                   "NAs produced")
    expect_identical(obtained, rvec(matrix(expected, nrow = 5L)))
    expect_identical(.Random.seed, state_expected)
    expect_error(rgamma_rvec(2L, shape = 1:3), "Can't recycle")
    expect_error(rgamma_rvec(2L, shape = 2, n_draw = 0L), "equals 0")
    expect_error(rgamma_rvec(2L, shape = rvec(c("a", "b"))), "`shape` has class")
    expect_error(rgamma_rvec(2L, shape = "a", n_draw = 3L),
                 "`shape` must not be a character vector.", fixed = TRUE)
    expect_error(rgamma_rvec(2L, shape = 2, rate = "a", n_draw = 3L),
                 "`rate` must not be a character vector.", fixed = TRUE)
    expect_error(rgamma_rvec(2L, shape = "a"), "Problem with call to function")
})

test_that("'rgamma_rvec' preserves the scale-to-rate conversion", {
    shape <- c(0.5, 2)
    scale <- rvec(matrix(c(0.3, 0.7, 1.1, 2.3, 0.9, 4.1), nrow = 2L))
    set.seed(42)
    expected <- rvec(matrix(rgamma(6L, shape, rate = as.matrix(1 / scale)), nrow = 2L))
    state_expected <- .Random.seed
    set.seed(42)
    obtained <- rgamma_rvec(2L, shape, scale = scale)
    expect_identical(obtained, expected)
    expect_identical(.Random.seed, state_expected)
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
    ans_expected <- rvec(matrix(as.double(rgeom(6, prob = m)), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rgeom_rvec' works with valid input - n_draw, non-rvec input", {
    prob <- (1:3)/4
    set.seed(0)
    ans_obtained <- rgeom_rvec(3, prob, n_draw = 2)
    set.seed(0)
    ans_expected <- rvec(matrix(as.double(rgeom(6, prob = rep(prob, 2))),
                                nr = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rgeom_rvec' works with valid input - n_draw, rvec input", {
    m <- matrix(seq(0.1, 0.6, 0.1), nr = 3)
    prob <- rvec(m)
    set.seed(0)
    ans_obtained <- rgeom_rvec(3, prob, n_draw = 2)
    set.seed(0)
    ans_expected <- rvec(matrix(as.double(rgeom(6, prob = m)), nr = 3))
    expect_identical(ans_obtained, ans_expected)
})


test_that("exponential and geometric distribution functions preserve alignment", {
    for (family in c("exp", "geom")) {
        for (kind in c("d", "p", "q")) {
            fun <- get(paste0(kind, family, "_rvec"))
            base_fun <- get(paste0(kind, family), envir = asNamespace("stats"))
            for (log in c(FALSE, TRUE)) {
                values <- if (kind == "q") c(0, 0.1, 0.3, 0.7, 0.9, 1) else c(0, 1, 2, 3, 10, Inf)
                if (kind == "q" && log)
                    values <- log(values)
                m <- matrix(values, nrow = 2L)
                cases <- list(list(m, matrix(c(0.3, 1), ncol = 1L)),
                              list(m[, 1L, drop = FALSE], matrix(seq(0.1, 0.6, 0.1), 2L)),
                              list(m[1L, , drop = FALSE], matrix(c(0.3, 1), ncol = 1L)))
                for (args in cases) {
                    full <- lapply(args, function(x)
                        x[rep(seq_len(nrow(x)), length.out = 2L),
                          rep(seq_len(ncol(x)), length.out = 3L), drop = FALSE])
                    for (lower in c(FALSE, TRUE)) {
                        flags <- if (kind == "d") list(log = log) else list(lower.tail = lower, log.p = log)
                        expected <- rvec(do.call(base_fun, c(full, flags)))
                        expect_identical(do.call(fun, c(lapply(args, rvec), flags)), expected)
                        expect_identical(do.call(fun, c(list(values, 0.3), flags)),
                                         as.double(do.call(base_fun, c(list(values, 0.3), flags))))
                    }
                }
            }
        }
    }
})

test_that("exponential and geometric generation preserve output and RNG state", {
    parameters <- list(1, c(0.3, 1), rvec(c(0.3, 1)),
                       rvec(matrix(c(0.1, 0.3, 1), nrow = 1L)),
                       rvec(matrix(seq(0.1, 0.6, 0.1), nrow = 2L)))
    for (family in c("exp", "geom")) {
        fun <- get(paste0("r", family, "_rvec"))
        base_fun <- get(paste0("r", family), envir = asNamespace("stats"))
        for (parameter in parameters) {
            for (explicit in c(FALSE, TRUE)) {
                draws <- if (is_rvec(parameter)) n_draw(parameter) else if (explicit) 3L else 1L
                m <- if (is_rvec(parameter)) as.matrix(parameter) else matrix(parameter, ncol = 1L)
                full <- m[rep(seq_len(nrow(m)), length.out = 2L),
                          rep(seq_len(ncol(m)), length.out = draws), drop = FALSE]
                set.seed(42)
                expected <- as.double(base_fun(2L * draws, full))
                if (explicit || is_rvec(parameter))
                    expected <- rvec(matrix(expected, nrow = 2L))
                state_expected <- .Random.seed
                next_expected <- rnorm(5L)
                set.seed(42)
                obtained <- fun(2L, parameter, n_draw = if (explicit) draws else NULL)
                expect_identical(obtained, expected)
                expect_identical(.Random.seed, state_expected)
                expect_identical(rnorm(5L), next_expected)
            }
        }
    }
})

test_that("exponential and geometric functions preserve empty outputs and validation", {
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    for (family in c("exp", "geom")) {
        for (kind in c("d", "p", "q")) {
            fun <- get(paste0(kind, family, "_rvec"))
            expect_identical(fun(numeric(), 0.3), numeric())
            expect_identical(fun(empty, 0.3), empty)
            expect_identical(fun(0, empty), empty)
            expect_warning(fun(NA_real_, 0.3), "NAs produced")
            expect_warning(fun(0, -1), "NAs produced")
            expect_error(fun(1:2, 1:3), "Can't recycle")
        }
        fun <- get(paste0("r", family, "_rvec"))
        base_fun <- get(paste0("r", family), envir = asNamespace("stats"))
        set.seed(42)
        state_before <- .Random.seed
        expect_identical(fun(0L, 0.3), numeric())
        expect_identical(fun(0L, 0.3, n_draw = 3L), empty)
        expect_identical(fun(0L, empty), empty)
        expect_error(fun(2L, rvec(c(0.3, 1)), n_draw = 3L), "has 1 draws")
        expect_error(fun(2L, 0.3, n_draw = 0L), "equals 0")
        expect_error(fun(2L, 1:3), "Can't recycle")
        expect_error(fun(2L, "a"), "Problem with call to function")
        expect_identical(.Random.seed, state_before)
        parameters <- c(0, -1, NA_real_, Inf)
        set.seed(42)
        expected <- as.double(suppressWarnings(base_fun(4L, parameters)))
        state_expected <- .Random.seed
        set.seed(42)
        expect_warning(obtained <- fun(4L, parameters), "NAs produced")
        expect_identical(obtained, expected)
        expect_identical(.Random.seed, state_expected)
    }
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
    ans_expected <- rvec(matrix(as.double(rhyper(nn = 6,
                                                 m = mm,
                                                 n = mm,
                                                 k = k)),
                                          nr = 2))
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
    ans_expected <- rvec(matrix(as.double(rhyper(nn = 6,
                                                 m = mm,
                                                 n = n,
                                                 k = k)),
                                          nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


test_that("Hypergeometric functions preserve alignment across four inputs", {
    for (kind in c("d", "p", "q")) {
        fun <- get(paste0(kind, "hyper_rvec"))
        base_fun <- get(paste0(kind, "hyper"), envir = asNamespace("stats"))
        for (log in c(FALSE, TRUE)) {
            values <- if (kind == "q") c(0.01, 0.1, 0.3, 0.7, 0.9, 1) else c(-1, 0, 1, 2, 3, 4)
            if (kind == "q" && log)
                values <- log(values)
            args <- list(matrix(values, 2L), matrix(c(10, 12), ncol = 1L),
                         matrix(c(20, 25, 30), nrow = 1L), matrix(1:6, 2L))
            for (position in 1:4) {
                inputs <- args
                inputs[[position]] <- inputs[[position]][1L, , drop = FALSE]
                full <- lapply(inputs, function(m)
                    m[rep(seq_len(nrow(m)), length.out = 2L),
                      rep(seq_len(ncol(m)), length.out = 3L), drop = FALSE])
                for (lower in c(FALSE, TRUE)) {
                    flags <- if (kind == "d") list(log = log) else list(lower.tail = lower, log.p = log)
                    expected <- rvec(do.call(base_fun, c(full, flags)))
                    expect_identical(do.call(fun, c(lapply(inputs, rvec), flags)), expected)
                }
            }
        }
    }
})

test_that("'rhyper_rvec' preserves draw order, output type, and RNG state", {
    layouts <- function(x) list(x[1L], x[1:2], rvec(x[1:2]),
                                rvec(matrix(x[1:3], 1L)), rvec(matrix(x, 2L)))
    for (m in layouts(10:15)) for (n in layouts(20:25)) for (k in layouts(1:6)) {
        for (n_draw in list(NULL, 3L)) {
            args <- list(m, n, k)
            if (!is.null(n_draw) && any(vapply(args, function(x) is_rvec(x) && n_draw(x) != n_draw, TRUE)))
                next
            draws <- if (!is.null(n_draw)) n_draw else max(vapply(args, function(x) if (is_rvec(x)) n_draw(x) else 1L, 1L))
            full <- lapply(args, function(x) {
                x <- if (is_rvec(x)) as.matrix(x) else matrix(x, ncol = 1L)
                x[rep(seq_len(nrow(x)), length.out = 2L),
                  rep(seq_len(ncol(x)), length.out = draws), drop = FALSE]
            })
            set.seed(42)
            expected <- as.double(do.call(rhyper, c(list(nn = 2L * draws), full)))
            if (any(vapply(args, is_rvec, TRUE)) || !is.null(n_draw))
                expected <- rvec(matrix(expected, nrow = 2L))
            state_expected <- .Random.seed
            next_expected <- rhyper(5L, 10, 20, 3)
            set.seed(42)
            expect_identical(rhyper_rvec(2L, m, n, k, n_draw = n_draw), expected)
            expect_identical(.Random.seed, state_expected)
            expect_identical(rhyper(5L, 10, 20, 3), next_expected)
        }
    }
})

test_that("'qhyper_rvec' preserves the base R log-probability boundary result", {
    for (lower in c(FALSE, TRUE)) {
        expected <- suppressWarnings(qhyper(-Inf, 10, 20, 3, lower.tail = lower, log.p = TRUE))
        if (anyNA(expected))
            expect_warning(obtained <- qhyper_rvec(-Inf, 10, 20, 3, lower.tail = lower, log.p = TRUE), "NAs produced")
        else
            obtained <- qhyper_rvec(-Inf, 10, 20, 3, lower.tail = lower, log.p = TRUE)
        expect_identical(obtained, expected)
    }
})

test_that("Hypergeometric functions check every pair of draw counts", {
    for (fun in list(dhyper_rvec, phyper_rvec, qhyper_rvec)) {
        for (pair in combn(1:4, 2L, simplify = FALSE)) {
            args <- list(rvec(c(0.1, 0.5)), m = rvec(c(10, 10)),
                         n = rvec(c(20, 20)), k = rvec(c(2, 2)))
            args[[pair[1L]]] <- rvec(matrix(rep(as.matrix(args[[pair[1L]]]), 2L), 2L))
            args[[pair[2L]]] <- rvec(matrix(rep(as.matrix(args[[pair[2L]]]), 3L), 2L))
            expect_error(do.call(fun, args), "Can't align")
        }
        expect_error(fun(rvec(c(0.1, 0.5)), m = rvec(c(10, 10)),
                         n = rvec(matrix(20, 2L, 2L)), k = rvec(matrix(2, 2L, 3L))),
                     "rvec `n`.*rvec `k`")
    }
    set.seed(42)
    state_before <- .Random.seed
    for (pair in combn(1:3, 2L, simplify = FALSE)) {
        args <- list(m = rvec(c(10, 10)), n = rvec(c(20, 20)), k = rvec(c(2, 2)))
        args[[pair[1L]]] <- rvec(matrix(rep(as.matrix(args[[pair[1L]]]), 2L), 2L))
        args[[pair[2L]]] <- rvec(matrix(rep(as.matrix(args[[pair[2L]]]), 3L), 2L))
        expect_error(do.call(rhyper_rvec, c(list(nn = 2L), args)), "Can't align")
    }
    expect_identical(.Random.seed, state_before)
})

test_that("Hypergeometric functions preserve empty outputs and validation", {
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    for (fun in list(dhyper_rvec, phyper_rvec, qhyper_rvec)) {
        expect_identical(fun(numeric(), 10, 20, 3), numeric())
        for (position in 1:4) {
            args <- list(0.5, 10, 20, 3)
            args[[position]] <- empty
            expect_identical(do.call(fun, args), empty)
        }
        expect_warning(fun(NA_real_, 10, 20, 3), "NAs produced")
        expect_warning(fun(0.5, -1, 20, 3), "NAs produced")
        expect_error(fun(1:2, 1:3, 20, 3), "Can't recycle")
    }
    set.seed(42)
    state_before <- .Random.seed
    expect_identical(rhyper_rvec(0L, 10, 20, 3), numeric())
    expect_identical(rhyper_rvec(0L, 10, 20, 3, n_draw = 3L), empty)
    for (position in 1:3) {
        args <- list(m = 10, n = 20, k = 3)
        args[[position]] <- empty
        expect_identical(do.call(rhyper_rvec, c(list(nn = 0L), args)), empty)
        args[[position]] <- rvec(1:2)
        expect_error(do.call(rhyper_rvec, c(list(nn = 2L, n_draw = 3L), args)), "has 1 draws")
        args[[position]] <- "a"
        expect_error(do.call(rhyper_rvec, c(list(nn = 2L, n_draw = 3L), args)), "must not be a character vector")
    }
    expect_identical(.Random.seed, state_before)
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


test_that("lognormal density, probability, and quantile functions preserve alignment", {
    for (kind in c("d", "p", "q")) {
        fun <- get(paste0(kind, "lnorm_rvec"))
        base_fun <- get(paste0(kind, "lnorm"), envir = asNamespace("stats"))
        for (log in c(FALSE, TRUE)) {
            values <- if (kind == "q") c(0, 0.1, 0.3, 0.7, 0.9, 1) else c(-Inf, -2, 0, 1, 3, Inf)
            if (kind == "q" && log)
                values <- log(values)
            x <- matrix(values, nrow = 2L)
            meanlog <- matrix(c(-1, 2), ncol = 1L)
            sdlog <- matrix(c(0.5, 1, 2), nrow = 1L)
            cases <- list(list(x, meanlog, sdlog),
                          list(x[, 1L, drop = FALSE], matrix(1:6, 2L), sdlog),
                          list(x[1L, , drop = FALSE], meanlog, matrix(1:6 / 3, 2L)))
            for (args in cases) {
                full <- lapply(args, function(m)
                    m[rep(seq_len(nrow(m)), length.out = 2L),
                      rep(seq_len(ncol(m)), length.out = 3L), drop = FALSE])
                for (lower in c(FALSE, TRUE)) {
                    flags <- if (kind == "d") list(log = log) else list(lower.tail = lower, log.p = log)
                    expected <- rvec(do.call(base_fun, c(full, flags)))
                    expect_identical(do.call(fun, c(lapply(args, rvec), flags)), expected)
                }
            }
        }
    }
})

test_that("'rlnorm_rvec' preserves draw order, degenerate values, and RNG state", {
    parameters <- list(c(-1, 2), rvec(c(-1, 2)), rvec(matrix(c(-1, 0, 2), nrow = 1L)),
                       rvec(matrix(c(-1, 0, 2, 1, 3, 4), nrow = 2L)))
    for (meanlog in parameters) {
        for (sdlog in list(1, c(0, 2), rvec(matrix(c(0, 1, 2, 0, 3, 4), 2L)))) {
            draws <- max(if (is_rvec(meanlog)) n_draw(meanlog) else 1L,
                         if (is_rvec(sdlog)) n_draw(sdlog) else 1L)
            full <- lapply(list(meanlog, sdlog), function(x) {
                m <- if (is_rvec(x)) as.matrix(x) else matrix(x, ncol = 1L)
                m[rep(seq_len(nrow(m)), length.out = 2L),
                  rep(seq_len(ncol(m)), length.out = draws), drop = FALSE]
            })
            set.seed(42)
            expected <- rlnorm(2L * draws, full[[1L]], full[[2L]])
            if (is_rvec(meanlog) || is_rvec(sdlog))
                expected <- rvec(matrix(expected, nrow = 2L))
            state_expected <- .Random.seed
            next_expected <- rlnorm(5L)
            set.seed(42)
            expect_identical(rlnorm_rvec(2L, meanlog, sdlog), expected)
            expect_identical(.Random.seed, state_expected)
            expect_identical(rlnorm(5L), next_expected)
        }
    }
    set.seed(42)
    expected <- rvec(matrix(rlnorm(6L, c(-1, 2), 1), nrow = 2L))
    state_expected <- .Random.seed
    set.seed(42)
    expect_identical(rlnorm_rvec(2L, c(-1, 2), 1, n_draw = 3L), expected)
    expect_identical(.Random.seed, state_expected)
})

test_that("lognormal functions preserve empty outputs and validation", {
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    set.seed(42)
    state_before <- .Random.seed
    for (fun in list(dlnorm_rvec, plnorm_rvec, qlnorm_rvec)) {
        expect_identical(fun(numeric()), numeric())
        expect_identical(fun(empty), empty)
        expect_identical(fun(0.5, meanlog = empty), empty)
        expect_identical(fun(0.5, sdlog = empty), empty)
        expect_warning(fun(NA_real_), "NAs produced")
        expect_warning(fun(0.5, sdlog = -1), "NAs produced")
        expect_error(fun(1:2, meanlog = 1:3), "Can't recycle")
    }
    expect_identical(rlnorm_rvec(0L), numeric())
    expect_identical(rlnorm_rvec(0L, n_draw = 3L), empty)
    expect_identical(rlnorm_rvec(0L, meanlog = empty), empty)
    expect_error(rlnorm_rvec(2L, meanlog = rvec(1:2), n_draw = 3L), "has 1 draws")
    expect_error(rlnorm_rvec(2L, meanlog = rvec(matrix(1:4, 2L)), sdlog = rvec(matrix(1:6, 2L))), "Can't align")
    expect_error(rlnorm_rvec(2L, meanlog = "a", n_draw = 3L), "must not be a character vector")
    expect_error(rlnorm_rvec(2L, sdlog = "a", n_draw = 3L), "must not be a character vector")
    expect_identical(.Random.seed, state_before)
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

test_that("'dmultinom_rvec' aligns compact inputs without changing results", {
    xs <- list(c(1, 2, 3), rvec(c(1, 2, 3)), rvec(c(1L, 2L, 3L)),
               rvec(matrix(c(1, 2, 3, 0, 0, 6, 3, 2, 1), 3L)))
    ps <- list(c(2, 3, 5), rvec(c(2L, 3L, 5L)),
               rvec(matrix(c(2, 3, 5, 0, 0, 1, 5, 5, 0), 3L)))
    for (x in xs) for (prob in ps) for (log in c(FALSE, TRUE)) {
        sizes <- if (is_rvec(x)) list(NULL, rvec(6L), rvec(matrix(6, 1L, 3L))) else list(NULL, 6L)
        for (size in sizes) {
            s <- if (is.null(size)) sum(x) else size
            args <- list(x, s, prob)
            nd <- max(vapply(args, function(a) if (is_rvec(a)) n_draw(a) else 1L, 1L))
            full <- lapply(args, function(a) {
                m <- if (is_rvec(a)) as.matrix(a) else matrix(a, ncol = 1L)
                m[, rep(seq_len(ncol(m)), length.out = nd), drop = FALSE]
            })
            expected <- vapply(seq_len(nd), function(j)
                dmultinom(full[[1L]][, j], full[[2L]][1L, j], full[[3L]][, j], log = log), 1)
            if (any(vapply(args, is_rvec, TRUE)))
                expected <- rvec(matrix(expected, 1L))
            expect_identical(dmultinom_rvec(x, size, prob, log), expected)
        }
    }
})

test_that("'dmultinom_rvec' retains default-size behavior across types and boundaries", {
    capture <- function(expr) {
        warnings <- character()
        value <- tryCatch(withCallingHandlers(expr, warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        }), error = function(e) conditionMessage(e))
        list(value = value, warnings = warnings)
    }
    values <- list(c(1, 2, 3), c(1L, 2L, 3L), c(TRUE, FALSE, TRUE),
                   c(NA_real_, 1, 2), c(NaN, 1, 2), c(Inf, 1, 2),
                   c(0.5, 2, 3.5), c(1e16, 1, -1e16),
                   c(2147483647L, 1L, 0L), c("a", "b", "c"), numeric())
    for (v in values) {
        x <- rvec(matrix(v, ncol = 1L))
        prob <- rep(1, length(x))
        for (log in c(FALSE, TRUE)) {
            expected <- capture(dmultinom_rvec(x, size = sum(x), prob = prob, log = log))
            expect_identical(capture(dmultinom_rvec(x, prob = prob, log = log)), expected)
        }
    }
})

test_that("'dmultinom_rvec' checks every pair of draw counts", {
    for (pair in combn(1:3, 2L, simplify = FALSE)) {
        args <- list(x = rvec(c(1, 2, 3)), size = rvec(6), prob = rvec(c(2, 3, 5)))
        for (j in seq_along(pair)) {
            i <- pair[[j]]
            m <- as.matrix(args[[i]])
            args[[i]] <- rvec(m[, rep(1L, j + 1L), drop = FALSE])
        }
        expect_error(do.call(dmultinom_rvec, args), "Can't align")
    }
})

test_that("'rmultinom_rvec' works with valid input - n_draw is NULL, size is rvec", {
    m <- matrix(4:5, nr = 1)
    size <- rvec(m)
    prob <- 3:1
    set.seed(0)
    ans_obtained <- rmultinom_rvec(n = 1, size, prob)
    set.seed(0)
    ans_expected <- rvec(cbind(as.double(rmultinom(n = 1,
                                                   size = m[1], prob = prob)),
                               as.double(rmultinom(n = 1,
                                                   size = m[2], prob = prob))))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rmultinom_rvec' works with valid input - n_draw is NULL, prob is rvec", {
    size <- 10
    m <- matrix(11:16, nr = 3)
    prob <- rvec(m)
    set.seed(0)
    ans_obtained <- rmultinom_rvec(n = 1, size, prob)
    set.seed(0)
    ans_expected <- rvec(cbind(as.double(rmultinom(n = 1,
                                                   size = 10, prob = m[,1])),
                               as.double(rmultinom(n = 1,
                                                   size = 10, prob = m[,2]))))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rmultinom_rvec' works with valid input - n_draw is NULL, size is rvec, prob is rvec", {
    size <- rvec(matrix(10:11, nr = 1))
    m <- matrix(11:16, nr = 3)
    prob <- rvec(m)
    set.seed(0)
    ans_obtained <- rmultinom_rvec(n = 1, size, prob)
    set.seed(0)
    ans_expected <- rvec(cbind(as.double(rmultinom(n = 1,
                                                   size = 10, prob = m[,1])),
                               as.double(rmultinom(n = 1,
                                                   size = 11, prob = m[,2]))))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rmultinom_rvec' works with valid input - n_draw is NULL, size not rvec, prob not rvec", {
    size <- 10
    prob <- 1:3
    set.seed(0)
    ans_obtained <- as.double(rmultinom_rvec(n = 1, size, prob))
    set.seed(0)
    ans_expected <- as.double(rmultinom(n = 1, size = size, prob = prob))
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
    ans_expected[[1]] <- rvec(cbind(as.double(rmultinom(n = 1,
                                                        size = 10,
                                                        prob = m[,1])),
                                    as.double(rmultinom(n = 1,
                                                        size = 11,
                                                        prob = m[,2]))))
    ans_expected[[2]] <- rvec(cbind(as.double(rmultinom(n = 1,
                                                        size = 10,
                                                        prob = m[,1])),
                                    as.double(rmultinom(n = 1,
                                                        size = 11,
                                                        prob = m[,2]))))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rmultinom_rvec' works with valid input - n_draw is 3, size, prob not rvec", {
    size <- 10
    prob <- 1:2
    set.seed(0)
    ans_obtained <- rmultinom_rvec(n = 1, size, prob, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(cbind(as.double(rmultinom(n = 1,
                                                   size = size,
                                                   prob = prob)),
                               as.double(rmultinom(n = 1,
                                                   size = size,
                                                   prob = prob)),
                               as.double(rmultinom(n = 1,
                                                   size = size,
                                                   prob = prob))))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rmultinom_rvec' works with valid input - n_draw is 3, prob is rvec", {
    size <- 10
    m <- matrix(1:6, nr = 2)
    prob <- rvec(m)
    set.seed(0)
    ans_obtained <- rmultinom_rvec(n = 1, size, prob, n_draw = 3)
    set.seed(0)
    ans_expected <- rvec(cbind(as.double(rmultinom(n = 1,
                                                   size = size,
                                                   prob = m[,1])),
                               as.double(rmultinom(n = 1,
                                                   size = size,
                                                   prob = m[,2])),
                               as.double(rmultinom(n = 1,
                                                   size = size,
                                                   prob = m[,3]))))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rmultinom' throws expected error when 'size' does not have length 1", {
  size <- rvec(matrix(1:6, nr = 2))
  prob <- 1:2
  expect_error(rmultinom_rvec(n = 1,
                              size = size,
                              prob = prob),
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


test_that("'rmultinom_rvec' preserves sample order, draw order, and RNG state", {
    sizes <- list(10L, rvec(10L), rvec(matrix(c(0L, 1L, 10L), 1L)))
    probs <- list(c(2, 3, 5), rvec(c(2L, 3L, 5L)),
                  rvec(matrix(c(2, 3, 5, 0, 0, 1, 5, 5, 0), 3L)),
                  setNames(c(TRUE, FALSE, FALSE), c("a", "b", "c")))
    for (size in sizes) for (prob in probs) for (draws in list(NULL, 3L)) {
        args <- list(size, prob)
        if (!is.null(draws) && any(vapply(args, function(x)
            is_rvec(x) && n_draw(x) != draws, TRUE)))
            next
        nd <- if (is.null(draws)) max(vapply(args, function(x)
            if (is_rvec(x)) n_draw(x) else 1L, 1L)) else draws
        s <- if (is_rvec(size)) as.vector(as.matrix(size)) else size
        p <- if (is_rvec(prob)) as.matrix(prob) else matrix(prob, ncol = 1L)
        s <- rep(s, length.out = nd)
        p <- p[, rep(seq_len(ncol(p)), length.out = nd), drop = FALSE]
        for (n in c(1L, 2L)) {
            set.seed(42)
            expected <- lapply(seq_len(n), function(i) {
                m <- vapply(seq_len(nd), function(j)
                    as.double(rmultinom(1L, s[[j]], p[, j])), double(nrow(p)))
                if (any(vapply(args, is_rvec, TRUE)) || !is.null(draws))
                    rvec(m)
                else
                    m
            })
            if (n == 1L)
                expected <- expected[[1L]]
            state_expected <- .Random.seed
            next_expected <- rmultinom(3L, 10, c(0.2, 0.3, 0.5))
            set.seed(42)
            expect_identical(rmultinom_rvec(n, size, prob, n_draw = draws), expected)
            expect_identical(.Random.seed, state_expected)
            expect_identical(rmultinom(3L, 10, c(0.2, 0.3, 0.5)), next_expected)
        }
    }
})

test_that("'rmultinom_rvec' validates compact inputs before sampling", {
    set.seed(42)
    state_before <- .Random.seed
    expect_error(rmultinom_rvec(0L, 10, c(2, 3, 5)), "`n` equals 0")
    for (n in c(1L, 2L)) {
        expect_error(rmultinom_rvec(n, rvec(10), c(2, 3, 5), n_draw = 3L),
                     "has 1 draws")
        expect_error(rmultinom_rvec(n, 10, rvec(c(2, 3, 5)), n_draw = 3L),
                     "has 1 draws")
        expect_error(rmultinom_rvec(n, rvec(matrix(10, 1L, 2L)),
                                   rvec(matrix(1, 3L, 3L))), "Can't align")
        expect_error(rmultinom_rvec(n, "a", c(2, 3, 5), n_draw = 3L),
                     "has class.*rvec_chr")
        expect_error(rmultinom_rvec(n, 10, c("a", "b"), n_draw = 3L),
                     "has class.*rvec_chr")
        expect_error(rmultinom_rvec(n, 10, matrix(1, 3L, 1L), n_draw = 3L),
                     "is not a vector or rvec")
        expect_error(rmultinom_rvec(n, 10, c(2, 3, 5), n_draw = 0L))
    }
    expect_identical(.Random.seed, state_before)
})


## 'nbinom' -------------------------------------------------------------------

test_that("'dnbinom_rvec' works with valid input - prob supplied", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    size <- rvec(m)
    prob <- rvec(m/7)
    ans_obtained <- dnbinom_rvec(x = x, size = size, prob = prob, log = TRUE)
    ans_expected <- rvec(matrix(dnbinom(x = x,
                                        size = m, prob = m/7, log = TRUE),
                                nr = 2))
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
    ans_expected <- rvec(matrix(as.double(rnbinom(n = 6,
                                                  size = m,
                                                  prob = 0.23)),
                                nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rnbinom_rvec' works with valid input - n_draw is NULL, mu supplied", {
  m <- matrix(1:6, nr = 2)
  size <- rvec(m)
  mu <- 2.23
  set.seed(0)
  ans_obtained <- rnbinom_rvec(n = 2, size, mu = mu)
  set.seed(0)
  ans_expected <- rvec(matrix(as.double(rnbinom(n = 6,
                                                size = m,
                                                mu = mu)),
                              nr = 2))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'rnbinom_rvec' works with valid input - n_draw is 3, prob supplied", {
  m <- matrix(1:6, nr = 2)
  size <- rvec(m)
  prob <- 0.23
  set.seed(0)
  ans_obtained <- rnbinom_rvec(n = 2, size, prob, n_draw = 3)
  set.seed(0)
  ans_expected <- rvec_dbl(matrix(rnbinom(n = 6,
                                          size = m,
                                          prob = 0.23),
                                  nr = 2))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'rnbinom_rvec' works with valid input - n_draw is 3, mu supplied", {
  m <- matrix(1:6, nr = 2)
  size <- rvec(m)
  mu <- 0.5
  set.seed(0)
  ans_obtained <- rnbinom_rvec(n = 2, size, mu = mu, n_draw = 3)
  set.seed(0)
  ans_expected <- rvec_dbl(matrix(rnbinom(n = 6,
                                          size = m,
                                          mu = mu),
                                  nr = 2))
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


test_that("Negative-binomial functions preserve alignment and mu conversion", {
    for (kind in c("d", "p", "q")) {
        fun <- get(paste0(kind, "nbinom_rvec"))
        base_fun <- get(paste0(kind, "nbinom"), envir = asNamespace("stats"))
        for (log in c(FALSE, TRUE)) {
            values <- if (kind == "q") c(0, 0.1, 0.3, 0.7, 0.9, 1) else c(-1, 0, 1, 3, 10, Inf)
            if (kind == "q" && log)
                values <- log(values)
            x <- matrix(values, nrow = 2L)
            cases <- list(list(x, matrix(c(0.5, 3), ncol = 1L), matrix(c(0.1, 0.5, 0.9), nrow = 1L)),
                          list(x[, 1L, drop = FALSE], matrix(1:6, 2L), matrix(0.3)),
                          list(x[1L, , drop = FALSE], matrix(2), matrix(1:6 / 7, 2L)))
            for (args in cases) {
                full <- lapply(args, function(m)
                    m[rep(seq_len(nrow(m)), length.out = 2L),
                      rep(seq_len(ncol(m)), length.out = 3L), drop = FALSE])
                for (param in c("prob", "mu")) for (lower in c(FALSE, TRUE)) {
                    flags <- if (kind == "d") list(log = log) else list(lower.tail = lower, log.p = log)
                    prob <- if (param == "prob") full[[3L]] else full[[2L]] / (full[[2L]] + full[[3L]])
                    expected <- rvec(do.call(base_fun, c(list(full[[1L]], full[[2L]], prob), flags)))
                    inputs <- list(rvec(args[[1L]]), size = rvec(args[[2L]]))
                    inputs[[param]] <- rvec(args[[3L]])
                    expect_identical(do.call(fun, c(inputs, flags)), expected)
                }
            }
        }
    }
})

test_that("'rnbinom_rvec' preserves draws and RNG state with prob and mu", {
    parameters <- list(c(0.1, 0.8), rvec(c(0.1, 0.8)),
                       rvec(matrix(c(0.1, 0.5, 0.8), nrow = 1L)),
                       rvec(matrix(1:6 / 7, nrow = 2L)))
    for (size in parameters) for (param in parameters) for (nm in c("prob", "mu")) {
        for (n_draw in list(NULL, 3L)) {
            args <- list(size, param)
            if (!is.null(n_draw) && any(vapply(args, function(x) is_rvec(x) && n_draw(x) != n_draw, TRUE)))
                next
            draws <- if (!is.null(n_draw)) n_draw else max(vapply(args, function(x) if (is_rvec(x)) n_draw(x) else 1L, 1L))
            full <- lapply(args, function(x) {
                m <- if (is_rvec(x)) as.matrix(x) else matrix(x, ncol = 1L)
                m[rep(seq_len(nrow(m)), length.out = 2L),
                  rep(seq_len(ncol(m)), length.out = draws), drop = FALSE]
            })
            prob <- if (nm == "prob") full[[2L]] else full[[1L]] / (full[[1L]] + full[[2L]])
            set.seed(42)
            expected <- as.double(rnbinom(2L * draws, full[[1L]], prob))
            if (any(vapply(args, is_rvec, TRUE)) || !is.null(n_draw))
                expected <- rvec(matrix(expected, nrow = 2L))
            state_expected <- .Random.seed
            next_expected <- rnbinom(5L, 2, 0.3)
            inputs <- list(n = 2L, size = size, n_draw = n_draw)
            inputs[[nm]] <- param
            set.seed(42)
            expect_identical(do.call(rnbinom_rvec, inputs), expected)
            expect_identical(.Random.seed, state_expected)
            expect_identical(rnbinom(5L, 2, 0.3), next_expected)
        }
    }
})

test_that("Negative-binomial functions preserve empty outputs and validation", {
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    set.seed(42)
    state_before <- .Random.seed
    for (nm in c("prob", "mu")) {
        extra <- setNames(list(0.3), nm)
        for (fun in list(dnbinom_rvec, pnbinom_rvec, qnbinom_rvec)) {
            expect_identical(do.call(fun, c(list(numeric(), size = 2), extra)), numeric())
            expect_identical(do.call(fun, c(list(empty, size = 2), extra)), empty)
            expect_identical(do.call(fun, c(list(0.5, size = empty), extra)), empty)
            expect_warning(do.call(fun, c(list(NA_real_, size = 2), extra)), "NAs produced")
            expect_error(do.call(fun, c(list(1:2, size = 1:3), extra)), "Can't recycle")
        }
        expect_identical(do.call(rnbinom_rvec, c(list(0L, size = 2), extra)), numeric())
        expect_identical(do.call(rnbinom_rvec, c(list(0L, size = 2, n_draw = 3L), extra)), empty)
        expect_identical(do.call(rnbinom_rvec, c(list(0L, size = empty), extra)), empty)
        expect_error(do.call(rnbinom_rvec, c(list(2L, size = rvec(1:2), n_draw = 3L), extra)), "has 1 draws")
        expect_error(do.call(rnbinom_rvec, c(list(2L, size = "a", n_draw = 3L), extra)), "must not be a character vector")
    }
    for (fun in list(dnbinom_rvec, pnbinom_rvec, qnbinom_rvec, rnbinom_rvec)) {
        expect_error(fun(2L, size = 2), "No value supplied")
        expect_error(fun(2L, size = 2, prob = 0.3, mu = 2), "Value supplied")
    }
    expect_identical(.Random.seed, state_before)
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


test_that("normal density, probability, and quantile functions preserve alignment", {
    for (kind in c("d", "p", "q")) {
        fun <- get(paste0(kind, "norm_rvec"))
        base_fun <- get(paste0(kind, "norm"), envir = asNamespace("stats"))
        for (log in c(FALSE, TRUE)) {
            values <- if (kind == "q") c(0, 0.1, 0.3, 0.7, 0.9, 1) else c(-Inf, -2, 0, 1, 3, Inf)
            if (kind == "q" && log)
                values <- log(values)
            x <- matrix(values, nrow = 2L)
            mean <- matrix(c(-1, 2), ncol = 1L)
            sd <- matrix(c(0.5, 1, 2), nrow = 1L)
            cases <- list(list(x, mean, sd),
                          list(x[, 1L, drop = FALSE], matrix(1:6, 2L), sd),
                          list(x[1L, , drop = FALSE], mean, matrix(1:6 / 3, 2L)))
            for (args in cases) {
                full <- lapply(args, function(m)
                    m[rep(seq_len(nrow(m)), length.out = 2L),
                      rep(seq_len(ncol(m)), length.out = 3L), drop = FALSE])
                for (lower in c(FALSE, TRUE)) {
                    flags <- if (kind == "d") list(log = log) else list(lower.tail = lower, log.p = log)
                    expected <- rvec(do.call(base_fun, c(full, flags)))
                    expect_identical(do.call(fun, c(lapply(args, rvec), flags)), expected)
                }
            }
        }
    }
})

test_that("'rnorm_rvec' preserves draw order, degenerate values, and RNG state", {
    parameters <- list(c(-1, 2), rvec(c(-1, 2)), rvec(matrix(c(-1, 0, 2), nrow = 1L)),
                       rvec(matrix(c(-1, 0, 2, 1, 3, 4), nrow = 2L)))
    for (mean in parameters) {
        for (sd in list(1, c(0, 2), rvec(matrix(c(0, 1, 2, 0, 3, 4), 2L)))) {
            draws <- max(if (is_rvec(mean)) n_draw(mean) else 1L,
                         if (is_rvec(sd)) n_draw(sd) else 1L)
            full <- lapply(list(mean, sd), function(x) {
                m <- if (is_rvec(x)) as.matrix(x) else matrix(x, ncol = 1L)
                m[rep(seq_len(nrow(m)), length.out = 2L),
                  rep(seq_len(ncol(m)), length.out = draws), drop = FALSE]
            })
            set.seed(42)
            expected <- rnorm(2L * draws, full[[1L]], full[[2L]])
            if (is_rvec(mean) || is_rvec(sd))
                expected <- rvec(matrix(expected, nrow = 2L))
            state_expected <- .Random.seed
            next_expected <- rnorm(5L)
            set.seed(42)
            expect_identical(rnorm_rvec(2L, mean, sd), expected)
            expect_identical(.Random.seed, state_expected)
            expect_identical(rnorm(5L), next_expected)
        }
    }
    set.seed(42)
    expected <- rvec(matrix(rnorm(6L, c(-1, 2), 1), nrow = 2L))
    state_expected <- .Random.seed
    set.seed(42)
    expect_identical(rnorm_rvec(2L, c(-1, 2), 1, n_draw = 3L), expected)
    expect_identical(.Random.seed, state_expected)
})

test_that("normal functions preserve empty outputs and validation", {
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    set.seed(42)
    state_before <- .Random.seed
    for (fun in list(dnorm_rvec, pnorm_rvec, qnorm_rvec)) {
        expect_identical(fun(numeric()), numeric())
        expect_identical(fun(empty), empty)
        expect_identical(fun(0.5, mean = empty), empty)
        expect_identical(fun(0.5, sd = empty), empty)
        expect_warning(fun(NA_real_), "NAs produced")
        expect_warning(fun(0.5, sd = -1), "NAs produced")
        expect_error(fun(1:2, mean = 1:3), "Can't recycle")
    }
    expect_identical(rnorm_rvec(0L), numeric())
    expect_identical(rnorm_rvec(0L, n_draw = 3L), empty)
    expect_identical(rnorm_rvec(0L, mean = empty), empty)
    expect_error(rnorm_rvec(2L, mean = rvec(1:2), n_draw = 3L), "has 1 draws")
    expect_error(rnorm_rvec(2L, mean = rvec(matrix(1:4, 2L)), sd = rvec(matrix(1:6, 2L))), "Can't align")
    expect_error(rnorm_rvec(2L, mean = "a", n_draw = 3L), "must not be a character vector")
    expect_error(rnorm_rvec(2L, sd = "a", n_draw = 3L), "must not be a character vector")
    expect_identical(.Random.seed, state_before)
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

test_that("'dpois_rvec' aligns observations and draws without changing results", {
    cases <- expand.grid(x_rows = c(1L, 2L), x_draws = c(1L, 3L),
                         lambda_rows = c(1L, 2L), lambda_draws = c(1L, 3L))
    for (i in seq_len(nrow(cases))) {
        case <- cases[i, ]
        x <- matrix(seq_len(case$x_rows * case$x_draws) - 1L, nrow = case$x_rows)
        lambda <- matrix(seq_len(case$lambda_rows * case$lambda_draws) / 2,
                          nrow = case$lambda_rows)
        n <- max(nrow(x), nrow(lambda))
        draws <- max(ncol(x), ncol(lambda))
        x_full <- x[rep(seq_len(nrow(x)), length.out = n),
                    rep(seq_len(ncol(x)), length.out = draws), drop = FALSE]
        lambda_full <- lambda[rep(seq_len(nrow(lambda)), length.out = n),
                              rep(seq_len(ncol(lambda)), length.out = draws), drop = FALSE]
        for (log in c(FALSE, TRUE)) {
            expected <- rvec(dpois(x_full, lambda_full, log = log))
            expect_identical(dpois_rvec(rvec(x), rvec(lambda), log = log), expected)
        }
    }
})

test_that("'dpois_rvec' preserves types, attributes, and empty outputs", {
    for (x in list(c(a = 0, b = 3), 0:1, c(TRUE, FALSE))) {
        expect_identical(dpois_rvec(x, 3), as.double(dpois(x, 3)))
        m <- matrix(rep(x, 3L), nrow = 2L,
                     dimnames = list(c("a", "b"), c("one", "two", "three")))
        expected <- rvec(matrix(as.double(dpois(m, 3)), nrow = 2L))
        expect_identical(dpois_rvec(rvec(m), 3), expected)
    }
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    expect_identical(dpois_rvec(numeric(), 3), numeric())
    expect_identical(dpois_rvec(empty, 3), empty)
    expect_identical(dpois_rvec(2, empty), empty)
})

test_that("'dpois_rvec' preserves warnings and validation without drawing", {
    set.seed(42)
    state_before <- .Random.seed
    expect_warning(result <- dpois_rvec(rvec(c(0, NA_real_)), 3), "NAs produced")
    expect_identical(result, rvec(matrix(c(dpois(0, 3), NA_real_), ncol = 1L)))
    expect_warning(dpois_rvec(2, -1), "NAs produced")
    expect_warning(dpois_rvec(0.5, 3), "non-integer")
    expect_error(dpois_rvec(1:2, 1:3), "Can't recycle")
    expect_error(dpois_rvec(rvec(matrix(1:4, 2)), rvec(matrix(1:6, 2))), "Can't align")
    expect_error(dpois_rvec(rvec("a"), 3), "`x` has class")
    expect_error(dpois_rvec(2, rvec("a")), "`lambda` has class")
    expect_error(dpois_rvec("a", 3), "Problem with call to function")
    expect_error(dpois_rvec(2, 3, log = NA))
    expect_identical(.Random.seed, state_before)
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

test_that("'ppois_rvec' and 'qpois_rvec' preserve tails, log probabilities, and alignment", {
    for (kind in c("p", "q")) {
        fun <- if (kind == "p") ppois_rvec else qpois_rvec
        base_fun <- if (kind == "p") ppois else qpois
        values <- if (kind == "p") c(0, 1, 2, 3, 10, Inf) else c(0, 0.1, 0.3, 0.7, 0.9, 1)
        for (lower in c(FALSE, TRUE)) {
            for (log in c(FALSE, TRUE)) {
                input <- if (kind == "q" && log) log(values) else values
                m <- matrix(input, nrow = 2L)
                cases <- list(
                    list(m, matrix(c(0.5, 3), ncol = 1L)),
                    list(m[, 1L, drop = FALSE], matrix(1:6, nrow = 2L)),
                    list(m[1L, , drop = FALSE], matrix(c(0.5, 3), ncol = 1L))
                )
                for (args in cases) {
                    x <- args[[1L]]
                    lambda <- args[[2L]]
                    x_full <- x[rep(seq_len(nrow(x)), length.out = 2L),
                                rep(seq_len(ncol(x)), length.out = 3L), drop = FALSE]
                    lambda_full <- lambda[rep(seq_len(nrow(lambda)), length.out = 2L),
                                          rep(seq_len(ncol(lambda)), length.out = 3L), drop = FALSE]
                    expected <- rvec(base_fun(x_full, lambda_full, lower.tail = lower, log.p = log))
                    expect_identical(fun(rvec(x), rvec(lambda), lower.tail = lower, log.p = log), expected)
                }
                expect_identical(fun(input, 3, lower.tail = lower, log.p = log),
                                 as.double(base_fun(input, 3, lower.tail = lower, log.p = log)))
            }
        }
    }
})

test_that("'ppois_rvec' and 'qpois_rvec' preserve empty outputs and validation", {
    set.seed(42)
    state_before <- .Random.seed
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    for (fun in list(ppois_rvec, qpois_rvec)) {
        expect_identical(fun(numeric(), 3), numeric())
        expect_identical(fun(empty, 3), empty)
        expect_identical(fun(0.5, empty), empty)
        expect_warning(fun(rvec(NA_real_), 3), "NAs produced")
        expect_warning(fun(0.5, -1), "NAs produced")
        expect_error(fun(c(0, 1), 1:3), "Can't recycle")
        expect_error(fun(rvec(matrix(rep(0.5, 4), 2)), rvec(matrix(1:6, 2))), "Can't align")
        expect_error(fun(0.5, rvec("a")), "`lambda` has class")
        expect_error(fun(0.5, 3, lower.tail = NA))
        expect_error(fun(0.5, 3, log.p = NA))
    }
    expect_warning(qpois_rvec(1.1, 3), "NAs produced")
    expect_identical(.Random.seed, state_before)
})

test_that("'rpois_rvec' works with valid input - no n_draw", {
    m <- matrix(seq(2.1, 2.6, 0.1), nr = 2)
    lambda <- rvec(m)
    set.seed(0)
    ans_obtained <- rpois_rvec(2, lambda)
    set.seed(0)
    ans_expected <- rvec(matrix(as.double(rpois(6, lambda = m)), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rpois_rvec' works with valid input - n_draw, non-rvec input", {
    lambda <- 1:3
    set.seed(0)
    ans_obtained <- rpois_rvec(3, lambda, n_draw = 2)
    set.seed(0)
    ans_expected <- rvec(matrix(as.double(rpois(6, lambda = rep(lambda, 2))),
                                nr = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rpois_rvec' works with valid input - n_draw, rvec input", {
    m <- matrix(seq(2.1, 2.6, 0.1), nr = 3)
    lambda <- rvec(m)
    set.seed(0)
    ans_obtained <- rpois_rvec(3, lambda, n_draw = 2)
    set.seed(0)
    ans_expected <- rvec(matrix(as.double(rpois(6, lambda = m)), nr = 3))
    expect_identical(ans_obtained, ans_expected)
})


test_that("'rpois_rvec' preserves values, double output, and RNG state", {
    for (lambda in list(3, c(0, 0.2, 3, 50, 3e9), 1:5,
                        c(TRUE, FALSE, TRUE, FALSE, TRUE))) {
        set.seed(42)
        expected <- rvec(matrix(as.double(rpois(15L, lambda)), nrow = 5L))
        state_expected <- .Random.seed
        set.seed(42)
        obtained <- rpois_rvec(5L, lambda, n_draw = 3L)
        expect_identical(obtained, expected)
        expect_identical(.Random.seed, state_expected)
    }
    set.seed(42)
    expected <- as.double(rpois(5L, 3))
    state_expected <- .Random.seed
    set.seed(42)
    expect_identical(rpois_rvec(5L, 3), expected)
    expect_identical(.Random.seed, state_expected)
})

test_that("'rpois_rvec' recycles rvec observations in draw order", {
    for (rows in c(1L, 2L)) {
        for (values in list(c(0.2, 3, 50, 1, 0, 2), 1:6,
                            c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))) {
            m <- matrix(values, nrow = rows)
            full <- m[rep(seq_len(rows), length.out = 2L), , drop = FALSE]
            set.seed(42)
            expected <- rvec(matrix(as.double(rpois(length(full), full)), nrow = 2L))
            state_expected <- .Random.seed
            for (draws in list(NULL, ncol(m))) {
                set.seed(42)
                obtained <- rpois_rvec(2L, rvec(m), n_draw = draws)
                expect_identical(obtained, expected)
                expect_identical(.Random.seed, state_expected)
            }
        }
    }
})

test_that("'rpois_rvec' preserves empty outputs without drawing", {
    set.seed(42)
    state_before <- .Random.seed
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    expect_identical(rpois_rvec(0L, 3), numeric())
    expect_identical(rpois_rvec(0L, 3, n_draw = 3L), empty)
    expect_identical(rpois_rvec(0L, empty), empty)
    expect_identical(.Random.seed, state_before)
})

test_that("'rpois_rvec' preserves warnings and input validation", {
    lambda <- c(NA_real_, NaN, -1, Inf, 0, 3)
    set.seed(42)
    expected <- rvec(matrix(as.double(suppressWarnings(rpois(12L, lambda))), nrow = 6L))
    state_expected <- .Random.seed
    set.seed(42)
    expect_warning(obtained <- rpois_rvec(6L, lambda, n_draw = 2L), "NAs produced")
    expect_identical(obtained, expected)
    expect_identical(.Random.seed, state_expected)
    expect_error(rpois_rvec(2L, rvec(c(2, 3)), n_draw = 3L),
                 "`n_draw` is 3 but `lambda` has 1 draws.", fixed = TRUE)
    expect_error(rpois_rvec(2L, 3, n_draw = 0L), "equals 0")
    expect_error(rpois_rvec(2L, 1:3), "Can't recycle")
    expect_error(rpois_rvec(2L, "a"), "Problem with call to function")
    expect_error(rpois_rvec(2L, rvec(c("a", "b"))), "Problem with call to function")
    expect_identical(.Random.seed, state_expected)
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


test_that("Student t functions preserve alignment and omitted ncp", {
    for (kind in c("d", "p", "q")) {
        fun <- get(paste0(kind, "t_rvec"))
        base_fun <- get(paste0(kind, "t"), envir = asNamespace("stats"))
        for (log in c(FALSE, TRUE)) {
            values <- if (kind == "q") c(0, 0.1, 0.3, 0.7, 0.9, 1) else c(-Inf, -2, 0, 1, 3, Inf)
            if (kind == "q" && log)
                values <- log(values)
            x <- matrix(values, nrow = 2L)
            cases <- list(list(x, matrix(c(0.5, 3), ncol = 1L)),
                          list(x[, 1L, drop = FALSE], matrix(1:6, 2L)),
                          list(x[1L, , drop = FALSE], matrix(c(1, 3), ncol = 1L)))
            for (args in cases) {
                full <- lapply(args, function(m)
                    m[rep(seq_len(nrow(m)), length.out = 2L),
                      rep(seq_len(ncol(m)), length.out = 3L), drop = FALSE])
                for (extra in list(list(), list(ncp = 0), list(ncp = c(0, 2)))) {
                    for (lower in c(FALSE, TRUE)) {
                        flags <- if (kind == "d") list(log = log) else list(lower.tail = lower, log.p = log)
                        expected <- rvec(do.call(base_fun, c(full, extra, flags)))
                        expect_identical(do.call(fun, c(lapply(args, rvec), extra, flags)), expected)
                    }
                }
            }
        }
    }
})

test_that("'rt_rvec' preserves draw order and RNG state with and without ncp", {
    parameters <- list(c(0.5, 3), rvec(c(0.5, 3)),
                       rvec(matrix(c(0.5, 1, 3), nrow = 1L)),
                       rvec(matrix(c(0.5, 1, 2, 3, 4, 5), nrow = 2L)))
    for (df in parameters) {
        for (n_draw in list(NULL, 3L)) {
            if (is_rvec(df) && !is.null(n_draw) && n_draw(df) != n_draw)
                next
            draws <- if (!is.null(n_draw)) n_draw else if (is_rvec(df)) n_draw(df) else 1L
            m <- if (is_rvec(df)) as.matrix(df) else matrix(df, ncol = 1L)
            full <- m[rep(seq_len(nrow(m)), length.out = 2L),
                      rep(seq_len(ncol(m)), length.out = draws), drop = FALSE]
            for (extra in list(list(), list(ncp = 0), list(ncp = c(0, 2)))) {
                set.seed(42)
                expected <- as.double(do.call(rt, c(list(n = 2L * draws, df = full), extra)))
                if (is_rvec(df) || !is.null(n_draw))
                    expected <- rvec(matrix(expected, nrow = 2L))
                state_expected <- .Random.seed
                next_expected <- rt(5L, df = 3)
                set.seed(42)
                expect_identical(do.call(rt_rvec, c(list(n = 2L, df = df, n_draw = n_draw), extra)), expected)
                expect_identical(.Random.seed, state_expected)
                expect_identical(rt(5L, df = 3), next_expected)
            }
        }
    }
})

test_that("Student t functions preserve empty outputs and validation", {
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    set.seed(42)
    state_before <- .Random.seed
    for (extra in list(list(), list(ncp = 0), list(ncp = 2))) {
        for (fun in list(dt_rvec, pt_rvec, qt_rvec)) {
            expect_identical(do.call(fun, c(list(numeric(), df = 2), extra)), numeric())
            expect_identical(do.call(fun, c(list(empty, df = 2), extra)), empty)
            expect_identical(do.call(fun, c(list(0.5, df = empty), extra)), empty)
            expect_warning(do.call(fun, c(list(NA_real_, df = 2), extra)), "NAs produced")
            expect_warning(do.call(fun, c(list(0.5, df = -1), extra)), "NAs produced")
            expect_error(do.call(fun, c(list(1:2, df = 1:3), extra)), "Can't recycle")
        }
        expect_identical(do.call(rt_rvec, c(list(n = 0L, df = 2), extra)), numeric())
        expect_identical(do.call(rt_rvec, c(list(n = 0L, df = 2, n_draw = 3L), extra)), empty)
        expect_identical(do.call(rt_rvec, c(list(n = 0L, df = empty), extra)), empty)
        expect_error(do.call(rt_rvec, c(list(n = 2L, df = rvec(1:2), n_draw = 3L), extra)), "has 1 draws")
    }
    for (fun in list(dt_rvec, pt_rvec, qt_rvec, rt_rvec)) {
        for (ncp in list(-1, NA_real_, "a", rvec(1)))
            expect_error(fun(2L, df = 2, ncp = ncp))
    }
    expect_identical(.Random.seed, state_before)
})


test_that("'rt_rvec' preserves RNG advancement before character-input errors", {
    for (extra in list(list(), list(ncp = 0), list(ncp = c(0, 2)))) {
        set.seed(42)
        expect_error(do.call(rt, c(list(n = 2L, df = matrix(c("a", "b"), 2L)), extra)))
        state_expected <- .Random.seed
        next_expected <- rnorm(5L)
        set.seed(42)
        expect_error(do.call(rt_rvec, c(list(n = 2L, df = rvec(c("a", "b"))), extra)))
        expect_identical(.Random.seed, state_expected)
        expect_identical(rnorm(5L), next_expected)
    }
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


test_that("uniform density, probability, and quantile functions preserve alignment", {
    for (kind in c("d", "p", "q")) {
        fun <- get(paste0(kind, "unif_rvec"))
        base_fun <- get(paste0(kind, "unif"), envir = asNamespace("stats"))
        for (log in c(FALSE, TRUE)) {
            values <- if (kind == "q") c(0, 0.1, 0.3, 0.7, 0.9, 1) else c(-Inf, -2, 0, 1, 3, Inf)
            if (kind == "q" && log)
                values <- log(values)
            x <- matrix(values, nrow = 2L)
            lower_bound <- matrix(c(-1, 0), ncol = 1L)
            upper_bound <- matrix(c(1, 2, 3), nrow = 1L)
            cases <- list(list(x, lower_bound, upper_bound),
                          list(x[, 1L, drop = FALSE], matrix(-6:-1, 2L), upper_bound),
                          list(x[1L, , drop = FALSE], lower_bound, matrix(1:6, 2L)))
            for (args in cases) {
                full <- lapply(args, function(m)
                    m[rep(seq_len(nrow(m)), length.out = 2L),
                      rep(seq_len(ncol(m)), length.out = 3L), drop = FALSE])
                for (lower in c(FALSE, TRUE)) {
                    flags <- if (kind == "d") list(log = log) else list(lower.tail = lower, log.p = log)
                    expected <- rvec(do.call(base_fun, c(full, flags)))
                    expect_identical(do.call(fun, c(lapply(args, rvec), flags)), expected)
                }
            }
        }
    }
})

test_that("'runif_rvec' preserves equal bounds, draw order, and RNG state", {
    lower_bounds <- list(c(-1, 0), rvec(c(-1, 0)), rvec(matrix(c(-1, 0, 1), nrow = 1L)),
                         rvec(matrix(c(-1, 0, 1, 0, 1, 2), nrow = 2L)))
    for (lower_bound in lower_bounds) {
        for (upper_bound in list(2, c(2, 3), rvec(matrix(c(2, 0, 1, 3, 4, 5), 2L)))) {
            draws <- max(if (is_rvec(lower_bound)) n_draw(lower_bound) else 1L,
                         if (is_rvec(upper_bound)) n_draw(upper_bound) else 1L)
            full <- lapply(list(lower_bound, upper_bound), function(x) {
                m <- if (is_rvec(x)) as.matrix(x) else matrix(x, ncol = 1L)
                m[rep(seq_len(nrow(m)), length.out = 2L),
                  rep(seq_len(ncol(m)), length.out = draws), drop = FALSE]
            })
            set.seed(42)
            expected <- runif(2L * draws, full[[1L]], full[[2L]])
            if (is_rvec(lower_bound) || is_rvec(upper_bound))
                expected <- rvec(matrix(expected, nrow = 2L))
            state_expected <- .Random.seed
            next_expected <- runif(5L)
            set.seed(42)
            expect_identical(runif_rvec(2L, lower_bound, upper_bound), expected)
            expect_identical(.Random.seed, state_expected)
            expect_identical(runif(5L), next_expected)
        }
    }
    set.seed(42)
    expected <- rvec(matrix(runif(6L, c(-1, 0), 2), nrow = 2L))
    state_expected <- .Random.seed
    set.seed(42)
    expect_identical(runif_rvec(2L, c(-1, 0), 2, n_draw = 3L), expected)
    expect_identical(.Random.seed, state_expected)
})

test_that("uniform functions preserve boundary handling, empty outputs, and validation", {
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    for (fun in list(dunif_rvec, punif_rvec, qunif_rvec)) {
        expect_identical(fun(numeric()), numeric())
        expect_identical(fun(empty), empty)
        expect_identical(fun(0.5, min = empty), empty)
        expect_warning(fun(NA_real_), "NAs produced")
        expect_warning(fun(0.5, min = 2, max = 1), "NAs produced")
        expect_error(fun(1:2, min = 1:3), "Can't recycle")
    }
    set.seed(42)
    state_before <- .Random.seed
    expect_identical(runif_rvec(0L), numeric())
    expect_identical(runif_rvec(0L, n_draw = 3L), empty)
    expect_identical(runif_rvec(0L, min = empty), empty)
    expect_error(runif_rvec(2L, min = rvec(1:2), n_draw = 3L), "has 1 draws")
    expect_error(runif_rvec(2L, min = rvec(matrix(1:4, 2L)), max = rvec(matrix(1:6, 2L))), "Can't align")
    expect_error(runif_rvec(2L, min = "a", n_draw = 3L), "must not be a character vector")
    expect_identical(.Random.seed, state_before)
    lower_bounds <- c(0, 2, -Inf, NA_real_)
    upper_bounds <- c(0, 1, Inf, 1)
    set.seed(42)
    expected <- suppressWarnings(runif(4L, lower_bounds, upper_bounds))
    state_expected <- .Random.seed
    set.seed(42)
    expect_warning(obtained <- runif_rvec(4L, lower_bounds, upper_bounds), "NAs produced")
    expect_identical(obtained, expected)
    expect_identical(.Random.seed, state_expected)
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


test_that("Weibull density, probability, and quantile functions preserve alignment", {
    for (kind in c("d", "p", "q")) {
        fun <- get(paste0(kind, "weibull_rvec"))
        base_fun <- get(paste0(kind, "weibull"), envir = asNamespace("stats"))
        for (log in c(FALSE, TRUE)) {
            values <- if (kind == "q") c(0, 0.1, 0.3, 0.7, 0.9, 1) else c(-Inf, -2, 0, 1, 3, Inf)
            if (kind == "q" && log)
                values <- log(values)
            x <- matrix(values, nrow = 2L)
            shape <- matrix(c(0.5, 2), ncol = 1L)
            scale <- matrix(c(0.5, 1, 2), nrow = 1L)
            cases <- list(list(x, shape, scale),
                          list(x[, 1L, drop = FALSE], matrix(1:6, 2L), scale),
                          list(x[1L, , drop = FALSE], shape, matrix(1:6 / 3, 2L)))
            for (args in cases) {
                full <- lapply(args, function(m)
                    m[rep(seq_len(nrow(m)), length.out = 2L),
                      rep(seq_len(ncol(m)), length.out = 3L), drop = FALSE])
                for (lower in c(FALSE, TRUE)) {
                    flags <- if (kind == "d") list(log = log) else list(lower.tail = lower, log.p = log)
                    expected <- rvec(do.call(base_fun, c(full, flags)))
                    expect_identical(do.call(fun, c(lapply(args, rvec), flags)), expected)
                }
            }
        }
    }
})

test_that("'rweibull_rvec' preserves draw order, degenerate values, and RNG state", {
    parameters <- list(c(0.5, 2), rvec(c(0.5, 2)), rvec(matrix(c(0.5, 1, 2), nrow = 1L)),
                       rvec(matrix(c(0.5, 1, 2, 1, 3, 4), nrow = 2L)))
    for (shape in parameters) {
        for (scale in list(1, c(0, 2), rvec(matrix(c(0, 1, 2, 0, 3, 4), 2L)))) {
            draws <- max(if (is_rvec(shape)) n_draw(shape) else 1L,
                         if (is_rvec(scale)) n_draw(scale) else 1L)
            full <- lapply(list(shape, scale), function(x) {
                m <- if (is_rvec(x)) as.matrix(x) else matrix(x, ncol = 1L)
                m[rep(seq_len(nrow(m)), length.out = 2L),
                  rep(seq_len(ncol(m)), length.out = draws), drop = FALSE]
            })
            set.seed(42)
            expected <- rweibull(2L * draws, full[[1L]], full[[2L]])
            if (is_rvec(shape) || is_rvec(scale))
                expected <- rvec(matrix(expected, nrow = 2L))
            state_expected <- .Random.seed
            next_expected <- rweibull(5L, shape = 2)
            set.seed(42)
            expect_identical(rweibull_rvec(2L, shape, scale), expected)
            expect_identical(.Random.seed, state_expected)
            expect_identical(rweibull(5L, shape = 2), next_expected)
        }
    }
    set.seed(42)
    expected <- rvec(matrix(rweibull(6L, c(0.5, 2), 1), nrow = 2L))
    state_expected <- .Random.seed
    set.seed(42)
    expect_identical(rweibull_rvec(2L, c(0.5, 2), 1, n_draw = 3L), expected)
    expect_identical(.Random.seed, state_expected)
})

test_that("Weibull functions preserve empty outputs and validation", {
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    set.seed(42)
    state_before <- .Random.seed
    for (fun in list(dweibull_rvec, pweibull_rvec, qweibull_rvec)) {
        expect_identical(fun(numeric(), shape = 2), numeric())
        expect_identical(fun(empty, shape = 2), empty)
        expect_identical(fun(0.5, shape = empty), empty)
        expect_identical(fun(0.5, shape = 2, scale = empty), empty)
        expect_warning(fun(NA_real_, shape = 2), "NAs produced")
        expect_warning(fun(0.5, shape = 2, scale = -1), "NAs produced")
        expect_error(fun(1:2, shape = 1:3), "Can't recycle")
    }
    expect_identical(rweibull_rvec(0L, shape = 2), numeric())
    expect_identical(rweibull_rvec(0L, shape = 2, n_draw = 3L), empty)
    expect_identical(rweibull_rvec(0L, shape = empty), empty)
    expect_error(rweibull_rvec(2L, shape = rvec(1:2), n_draw = 3L), "has 1 draws")
    expect_error(rweibull_rvec(2L, shape = rvec(matrix(1:4, 2L)), scale = rvec(matrix(1:6, 2L))), "Can't align")
    expect_error(rweibull_rvec(2L, shape = "a", n_draw = 3L), "must not be a character vector")
    expect_error(rweibull_rvec(2L, shape = 2, scale = "a", n_draw = 3L), "must not be a character vector")
    expect_identical(.Random.seed, state_before)
})


test_that("'nbinom_prob_from_mu' preserves arithmetic and warnings", {
    parameters <- list(c(0, 1), c(Inf, -Inf), c(NA_real_, NaN),
                       c(1e-300, 1e300), c(-1, 1), c(TRUE, FALSE),
                       c(2147483647L, 1L), rvec(c(2147483647L, 1L)),
                       rvec(c(TRUE, FALSE)), rvec(c(Inf, NaN)),
                       rvec(matrix(c(0, 1, Inf, NaN, 1e300, 1e-300), 2L)),
                       rvec(matrix(1:6, 2L)), rvec(matrix(c(0.1, 0.5, 0.9), 1L)),
                       rvec(c(0.1, 0.5)))
    capture <- function(fun, size, mu) {
        warnings <- character()
        value <- withCallingHandlers(fun(size, mu), warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        })
        list(value = value, warnings = warnings)
    }
    for (size in parameters) for (mu in parameters) {
        size <- vec_recycle(size, size = 2L)
        mu <- vec_recycle(mu, size = 2L)
        expected <- capture(function(size, mu) size / (size + mu), size, mu)
        expect_identical(capture(nbinom_prob_from_mu, size, mu), expected)
    }
})

test_that("'nbinom_prob_from_mu' preserves empty results and invalid-input errors", {
    empty <- rvec(matrix(numeric(), nrow = 0L, ncol = 3L))
    expect_identical(nbinom_prob_from_mu(empty, numeric()), empty)
    expect_identical(nbinom_prob_from_mu(numeric(), empty), empty)
    expect_identical(nbinom_prob_from_mu(numeric(), numeric()), numeric())
    inputs <- list(list(rvec(matrix(1:4, 2L)), rvec(matrix(1:6, 2L))),
                   list(rvec(c("a", "b")), c(1, 2)),
                   list(c(1, 2), rvec(c("a", "b"))),
                   list(rvec(1:2), c("a", "b")))
    error_message <- function(fun, args)
        tryCatch(do.call(fun, args), error = function(e) conditionMessage(e))
    for (args in inputs) {
        expected <- error_message(function(size, mu) size / (size + mu), args)
        expect_type(expected, "character")
        expect_identical(error_message(nbinom_prob_from_mu, args), expected)
    }
})


## 'rdist_rvec_1' --------------------------------------------------------------

test_that("'rdist_rvec_1' works with valid rvec input", {
    m <- matrix(1:6, nr = 2)
    lambda <- rvec(m)
    set.seed(0)
    ans_obtained <- rdist_rvec_1(fun = rpois, arg = lambda, n = 2L, n_draw = 3L)
    set.seed(0)
    ans_expected <- rvec_dbl(matrix(rpois(n = 6, lambda = 1:6), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rdist_rvec_1' works with valid non-rvec input", {
    set.seed(0)
    ans_obtained <- rdist_rvec_1(fun = rpois, arg = 1:6, n = 6L, n_draw = NULL)
    set.seed(0)
    ans_expected <- as.double(rpois(n = 6, lambda = 1:6))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rdist_rvec_1' throws appropriate error with invalid inputs", {
    m <- matrix(letters[1:6], nr = 2)
    lambda <- rvec(m)
    expect_error(rdist_rvec_1(fun = rpois, arg = lambda, n = 2L, n_draw = 3L),
                 "Problem with call to function `rpois\\(\\)`.")
})

test_that("'rdist_rvec_1' gives appropriate warning with NAs", {
    m <- matrix(c(1:5, NA), nr = 2)
    lambda <- rvec(m)
    expect_warning(rdist_rvec_1(fun = rpois, arg = lambda, n = 2L, n_draw = 3L),
                   "NAs produced")
})


## 'dist_rvec_2_compact' --------------------------------------------------------------

test_that("'dist_rvec_2_compact' works with valid rvec input - density, both args rvecs", {
    m <- matrix(1:6, nr = 2)
    x <- rvec(m)
    lambda <- rvec(m + 1)
    ans_obtained <- dist_rvec_2_compact(fun = dpois, arg1 = x, arg2 = lambda)
    ans_expected <- rvec(matrix(dpois(x = m, lambda = m + 1), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_2_compact' works with valid rvec input - density, arg1 numeric", {
    m <- matrix(1:6, nr = 2)
    x <- 1:2
    lambda <- rvec(m)
    ans_obtained <- dist_rvec_2_compact(fun = dpois, arg1 = x, arg2 = lambda)
    ans_expected <- rvec(matrix(dpois(x = rep(1:2, 3), lambda = m), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_2_compact' works with valid rvec input - density, arg2 numeric", {
    m <- matrix(1:6, nr = 2)
    x <- rvec(m)
    lambda <- 1:2
    ans_obtained <- dist_rvec_2_compact(fun = dpois, arg1 = x, arg2 = lambda)
    ans_expected <- rvec(matrix(dpois(x = m, lambda = rep(1:2, 3)), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_2_compact' works with valid rvec input - density, arg1, arg2 numeric", {
    x <- 2:1
    lambda <- 1:2
    ans_obtained <- dist_rvec_2_compact(fun = dpois, arg1 = x, arg2 = lambda)
    ans_expected <- dpois(x = 2, lambda = lambda)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_2_compact' throws appropriate error with invalid inputs", {
    m <- matrix(letters[1:6], nr = 2)
    x <- rvec(m)
    lambda <- c(1, 1)
    expect_error(dist_rvec_2_compact(fun = dpois, arg1 = x, arg2 = lambda),
                 "`x` has class")
})

test_that("'dist_rvec_2_compact' throws appropriate error with error in fun", {
    m <- matrix(1:6, nr = 2)
    x <- rvec(m)
    lambda <- c(1, "a")
    expect_error(dist_rvec_2_compact(fun = dpois, arg1 = x, arg2 = lambda),
                 "Non-numeric argument")
})

test_that("'dist_rvec_2_compact' warns about NAs", {
    m <- matrix(1:6, nr = 2)
    x <- rvec(m)
    lambda <- c(1, -1)
    expect_warning(dist_rvec_2_compact(fun = dpois, arg1 = x, arg2 = lambda),
                 "NAs produced")
})


## 'dist_rvec_3_compact' --------------------------------------------------------------

test_that("'dist_rvec_3_compact' works with valid rvec input - density, all args rvecs", {
    m <- matrix(1:6, nr = 2)
    x <- rvec(m)
    mean <- rvec(m + 1)
    sd <- rvec(m + 3)
    set.seed(0)
    ans_obtained <- dist_rvec_3_compact(fun = dnorm, arg1 = x, arg2 = mean, arg3 = sd)
    set.seed(0)
    ans_expected <- rvec(matrix(dnorm(x = m, mean = m + 1, sd = m + 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_3_compact' works with valid rvec input - density, arg2 numeric", {
    m <- matrix(1:6, nr = 2)
    x <- rvec(m)
    mean <- 1:2
    sd <- rvec(m + 3)
    set.seed(0)
    ans_obtained <- dist_rvec_3_compact(fun = dnorm, arg1 = x, arg2 = mean, arg3 = sd)
    set.seed(0)
    ans_expected <- rvec(matrix(dnorm(x = m, mean = rep(1:2, times = 3), sd = m + 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_3_compact' works with valid rvec input - density, arg1, arg2 numeric", {
    x <- 3:4
    mean <- 1:2
    m <- matrix(1:6, nr = 2)
    sd <- rvec(m)
    set.seed(0)
    ans_obtained <- dist_rvec_3_compact(fun = dnorm, arg1 = x, arg2 = mean, arg3 = sd)
    set.seed(0)
    ans_expected <- rvec(matrix(dnorm(x = rep(3:4, times = 3),
                                      mean = rep(1:2, times = 3),
                                      sd = m),
                                nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


test_that("'dist_rvec_3_compact' works with valid rvec input - density, arg1 numeric", {
    m <- matrix(1:6, nr = 2)
    x <- 2:1
    mean <- rvec(m + 1)
    sd <- rvec(m + 3)
    set.seed(0)
    ans_obtained <- dist_rvec_3_compact(fun = dnorm, arg1 = x, arg2 = mean, arg3 = sd)
    set.seed(0)
    ans_expected <- rvec(matrix(dnorm(x = x, mean = m + 1, sd = m + 3), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})


test_that("'dist_rvec_3_compact' works with valid rvec input - density, arg1, arg2, arg3 numeric", {
    x <- 2:1
    mean <- 1:2
    sd <- 1:2
    ans_obtained <- dist_rvec_3_compact(fun = dnorm, arg1 = x, arg2 = mean, arg3 = sd, log = TRUE)
    ans_expected <- dnorm(x = x, mean = mean, sd = sd, log = TRUE)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_3_compact' throws appropriate error with invalid inputs", {
    m <- matrix(1:6, nr = 2)
    x <- rvec(m)
    mean <- 1:2
    sd  <- c("a", 0)
    expect_error(dist_rvec_3_compact(fun = dnorm, arg1 = x, arg2 = mean, arg3 = sd),
                 "Problem with call to function `dnorm\\(\\)`.")
})

test_that("'dist_rvec_3_compact' warns about NAs", {
    m <- matrix(c(1:5, NA), nr = 2)
    x <- rvec(m)
    mean <- 1:2
    sd  <- c(0.1, -0.4)
    expect_warning(dist_rvec_3_compact(fun = dnorm, arg1 = x, arg2 = mean, arg3 = sd),
                   "NAs produced")
})


## 'dist_rvec_4_compact' --------------------------------------------------------------

test_that("'dist_rvec_4_compact' works with valid rvec input - density, all args rvecs", {
    y <- matrix(1:6, nr = 2)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4_compact(fun = dhyper, arg1 = yy, arg2 = yy,
                                arg3 = zz, arg4 = zz)
    ans_expected <- rvec(matrix(dhyper(y, y, z, z), nrow = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4_compact' works with valid rvec input - density, args 2,3,4 rvecs", {
    y <- matrix(1:6, nr = 2)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4_compact(fun = dhyper, arg1 = z, arg2 = yy,
                                arg3 = zz, arg4 = zz)
    ans_expected <- rvec(matrix(dhyper(x = z, m = y, n = z, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4_compact' works with valid rvec input - density, args 1,3,4 rvecs", {
    y <- matrix(1:6, nr = 2)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4_compact(fun = dhyper, arg1 = yy, arg2 = z,
                                arg3 = zz, arg4 = zz)
    ans_expected <- rvec(matrix(dhyper(x = y, m = z, n = z, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4_compact' works with valid rvec input - density, args 1,2,4 rvecs", {
    y <- matrix(1:6, nr = 2)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4_compact(fun = dhyper, arg1 = yy, arg2 = yy,
                                arg3 = z, arg4 = zz)
    ans_expected <- rvec(matrix(dhyper(x = y, m = y, n = z, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4_compact' works with valid rvec input - density, args 1,2,3 rvecs", {
    y <- matrix(1:6, nr = 2)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4_compact(fun = dhyper, arg1 = yy, arg2 = yy,
                                arg3 = zz, arg4 = z)
    ans_expected <- rvec(matrix(dhyper(x = y, m = y, n = z, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4_compact' works with valid rvec input - density, args 3,4 rvecs", {
    y <- matrix(1:6, nr = 2)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4_compact(fun = dhyper, arg1 = z, arg2 = z,
                                arg3 = yy, arg4 = yy)
    ans_expected <- rvec(matrix(dhyper(x = z, m = z, n = y, k = y), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4_compact' works with valid rvec input - density, args 2,4 rvecs", {
    y <- matrix(1, nr = 2, nc = 3)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4_compact(fun = dhyper, arg1 = z, arg2 = zz,
                                arg3 = z, arg4 = yy)
    ans_expected <- rvec(matrix(dhyper(x = z, m = z, n = z, k = y), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4_compact' works with valid rvec input - density, args 1,4 rvecs", {
    y <- matrix(1, nr = 2, nc = 3)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4_compact(fun = dhyper, arg1 = yy, arg2 = z,
                                arg3 = z, arg4 = yy)
    ans_expected <- rvec(matrix(dhyper(x = y, m = z, n = z, k = y), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4_compact' works with valid rvec input - density, args 1,3 rvecs", {
    y <- matrix(1, nr = 2, nc = 3)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4_compact(fun = dhyper, arg1 = yy, arg2 = z,
                                arg3 = zz, arg4 = z)
    ans_expected <- rvec(matrix(dhyper(x = y, m = z, n = z, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4_compact' works with valid rvec input - density, args 2,3 rvecs", {
    y <- matrix(1, nr = 2, nc = 3)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4_compact(fun = dhyper, arg1 = z, arg2 = yy,
                                arg3 = zz, arg4 = z)
    ans_expected <- rvec(matrix(dhyper(x = z, m = y, n = z, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4_compact' works with valid rvec input - density, arg 4 rvec", {
    y <- matrix(1, nr = 2, nc = 3)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4_compact(fun = dhyper, arg1 = z, arg2 = z,
                                arg3 = z, arg4 = yy)
    ans_expected <- rvec(matrix(dhyper(x = z, m = z, n = z, k = y), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4_compact' works with valid rvec input - density, arg 3 rvec", {
    y <- matrix(1, nr = 2, nc = 3)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4_compact(fun = dhyper, arg1 = z, arg2 = z,
                                arg3 = yy, arg4 = z)
    ans_expected <- rvec(matrix(dhyper(x = z, m = z, n = y, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4_compact' works with valid rvec input - density, arg 2 rvec", {
    y <- matrix(1, nr = 2, nc = 3)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4_compact(fun = dhyper, arg1 = z, arg2 = yy,
                                arg3 = z, arg4 = z)
    ans_expected <- rvec(matrix(dhyper(x = z, m = y, n = z, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4_compact' works with valid rvec input - density, arg 1 rvec", {
    y <- matrix(1, nr = 2, nc = 3)
    yy <- rvec(y)
    z <- matrix(1:2, nr = 2)
    zz <- rvec(z)
    ans_obtained <- dist_rvec_4_compact(fun = dhyper, arg1 = yy, arg2 = z,
                                arg3 = z, arg4 = z)
    ans_expected <- rvec(matrix(dhyper(x = y, m = z, n = z, k = z), nr = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'dist_rvec_4_compact' throws appropriate error with invalid inputs", {
    m <- matrix(1:6, nr = 2)
    x <- rvec(m)
    n <- 1:2
    k <- as.character(1:2)
    expect_error(dist_rvec_4_compact(fun = dhyper, arg1 = x, arg2 = m,
                             arg3 = n, arg4 = "k"),
                 "Problem with call to function `dhyper\\(\\)`.")
})

test_that("'dist_rvec_4_compact' warns about NAs", {
  x <- rvec(matrix(c(1:5, NA), nr = 2))
  m <- 2:1
  n <- 1:2
  k <- c(1, -2)
  expect_warning(dist_rvec_4_compact(fun = dhyper, arg1 = x, arg2 = m,
                             arg3 = n, arg4 = k),
                 "NAs produced")
})


test_that("random generators reject nonvector parameters with explicit draw counts", {
    cases <- list(
        rbeta_rvec = list(shape1 = 2, shape2 = 3),
        rbinom_rvec = list(size = 2, prob = 0.5),
        rcauchy_rvec = list(location = 0, scale = 1),
        rchisq_rvec = list(df = 2),
        rexp_rvec = list(rate = 1),
        rf_rvec = list(df1 = 2, df2 = 3),
        rgamma_rvec = list(shape = 2, rate = 1),
        rgeom_rvec = list(prob = 0.5),
        rhyper_rvec = list(m = 2, n = 3, k = 1),
        rlnorm_rvec = list(meanlog = 0, sdlog = 1),
        rnbinom_rvec = list(size = 2, prob = 0.5),
        rnorm_rvec = list(mean = 0, sd = 1),
        rpois_rvec = list(lambda = 1),
        rt_rvec = list(df = 2),
        runif_rvec = list(min = 0, max = 1),
        rweibull_rvec = list(shape = 2, scale = 1)
    )
    withr::local_seed(1)
    state_before <- .Random.seed
    for (nm in names(cases)) {
        fun <- get(nm)
        parameters <- cases[[nm]]
        count <- if (nm == "rhyper_rvec") list(nn = 2L) else list(n = 2L)
        for (parameter in names(parameters)) {
            for (invalid in list(list(1), matrix(1))) {
                args <- parameters
                args[[parameter]] <- invalid
                error <- expect_error(do.call(fun, c(count, args, list(n_draw = 3L))),
                                      paste0("`", parameter, "` is not a vector or rvec."),
                                      fixed = TRUE)
                expect_match(conditionMessage(error),
                             paste0("`", parameter, "` has class"), fixed = TRUE)
                expect_match(conditionMessage(error), class(invalid)[1L], fixed = TRUE)
            }
        }
    }
    expect_identical(.Random.seed, state_before)
})


test_that("'rdist_rvec_3' consolidates missing-value warnings", {
    withr::local_seed(1)
    m <- c(-1, 0)
    n <- c(2, 2)
    k <- c(1, 1)
    for (nd in list(NULL, 3L)) {
        warnings <- character()
        actual <- withCallingHandlers(
            rdist_rvec_3(fun = rhyper, arg1 = m, arg2 = n,
                         arg3 = k, n = 2L, n_draw = nd),
            warning = function(w) {
                warnings <<- c(warnings, conditionMessage(w))
                invokeRestart("muffleWarning")
            })
        expect_identical(warnings, "NAs produced")
        values <- if (is.null(nd)) actual else as.matrix(actual)
        expect_identical(as.vector(is.na(values)), rep(c(TRUE, FALSE), length.out = length(values)))
        expect_true(all(values[!is.na(values)] == 0))
        if (is.null(nd))
            expect_type(actual, "double")
        else {
            expect_s3_class(actual, "rvec_dbl")
            expect_identical(dim(values), c(2L, 3L))
        }
    }
})


test_that("'rdist_rvec_3' identifies the failing generator in errors", {
    m <- "invalid"
    n <- 2
    k <- 1
    for (nd in list(NULL, 3L)) {
        error <- expect_error(
            rdist_rvec_3(fun = rhyper, arg1 = m, arg2 = n,
                         arg3 = k, n = 2L, n_draw = nd),
            "Problem with call to function `rhyper()`.", fixed = TRUE)
        expect_match(conditionMessage(error), "invalid arguments", fixed = TRUE)
    }
})
