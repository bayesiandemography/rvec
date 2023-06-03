
## 'draws_all' -------------------------------------------------------------

test_that("'draws_all' works with rvec_dbl when nrow > 0", {
    set.seed(0)
    m <- matrix(sample(c(TRUE, FALSE), size = 10, replace = TRUE),
                nr = 5)
    x <- rvec(m)
    ans_obtained <- draws_all(x)
    ans_expected <- apply(m, 1, all)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'draws_all' works with rvec_dbl when nrow > 0 - numeric", {
    set.seed(0)
    m <- matrix(sample(c(1, 0), size = 10, replace = TRUE),
                nr = 5)
    x <- rvec(m)
    expect_warning(draws_all(x),
                   "Coercing from type")
    suppressWarnings(ans_obtained <- draws_all(x))
    suppressWarnings(ans_expected <- apply(m, 1, all))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'draws_all' works with rvec_int when nrow == 0", {
    set.seed(0)
    m <- matrix(integer(), nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- draws_all(x)
    ans_expected <- TRUE
    expect_identical(ans_obtained, ans_expected)
})

test_that("'draws_all' preserves names", {
    set.seed(0)
    m <- matrix(TRUE, nr = 5)
    rownames(m) <- 1:5
    x <- rvec(m)
    ans <- draws_all(x)
    expect_identical(names(ans), as.character(1:5))
})

test_that("'draws_all' throws correct error with rvec_chr", {
    m <- matrix("a", nrow = 5, ncol = 1)
    x <- rvec(m)
    expect_error(draws_all(x),
                 "`all\\(\\)` not defined for character.")
})


## 'draws_any' -------------------------------------------------------------

test_that("'draws_any' works with rvec_dbl when nrow > 0", {
    set.seed(0)
    m <- matrix(sample(c(TRUE, FALSE), size = 10, replace = TRUE),
                nr = 5)
    x <- rvec(m)
    ans_obtained <- draws_any(x)
    ans_expected <- apply(m, 1, any)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'draws_any' works with rvec_dbl when nrow > 0 - numeric", {
    set.seed(0)
    m <- matrix(sample(c(1, 0), size = 10, replace = TRUE),
                nr = 5)
    x <- rvec(m)
    expect_warning(draws_any(x),
                   "Coercing from type")
    suppressWarnings(ans_obtained <- draws_any(x))
    suppressWarnings(ans_expected <- apply(m, 1, any))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'draws_any' works with rvec_int when nrow == 0", {
    set.seed(0)
    m <- matrix(integer(), nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- draws_any(x)
    ans_expected <- FALSE
    expect_identical(ans_obtained, ans_expected)
})

test_that("'draws_any' preserves names", {
    set.seed(0)
    m <- matrix(TRUE, nr = 5)
    rownames(m) <- 1:5
    x <- rvec(m)
    ans <- draws_any(x)
    expect_identical(names(ans), as.character(1:5))
})

test_that("'draws_any' throws correct error with rvec_chr", {
    m <- matrix("a", nrow = 5, ncol = 1)
    x <- rvec(m)
    expect_error(draws_any(x),
                 "`any\\(\\)` not defined for character.")
})


## 'draws_median' -------------------------------------------------------------

test_that("'draws_median' works with rvec_dbl when nrow > 0", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    x <- rvec(m)
    ans_obtained <- draws_median(x)
    ans_expected <- apply(m, 1, median)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'draws_median' works with rvec_int when nrow == 0", {
    set.seed(0)
    m <- matrix(integer(), nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- draws_median(x)
    ans_expected <- NA_real_
    expect_identical(ans_obtained, ans_expected)
})

test_that("'draws_median' preserves names", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    rownames(m) <- 1:5
    x <- rvec(m)
    ans <- draws_median(x)
    expect_identical(names(ans), as.character(1:5))
})

test_that("'draws_median' works with rvec_lgl", {
    m <- matrix(TRUE, nrow = 5, ncol = 1)
    x <- rvec(m)
    ans_obtained <- draws_median(x)
    ans_expected <- rep(1, 5)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_median' throws correct error with rvec_chr", {
    m <- matrix("a", nrow = 5, ncol = 1)
    x <- rvec(m)
    expect_error(draws_median(x),
                 "Median not defined for character.")
})


## 'draws_mean' ---------------------------------------------------------------

test_that("'draws_mean' works with rvec_dbl when nrow > 0", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    x <- rvec(m)
    ans_obtained <- draws_mean(x)
    ans_expected <- apply(m, 1, mean)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_mean' works with rvec_int when nrow == 0", {
    set.seed(0)
    m <- matrix(integer(), nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- draws_mean(x)
    ans_expected <- NaN
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_mean' preserves names", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    rownames(m) <- 1:5
    x <- rvec(m)
    ans <- draws_mean(x)
    expect_equal(names(ans), as.character(1:5))
})

test_that("'draws_mean' works with rvec_lgl", {
    m <- matrix(TRUE, nrow = 5, ncol = 1)
    x <- rvec(m)
    ans_obtained <- draws_mean(x)
    ans_expected <- rep(1, 5)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_mean' throws correct error with rvec_chr", {
    m <- matrix("a", nrow = 5, ncol = 1)
    x <- rvec(m)
    expect_error(draws_mean(x),
                 "Mean not defined for character.")
})


## 'draws_mode' ---------------------------------------------------------------

test_that("'draws_mode' works with rvec_chr when nrow > 0", {
    set.seed(0)
    m <- matrix("a", nr = 3, nc = 3)
    x <- rvec(m)
    ans_obtained <- draws_mode(x)
    ans_expected <- rep("a", 3)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_mode' works with rvec_dbl when nrow > 0", {
    m <- matrix(1:20 + 0.1, nr = 5)
    x <- rvec(m)
    ans_obtained <- draws_mode(x)
    ans_expected <- rep(NA_real_, 5)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_mode' works with rvec_int when nrow == 0", {
    set.seed(0)
    m <- matrix(integer(), nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- draws_mode(x)
    ans_expected <- NA_integer_
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_mode' preserves names", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    rownames(m) <- 1:5
    x <- rvec(m)
    ans <- draws_mode(x)
    expect_equal(names(ans), as.character(1:5))
})

test_that("'draws_mode' works with rvec_lgl", {
    m <- matrix(TRUE, nrow = 5, ncol = 1)
    x <- rvec(m)
    ans_obtained <- draws_mode(x)
    ans_expected <- rep(TRUE, 5)
    expect_equal(ans_obtained, ans_expected)
})


## 'draws_quantile' -----------------------------------------------------------

test_that("'draws_quantile' works with rvec_dbl when nrow > 0", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    y <- rvec(m)
    ans_obtained <- draws_quantile(y)
    ans_expected <- apply(m, 1, quantile, prob = c(0.025, 0.5, 0.975))
    ans_expected <- tibble::as_tibble(t(ans_expected))
    names(ans_expected) <- sub("%", "", names(ans_expected))
    names(ans_expected) <- paste("y", names(ans_expected), sep = "_")
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_median' works with rvec_int when nrow == 0", {
    set.seed(0)
    m <- matrix(integer(), nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- draws_quantile(x)
    ans_expected <- tibble::tibble("x_2.5" = NA_real_,
                                   "x_50" = NA_real_,
                                   "x_97.5" = NA_real_) 
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_quantile' works with rvec_lgl", {
    m <- matrix(TRUE, nrow = 5, ncol = 1)
    y <- rvec(m)
    ans_obtained <- draws_quantile(x = y)
    ans_expected <- apply(m, 1, quantile, prob = c(0.025, 0.5, 0.975))
    ans_expected <- tibble::as_tibble(t(ans_expected))
    names(ans_expected) <- sub("%", "", names(ans_expected))
    names(ans_expected) <- paste0("y_", names(ans_expected))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_median' throws correct error with rvec_chr", {
    m <- matrix("a", nrow = 5, ncol = 1)
    x <- rvec(m)
    expect_error(draws_quantile(x),
                 "Quantiles not defined for character.")
})


## 'draws_fun' ----------------------------------------------------------------

test_that("'draws_fun' works with rvec_dbl when nrow > 0 and return value is scalar", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    x <- rvec(m)
    ans_obtained <- draws_fun(x, fun = mad)
    ans_expected <- apply(m, 1, mad)
    expect_equal(ans_obtained, ans_expected)
})


test_that("'draws_fun' works with rvec_dbl when nrow > 0  and return value is vector", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    x <- rvec(m)
    ans_obtained <- draws_fun(x, fun = range)
    ans_expected <- apply(m, 1, range, simplify = FALSE)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_median' works with rvec_int when nrow == 0", {
    set.seed(0)
    m <- matrix(integer(), nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- draws_fun(x, fun = range)
    ans_expected <- list()
    expect_equal(ans_obtained, ans_expected)
})

