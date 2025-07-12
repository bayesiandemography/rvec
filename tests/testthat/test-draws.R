
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


## 'draws_ci' -----------------------------------------------------------

test_that("'draws_ci' works with rvec_dbl when nrow > 0 - no width, prefix supplied, width length 1", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    y <- rvec(m)
    ans_obtained <- draws_ci(y)
    ans_expected <- apply(m, 1, quantile, prob = c(0.025, 0.5, 0.975))
    ans_expected <- tibble::as_tibble(t(ans_expected))
    names(ans_expected) <- c("y.lower", "y.mid", "y.upper")
    expect_equal(ans_obtained, ans_expected)
})


test_that("'draws_ci' works with rvec_dbl when nrow > 0 - prefix, length 1 width supplied", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    y <- rvec(m)
    ans_obtained <- draws_ci(y, width = 0.8, prefix = "var")
    ans_expected <- apply(m, 1, quantile, prob = c(0.1, 0.5, 0.9))
    ans_expected <- tibble::as_tibble(t(ans_expected))
    names(ans_expected) <- c("var.lower", "var.mid", "var.upper")
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_ci' works with rvec_dbl when nrow > 0 - prefix, length 2 width supplied", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    y <- rvec(m)
    ans_obtained <- draws_ci(y, width = c(0.8, 0.9), prefix = "var")
    ans_expected <- apply(m, 1, quantile, prob = c(0.05, 0.1, 0.5, 0.9, 0.95))
    ans_expected <- tibble::as_tibble(t(ans_expected))
    names(ans_expected) <- c("var.lower", "var.lower1", "var.mid", "var.upper1", "var.upper")
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_ci' works with rvec_dbl when nrow > 0 - prefix, length 3 width supplied", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    y <- rvec(m)
    ans_obtained <- draws_ci(y, width = c(0.8, 0.9, 1), prefix = "var")
    ans_expected <- apply(m, 1, quantile, prob = c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1))
    ans_expected <- tibble::as_tibble(t(ans_expected))
    names(ans_expected) <- c("var.lower", "var.lower1", "var.lower2",
                             "var.mid",
                             "var.upper2", "var.upper1", "var.upper")
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_ci' works with rvec_int when nrow == 0", {
    set.seed(0)
    m <- matrix(integer(), nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- draws_ci(x)
    ans_expected <- tibble::tibble("x.lower" = NA_real_,
                                   "x.mid" = NA_real_,
                                   "x.upper" = NA_real_) 
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_ci' throws correct error with rvec_chr", {
    expect_error(draws_ci(rvec_chr("a")),
                 "Credible intervals not defined for character.")
})


## 'draws_max' ----------------------------------------------------------------

test_that("'draws_max' works with rvec_dbl when nrow > 0", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    y <- rvec(m)
    ans_obtained <- draws_max(y)
    ans_expected <- apply(m, 1, max)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_max' works with rvec_int when nrow == 0", {
    set.seed(0)
    m <- matrix(integer(), nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- suppressWarnings(draws_max(x))
    ans_expected <- -Inf
    expect_equal(ans_obtained, ans_expected)
    expect_warning(draws_max(x),
                   "`n_draw` is 0: returning \"-Inf\"")
})

test_that("'draws_max' works with rvec_lgl", {
    m <- matrix(TRUE, nrow = 5, ncol = 1)
    y <- rvec(m)
    ans_obtained <- draws_max(x = y)
    ans_expected <- apply(m, 1, max)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_median' throws correct error with rvec_chr", {
    m <- matrix("a", nrow = 5, ncol = 1)
    x <- rvec(m)
    expect_error(draws_max(x),
                 "Maximum not defined for character.")
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


## 'draws_min' ----------------------------------------------------------------

test_that("'draws_min' works with rvec_dbl when nrow > 0", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    y <- rvec(m)
    ans_obtained <- draws_min(y)
    ans_expected <- apply(m, 1, min)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_min' works with rvec_int when nrow == 0", {
    set.seed(0)
    m <- matrix(integer(), nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- suppressWarnings(draws_min(x))
    ans_expected <- Inf
    expect_equal(ans_obtained, ans_expected)
    expect_warning(draws_min(x),
                   "`n_draw` is 0: returning \"Inf\"")
})

test_that("'draws_min' works with rvec_lgl", {
    m <- matrix(TRUE, nrow = 5, ncol = 1)
    y <- rvec(m)
    ans_obtained <- draws_min(x = y)
    ans_expected <- apply(m, 1, min)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_median' throws correct error with rvec_chr", {
    m <- matrix("a", nrow = 5, ncol = 1)
    x <- rvec(m)
    expect_error(draws_min(x),
                 "Minimum not defined for character.")
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
    ans_obtained <- draws_quantile(y, probs = c(0.025, 0.5, 0.975))
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
                                   "x_25" = NA_real_,
                                   "x_50" = NA_real_,
                                   "x_75" = NA_real_,
                                   "x_97.5" = NA_real_) 
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_quantile' works with rvec_lgl", {
    m <- matrix(TRUE, nrow = 5, ncol = 1)
    y <- rvec(m)
    ans_obtained <- draws_quantile(x = y)
    ans_expected <- apply(m, 1, quantile, prob = c(0.025, 0.25, 0.5, 0.75, 0.975))
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


## 'prob' ---------------------------------------------------------------------

test_that("'prob' works with no NAs", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    x <- rvec(m)
    ans_obtained <- prob(x > 0)
    ans_expected <- apply(m > 0, 1, mean)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'prob' works when nrow == 0", {
    set.seed(0)
    m <- matrix(logical(), nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- prob(x)
    ans_expected <- NaN
    expect_equal(ans_obtained, ans_expected)
})

test_that("'prob' works with rvec_lgl", {
    m <- matrix(TRUE, nrow = 5, ncol = 1)
    x <- rvec(m)
    ans_obtained <- prob(x)
    ans_expected <- rep(1, 5)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'prob' works with NA", {
    m <- matrix(c(T,NA,T,T), nrow = 2, ncol = 2)
    x <- rvec(m)
    ans_obtained <- prob(x)
    ans_expected <- c(1,NA)
    expect_equal(ans_obtained, ans_expected)
    ans_obtained <- prob(x, na_rm = TRUE)
    ans_expected <- c(1,1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'prob' works with logical vector", {
  x <- c(TRUE, FALSE, NA)
  ans_obtained <- prob(x)
  ans_expected <- c(1, 0, NA)
  expect_equal(ans_obtained, ans_expected)
})


