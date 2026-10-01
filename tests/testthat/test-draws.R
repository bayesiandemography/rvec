
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


test_that("draws_ci selects the point estimate without changing intervals", {
    x <- rvec(rbind(a = c(0, 1, 2, 3, 24), b = c(1, 1, 1, 2, 10)))
    for (width in list(0.95, c(0.5, 0.8, 0.95))) {
        default <- draws_ci(x, width = width, prefix = "var")
        median <- draws_ci(x, width = width, prefix = "var", point = "median")
        mean <- draws_ci(x, width = width, prefix = "var", point = "mean")
        expect_identical(default, median)
        expect_equal(unname(mean$var.mid), c(6, 3))
        expect_false(isTRUE(all.equal(mean$var.mid, median$var.mid)))
        mid <- length(width) + 1L
        expect_identical(mean[-mid], median[-mid])
        expect_identical(names(mean), names(median))
    }
    expect_identical(draws_ci(x, 0.8, "var", FALSE),
                     draws_ci(x, width = 0.8, prefix = "var", point = "median"))
    expect_identical(names(draws_ci(x, point = "mean")),
                     c("x.lower", "x.mid", "x.upper"))
})


test_that("draws_ci mean supports numeric types and missing values", {
    for (values in list(c(0, 0, 1, 9), c(0L, 0L, 1L, 9L),
                        c(FALSE, FALSE, FALSE, TRUE))) {
        x <- rvec(rbind(values, replace(values, 2L, NA)))
        result <- draws_ci(x, point = "mean")
        expect_equal(unname(result$x.mid), c(mean(values), NA_real_))
        result <- draws_ci(x, point = "mean", na_rm = TRUE)
        expect_equal(unname(result$x.mid), c(mean(values), mean(values[-2L])))
        median <- draws_ci(x, na_rm = TRUE)
        expect_identical(result[c(1, 3)], median[c(1, 3)])
    }
    x <- rvec(matrix(NA_real_, nrow = 1, ncol = 4))
    expect_true(is.na(draws_ci(x, point = "mean")$x.mid))
    expect_true(is.nan(draws_ci(x, point = "mean", na_rm = TRUE)$x.mid))
})


test_that("draws_ci mean preserves the empty input result structure", {
    x <- rvec(matrix(integer(), nrow = 0, ncol = 5))
    expect_identical(draws_ci(x, point = "mean"),
                     tibble::tibble(x.lower = NA_real_, x.mid = NaN,
                                    x.upper = NA_real_))
})


test_that("draws_ci validates point and continues to reject character inputs", {
    x <- rvec(c(1, 2, 3))
    expect_error(draws_ci(x, point = "mode"), "arg.*should be one of")
    expect_error(draws_ci(x, point = "m"), "arg.*should be one of")
    expect_identical(draws_ci(x, point = "mea"), draws_ci(x, point = "mean"))
    expect_error(draws_ci(rvec_chr("a"), point = "mean"),
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

test_that("'draws_quantile' throws correct error with rvec_chr", {
    m <- matrix("a", nrow = 5, ncol = 1)
    x <- rvec(m)
    expect_error(draws_quantile(x),
                 "Quantiles not defined for character.")
})


## 'draws_sd' -----------------------------------------------------------------

test_that("'draws_sd' works with rvec_dbl when nrow > 0", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    y <- rvec(m)
    ans_obtained <- draws_sd(y)
    ans_expected <- apply(m, 1, sd)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_sd' works with rvec_int when nrow == 0", {
    set.seed(0)
    m <- matrix(integer(), nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- draws_sd(x)
    ans_expected <- NA_real_
    expect_identical(ans_obtained, ans_expected)
})

test_that("'draws_sd' preserves names", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    rownames(m) <- 1:5
    x <- rvec(m)
    ans <- draws_sd(x)
    expect_identical(names(ans), as.character(1:5))
})

test_that("'draws_sd' works with rvec_lgl", {
    m <- matrix(TRUE, nrow = 5, ncol = 2)
    x <- rvec(m)
    ans_obtained <- draws_sd(x)
    ans_expected <- rep(0, 5)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_sd' throws correct error with rvec_chr", {
    m <- matrix("a", nrow = 5, ncol = 1)
    x <- rvec(m)
    expect_error(draws_sd(x),
                 "Standard deviation not defined for character.")
})


## 'draws_var' -----------------------------------------------------------------

test_that("'draws_var' works with rvec_dbl when nrow > 0", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    y <- rvec(m)
    ans_obtained <- draws_var(y)
    ans_expected <- apply(m, 1, var)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_var' works with rvec_int when nrow == 0", {
    set.seed(0)
    m <- matrix(integer(), nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- draws_var(x)
    ans_expected <- NA_real_
    expect_identical(ans_obtained, ans_expected)
})

test_that("'draws_var' preserves names", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    rownames(m) <- 1:5
    x <- rvec(m)
    ans <- draws_var(x)
    expect_identical(names(ans), as.character(1:5))
})

test_that("'draws_var' works with rvec_lgl", {
    m <- matrix(TRUE, nrow = 5, ncol = 2)
    x <- rvec(m)
    ans_obtained <- draws_var(x)
    ans_expected <- rep(0, 5)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_var' throws correct error with rvec_chr", {
    m <- matrix("a", nrow = 5, ncol = 1)
    x <- rvec(m)
    expect_error(draws_var(x),
                 "Variance not defined for character.")
})


## 'draws_cv' -----------------------------------------------------------------

test_that("'draws_cv' works with rvec_dbl when nrow > 0", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    y <- rvec(m)
    ans_obtained <- draws_cv(y)
    ans_expected <- apply(m, 1, function(x) sd(x) / mean(x))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_cv' works with rvec_int when nrow == 0", {
    set.seed(0)
    m <- matrix(integer(), nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- draws_cv(x)
    ans_expected <- NA_real_
    expect_identical(ans_obtained, ans_expected)
})

test_that("'draws_cv' preserves names", {
    set.seed(0)
    m <- matrix(rnorm(20), nr = 5)
    rownames(m) <- 1:5
    x <- rvec(m)
    ans <- draws_cv(x)
    expect_identical(names(ans), as.character(1:5))
})

test_that("'draws_cv' works with rvec_lgl", {
    m <- matrix(TRUE, nrow = 5, ncol = 2)
    x <- rvec(m)
    ans_obtained <- draws_cv(x)
    ans_expected <- rep(0, 5)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_cv' returns NA_real_ when mean is 0", {
    m <- matrix(c(-1, 1), nrow = 1, ncol = 2)
    x <- rvec(m)
    ans_obtained <- draws_cv(x)
    ans_expected <- NA_real_
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draws_cv' throws correct error with rvec_chr", {
    m <- matrix("a", nrow = 5, ncol = 1)
    x <- rvec(m)
    expect_error(draws_cv(x),
                 "Coefficient of variation not defined for character.")
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




test_that("double draw summaries retain results without coercion copies", {
    functions <- list(draws_median, draws_mean, draws_sd, draws_var)
    matrix_functions <- list(matrixStats::rowMedians, matrixStats::rowMeans2,
                             matrixStats::rowSds, matrixStats::rowVars)
    for (v in list(c(0, -0, NA_real_, NaN), c(Inf, -Inf, 1, 1e300),
                   c(1e16, 1, -1e16, 1))) {
        m <- matrix(v, 2L, dimnames = list(c("a", "b"), NULL))
        x <- rvec(m)
        before <- serialize(x, NULL)
        for (remove in c(FALSE, TRUE)) {
            for (i in seq_along(functions)) {
                expected <- matrix_functions[[i]](1 * m, na.rm = remove)
                names(expected) <- rownames(m)
                expect_identical(functions[[i]](x, na_rm = remove), expected)
            }
            expected <- matrixStats::rowSds(1 * m, na.rm = remove) /
                matrixStats::rowMeans2(1 * m, na.rm = remove)
            expected[matrixStats::rowMeans2(1 * m, na.rm = remove) == 0] <- NA_real_
            names(expected) <- rownames(m)
            expect_identical(draws_cv(x, na_rm = remove), expected)
            expect_identical(sd(x, na.rm = remove),
                             rvec(matrix(matrixStats::colSds(1 * m, na.rm = remove), 1L)))
            expect_identical(var(x, na.rm = remove),
                             rvec(matrix(matrixStats::colVars(1 * m, na.rm = remove), 1L)))
        }
        expect_identical(serialize(x, NULL), before)
    }
})

test_that("draws_mode preserves ties, missing values, types, and input data", {
    for (kind in c("dbl", "int", "lgl", "chr")) {
        constructor <- get(paste0("rvec_", kind))
        values <- switch(kind, dbl = c(1.5, 2.5), int = c(1L, 2L),
                         lgl = c(TRUE, FALSE), chr = c("b", "a"))
        a <- values[1L]
        b <- values[2L]
        m <- rbind(unique = c(a, a, b, NA), tied = c(a, a, b, b),
                   missing = c(NA, NA, a, b), missing_tie = c(a, a, NA, NA))
        x <- constructor(m)
        expect_identical(draws_mode(x),
                         setNames(c(a, values[NA_integer_], values[NA_integer_], values[NA_integer_]),
                                  rownames(m)))
        expect_identical(draws_mode(x, na_rm = TRUE),
                         setNames(c(a, values[NA_integer_], values[NA_integer_], a), rownames(m)))
        expect_identical(vctrs::field(x, "data"), m)
        empty <- constructor(matrix(values[integer()], 0L, 3L))
        expect_identical(draws_mode(empty), values[NA_integer_])
        all_missing <- constructor(matrix(rep(values[NA_integer_], 6L), 2L, 3L))
        expect_identical(draws_mode(all_missing), rep(values[NA_integer_], 2L))
        warnings <- character()
        result <- withCallingHandlers(draws_mode(all_missing, na_rm = TRUE),
                                      warning = function(w) {
                                          warnings <<- c(warnings, conditionMessage(w))
                                          invokeRestart("muffleWarning")
                                      })
        expect_identical(result, rep(values[NA_integer_], 2L))
        expect_identical(warnings, rep("no non-missing arguments to max; returning -Inf", 2L))
    }
})

test_that("draws_mode retains non-finite modes", {
    x <- rvec_dbl(rbind(infinite = c(Inf, Inf, 1),
                        negative = c(-Inf, -Inf, 1), nan = c(NaN, NaN, 1)))
    expect_identical(draws_mode(x), c(infinite = Inf, negative = -Inf, nan = NaN))
    expect_identical(draws_mode(x, na_rm = TRUE), c(infinite = Inf, negative = -Inf, nan = 1))
})
