

test_that("var.default works with x only", {
    expect_identical(var(1:5), stats::var(1:5))
})

test_that("var.default works with non-rvec x and non-rvec y", {
    expect_identical(var(1:5, 5:1), stats::var(1:5, 5:1))
})

test_that("var.default works with non-rvec x and rvec y", {
    expect_identical(var(1:5, rvec(matrix(1))),
                     var_rvec_nonrvec(rvec(matrix(1)), 1:5, na.rm = FALSE))
})

test_that("var.rvec works with single rvec", {
    x <- rvec(matrix(1:6, 3))
    expect_identical(var(x), var_rvec(x, na.rm = FALSE))
})

test_that("var.rvec works with two rvecs", {
    x <- rvec(matrix(1:6, 3))
    expect_identical(var(x, x),
                     var_rvec_rvec(x, x, na.rm = FALSE, use = "everything"))
})

test_that("var.rvec works with rvec and non-rvec", {
    x <- rvec(matrix(1:6, 3))
    y <- -3
    expect_identical(var(x, y),
                     var_rvec_nonrvec(e1 = x,
                                      e2 = y,
                                      nm_e2 = "y",
                                      na.rm = FALSE,
                                      use = "everything"))
})

test_that("var.rvec throws appopriate error with character", {
    expect_error(var(rvec("a")),
                 "Variance not defined for character vectors.")
})

test_that("'var_rvec' works with valid inputs, nrow > 0", {
    set.seed(0)
    m <- matrix(rnorm(10), nr = 5)
    x <- rvec(m)
    ans_obtained <- var_rvec(x, na.rm = FALSE)
    ans_expected <- rvec(matrix(c(var(m[,1]), var(m[,2])), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'var_rvec' works with valid inputs, nrow == 0", {
    m <- matrix(0, nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- var_rvec(x, na.rm = FALSE)
    ans_expected <- rvec(matrix(rep(NA_real_, 5), nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'var_rvec_rvec' works with valid inputs, nrow > 0", {
    set.seed(0)
    m <- matrix(rnorm(10), nr = 5)
    x <- rvec(m)
    ans_obtained <- var_rvec_rvec(x = x, y = x, na.rm = FALSE, use = "everything")
    ans_expected <- rvec(matrix(c(var(m[,1], m[,1]), var(m[,2], m[,2])), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'var_rvec_rvec' works with valid inputs, nrow == 0", {
    m <- matrix(0, nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- var_rvec_rvec(x = x, y = x, na.rm = FALSE, use = "everything")
    ans_expected <- rvec_dbl(matrix(c(NA, NA, NA, NA, NA), nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'var_rvec_rvec' preserves use and missing-value behavior", {
    x_m <- matrix(c(1, NA, 3, 4, 5, 6), nrow = 3)
    y_m <- matrix(c(6, 5, NA, 3, 2, 1), nrow = 3)
    x <- rvec(x_m)
    y <- rvec(y_m)

    for (use in c("everything", "complete.obs", "na.or.complete",
                  "pairwise.complete.obs")) {
        expected <- vapply(seq_len(ncol(x_m)), function(j)
            stats::var(x_m[, j], y_m[, j], use = use), numeric(1))
        expect_identical(var(x, y, use = use),
                         rvec_dbl(matrix(expected, nrow = 1L)))
    }
})

test_that("'var_rvec_rvec' throws expected error with character", {
    x <- rvec(matrix(rep(1, 5), nr = 1))
    y <- rvec(matrix(rep("a", 5), nr = 1))
    expect_error(var_rvec_rvec(x = x, y = y, na.rm = TRUE, use = "everything"),
                 "Variance not defined for character vectors.")
})

test_that("'var_rvec_nonrvec' works with valid inputs, nrow > 0", {
    m <- matrix(rnorm(10), nr = 5)
    x <- rvec(m)
    y <- 1:5
    ans_obtained <- var_rvec_nonrvec(e1 = x, e2 = y, nm_e2 = "y",
                                     na.rm = FALSE, use = "everything")
    ans_expected <- rvec(matrix(c(var(m[,1], y), var(m[,2], y)), nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'var_rvec_nonrvec' works with valid inputs, nrow == 0", {
    m <- matrix(0, nr = 0, ncol = 5)
    x <- rvec(m)
    y <- integer()
    ans_obtained <- var_rvec_nonrvec(e1 = x, e2 = y, nm_e2 = "y",
                                     na.rm = FALSE, use = "everything")
    ans_expected <- rvec_dbl(matrix(c(NA, NA, NA, NA, NA), nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'var_rvec_nonrvec' preserves matrix covariance results", {
    m <- cbind(1:4, c(2, 1, 5, 3), c(8, 2, 4, 1))
    x <- rvec(m)
    y <- cbind(4:1, c(1, 3, 2, 6))
    expected <- unlist(lapply(seq_len(ncol(m)), function(j)
        stats::var(m[, j], y, use = "everything")))

    ans <- var_rvec_nonrvec(e1 = x, e2 = y, nm_e2 = "y",
                            na.rm = FALSE, use = "everything")
    expect_identical(ans, rvec_dbl(matrix(expected, nrow = 1L)))
})

test_that("'var_rvec_nonrvec' throws expected error with non-atomic", {
    x <- rvec(matrix(rep(1, 5), nr = 1))
    y <- list()
    expect_error(var_rvec_nonrvec(e1 = x, e2 = y, nm_e2 = "y",
                                  na.rm = TRUE, use = "everything"),
                 "`y` has class <list>.")
})
