




## 'median' -------------------------------------------------------------------

test_that("'median' method for rvec works - chr", {
    m <- matrix("a", nr = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec(matrix(apply(m, 2, median))))
    x <- rvec_chr()
    expect_identical(median(x), rvec_chr(matrix(median(character()))))
})

test_that("'median' method for rvec works - dbl", {
    set.seed(0)
    m <- matrix(rnorm(15), nr = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec(matrix(apply(m, 2, median), nrow = 1)))
    x <- rvec_dbl()
    expect_identical(median(x), rvec_dbl(matrix(median(double()), nrow = 1)))
    m <- matrix(c(1:3, NA_real_))
    x <- rvec(m)
    expect_identical(median(x), rvec(matrix(apply(m, 2, median), nr = 1)))
    expect_identical(median(x, na.rm = TRUE),
                     rvec(matrix(apply(m, 2, median, na.rm = TRUE), nr = 1)))
})

test_that("'median' method for rvec works - int", {
    m <- matrix(1:15, nr = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec_int(matrix(apply(m, 2, median), nr = 1)))
    m <- matrix(c(1:14, 16), nr = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec_dbl(matrix(apply(m, 2, median), nr = 1)))
    x <- rvec_int()
    expect_identical(median(x), rvec_int(matrix(median(integer()), nr = 1)))
    m <- matrix(c(1:3, NA), nr = 2)
    x <- rvec(m)
    expect_identical(median(x), rvec(matrix(apply(m, 2, median), nr = 1)))
    expect_identical(median(x, na.rm = TRUE),
                     rvec(matrix(apply(m, 2, median, na.rm = TRUE), nr = 1)))
})

test_that("'median' method for rvec works - lgl", {
    m <- matrix(c(TRUE, FALSE, TRUE), nr = 3, ncol = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec_lgl(matrix(apply(m, 2, median), nr = 1)))
    m <- matrix(c(TRUE, FALSE), nr = 2, ncol = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec_dbl(matrix(apply(m, 2, median), nr = 1)))
    x <- rvec_lgl()
    expect_identical(median(x), rvec_lgl(matrix(median(logical()), nr = 1)))
    m <- matrix(c(TRUE, FALSE, TRUE, NA), nr = 2)
    x <- rvec(m)
    expect_identical(median(x), rvec(matrix(apply(m, 2, median), nr = 1)))
    expect_identical(median(x, na.rm = TRUE),
                     rvec(matrix(apply(m, 2, median, na.rm = TRUE), nr = 1)))
})
