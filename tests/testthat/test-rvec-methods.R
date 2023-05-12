
## 'as.matrix' ----------------------------------------------------------------

test_that("'as.matrix' method for rvec works", {
    m <- matrix(c(T, F), nr = 1)
    x <- rvec(m)
    expect_identical(as.matrix(x), m)
    m <- matrix(character(), nr = 0, nc = 3)
    x <- rvec(m)
    expect_identical(as.matrix(x), m)
})


## 'format' -------------------------------------------------------------------

test_that("'format' method for rvec works - one column", {
    x <- rvec(matrix("a", nr = 1))
    ans_obtained <- format(x)
    ans_expected <- "a"
    expect_identical(ans_obtained, ans_expected)
})

test_that("'format' method for rvec works - two columns", {
    x <- rvec(matrix(c(FALSE, TRUE), nr = 2, nc = 2))
    ans_obtained <- format(x)
    ans_expected <- c("F,F", "T,T")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'format' method for rvec works - three columns", {
    x <- rvec(matrix(1:6, nr = 2, nc = 3))
    ans_obtained <- format(x)
    ans_expected <- c("1,3,5", "2,4,6")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'format' method for rvec works - four columns", {
    x <- rvec(matrix(pi, nr = 2, nc = 4))
    ans_obtained <- format(x)
    ans_expected <- c("3.142,..,3.142", "3.142,..,3.142")
    expect_identical(ans_obtained, ans_expected)
})


## 'median' -------------------------------------------------------------------

test_that("'median' method for rvec works - chr", {
    m <- matrix("a", nr = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec(apply(m, 2, median)))
    x <- rvec_chr()
    expect_identical(median(x), rvec_chr(median(character())))
})

test_that("'median' method for rvec works - dbl", {
    set.seed(0)
    m <- matrix(rnorm(15), nr = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec(apply(m, 2, median)))
    x <- rvec_dbl()
    expect_identical(median(x), rvec_dbl(median(double())))
    m <- matrix(c(1:3, NA_real_))
    x <- rvec(m)
    expect_identical(median(x), rvec(apply(m, 2, median)))
    expect_identical(median(x, na.rm = TRUE),
                     rvec(apply(m, 2, median, na.rm = TRUE)))
})

test_that("'median' method for rvec works - int", {
    m <- matrix(1:15, nr = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec_int(apply(m, 2, median)))
    m <- matrix(c(1:14, 16), nr = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec_dbl(apply(m, 2, median)))
    x <- rvec_int()
    expect_identical(median(x), rvec_int(median(integer())))
    m <- matrix(c(1:3, NA), nr = 2)
    x <- rvec(m)
    expect_identical(median(x), rvec(apply(m, 2, median)))
    expect_identical(median(x, na.rm = TRUE),
                     rvec(apply(m, 2, median, na.rm = TRUE)))
})

test_that("'median' method for rvec works - lgl", {
    m <- matrix(c(TRUE, FALSE, TRUE), nr = 3, ncol = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec_lgl(apply(m, 2, median)))
    m <- matrix(c(TRUE, FALSE), nr = 2, ncol = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec_dbl(apply(m, 2, median)))
    x <- rvec_lgl()
    expect_identical(median(x), rvec_lgl(median(logical())))
    m <- matrix(c(TRUE, FALSE, TRUE, NA), nr = 2)
    x <- rvec(m)
    expect_identical(median(x), rvec(apply(m, 2, median)))
    expect_identical(median(x, na.rm = TRUE),
                     rvec(apply(m, 2, median, na.rm = TRUE)))
})


## 'vec_cast' -----------------------------------------------------------------

test_that("'vec_cast' works with logical to integer", {
    x <- rvec_lgl(matrix(c(TRUE, FALSE, TRUE, NA), nr = 1))
    to <- rvec_int(matrix(1L, nr = 0L, ncol = 4))
    ans_obtained <- vec_cast(x = x, to = to)
    ans_expected <- rvec_int(matrix(c(1L, 0L, 1L, NA), nr = 1))
    expect_identical(ans_obtained, ans_expected)
})






## 'vec_math' -----------------------------------------------------------------

test_that("'sum' works with non-empty double", {
    x <- rvec_dbl(rbind(1:4, 5:8))
    ans_obtained <- sum(x)
    ans_expected <- rvec_dbl(matrix(c(6, 8, 10, 12), nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'sum' works with non-empty integer", {
    x <- rvec_int(rbind(1:4, 5:8))
    ans_obtained <- sum(x)
    ans_expected <- rvec_int(matrix(c(6L, 8L, 10L, 12L), nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'sum' works with non-empty logical", {
    x <- rvec_lgl(rbind(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, TRUE)))
    ans_obtained <- sum(x)
    ans_expected <- rvec_int(matrix(c(1, 1, 2), nr = 1L))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'sum' works with empty vector", {
    x <- rvec_int()
    ans_obtained <- sum(x)
    ans_expected <- rvec_int(matrix(0L, nr = 1L))
    expect_identical(ans_obtained, ans_expected)
})




