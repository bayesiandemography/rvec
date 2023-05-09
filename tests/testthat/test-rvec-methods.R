
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






## 'vec_cast' -----------------------------------------------------------------

test_that("'vec_cast' works with logical to integer", {
    x <- rvec_lgl(matrix(c(TRUE, FALSE, TRUE, NA), nr = 1))
    to <- rvec_int(matrix(1L, nr = 0L, ncol = 4))
    ans_obtained <- vec_cast(x = x, to = to)
    ans_expected <- rvec_int(matrix(c(1L, 0L, 1L, NA), nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

