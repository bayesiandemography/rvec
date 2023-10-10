
## 'vec_math' -----------------------------------------------------------------

test_that("'is.nan' works with non-empty rvec_dbl", {
    x <- rvec_dbl(rbind(c(1, NaN),
                        c(1, 2),
                        c(NaN, NaN)))
    ans_obtained <- is.nan(x)
    ans_expected <- rvec_lgl(rbind(c(FALSE, TRUE),
                                   c(FALSE, FALSE),
                                   c(TRUE, TRUE)))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'is.nan' works with empty rvec_dbl", {
    x <- rvec_dbl()
    ans_obtained <- is.nan(x)
    ans_expected <- rvec_lgl()
    expect_equal(ans_obtained, ans_expected)
})

test_that("'is.nan' works with empty rvec_int", {
    x <- rvec_int()
    ans_obtained <- is.nan(x)
    ans_expected <- rvec_lgl()
    expect_equal(ans_obtained, ans_expected)
})

test_that("'is.nan' works with empty rvec_lgl", {
    x <- rvec_lgl()
    ans_obtained <- is.nan(x)
    ans_expected <- rvec_lgl()
    expect_equal(ans_obtained, ans_expected)
})

test_that("'is.finite' works with non-empty rvec_dbl", {
    x <- rvec_dbl(rbind(c(1, Inf),
                        c(1, 2),
                        c(1, NA),
                        c(Inf, NA)))
    ans_obtained <- is.finite(x)
    ans_expected <- rvec_lgl(rbind(c(TRUE, FALSE),
                                   c(TRUE, TRUE),
                                   c(TRUE, FALSE),
                                   c(FALSE, FALSE)))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'is.finite' works with non-empty rvec_int", {
    x <- rvec_int(matrix(c(1, NA), 1))
    ans_obtained <- is.finite(x)
    ans_expected <- rvec_lgl(matrix(c(TRUE, FALSE), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'is.finite' works with non-empty rvec_lgl", {
    x <- rvec_lgl(matrix(c(TRUE, NA), nr = 1))
    ans_obtained <- is.finite(x)
    ans_expected <- rvec_lgl(matrix(c(TRUE, FALSE), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'is.finite' works with empty rvec_dbl", {
    x <- rvec_dbl()
    ans_obtained <- is.finite(x)
    ans_expected <- rvec_lgl()
    expect_equal(ans_obtained, ans_expected)
})

test_that("'is.finite' works with empty rvec_int", {
    x <- rvec_int()
    ans_obtained <- is.finite(x)
    ans_expected <- rvec_lgl()
    expect_equal(ans_obtained, ans_expected)
})

test_that("'is.finite' works with empty rvec_lgl", {
    x <- rvec_lgl()
    ans_obtained <- is.finite(x)
    ans_expected <- rvec_lgl()
    expect_equal(ans_obtained, ans_expected)
})

test_that("'is.infinite' works with non-empty rvec_dbl", {
    x <- rvec_dbl(rbind(c(1, Inf),
                        c(Inf, NA)))
    ans_obtained <- is.infinite(x)
    ans_expected <- rvec_lgl(rbind(c(FALSE, TRUE),
                                   c(TRUE, FALSE)))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'is.infinite' works with non-empty rvec_int", {
    x <- rvec_int(matrix(c(1, NA), nr = 1))
    ans_obtained <- is.infinite(x)
    ans_expected <- rvec_lgl(matrix(c(FALSE, FALSE), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'is.infinite' works with non-empty rvec_lgl", {
    x <- rvec_lgl(matrix(NA))
    ans_obtained <- is.infinite(x)
    ans_expected <- rvec_lgl(matrix(FALSE))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'is.infinite' works with empty rvec_dbl", {
    x <- rvec_dbl()
    ans_obtained <- is.infinite(x)
    ans_expected <- rvec_lgl()
    expect_equal(ans_obtained, ans_expected)
})

test_that("'is.infinite' works with empty rvec_int", {
    x <- rvec_int()
    ans_obtained <- is.infinite(x)
    ans_expected <- rvec_lgl()
    expect_equal(ans_obtained, ans_expected)
})

test_that("'is.infinite' works with empty rvec_lgl", {
    x <- rvec_lgl()
    ans_obtained <- is.infinite(x)
    ans_expected <- rvec_lgl()
    expect_equal(ans_obtained, ans_expected)
})

test_that("'prod' works with non-empty double", {
    x <- rvec_dbl(rbind(1:4, 5:8))
    ans_obtained <- prod(x)
    ans_expected <- rvec_dbl(matrix((1:4) * (5:8), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'prod' works with non-empty integer", {
    x <- rvec_int(rbind(1:4, 5:8))
    ans_obtained <- prod(x)
    ans_expected <- rvec_dbl(matrix((1:4) * (5:8), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'prod' works with non-empty logical", {
    x <- rvec_lgl(rbind(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, TRUE)))
    ans_obtained <- prod(x)
    ans_expected <- rvec_dbl(matrix(c(0, 0, 1), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'prod' works with empty rvec_dbl", {
    x <- rvec_dbl()
    ans_obtained <- prod(x)
    ans_expected <- rvec_dbl(matrix(1, nr = 1L))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'prod' works with empty rvec_int", {
    x <- rvec_int()
    ans_obtained <- prod(x)
    ans_expected <- rvec_dbl(matrix(1, nr = 1L))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'prod' works with empty rvec_lgl", {
    x <- rvec_lgl()
    ans_obtained <- prod(x)
    ans_expected <- rvec_dbl(matrix(1, nr = 1L))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'sum' works with non-empty double", {
    x <- rvec_dbl(rbind(1:4, 5:8))
    ans_obtained <- sum(x)
    ans_expected <- rvec_dbl(matrix(c(6, 8, 10, 12), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'sum' works with non-empty integer", {
    x <- rvec_int(rbind(1:4, 5:8))
    ans_obtained <- sum(x)
    ans_expected <- rvec_int(matrix(c(6L, 8L, 10L, 12L), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'sum' works with non-empty logical", {
    x <- rvec_lgl(rbind(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, TRUE)))
    ans_obtained <- sum(x)
    ans_expected <- rvec_int(matrix(c(1, 1, 2), nr = 1L))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'sum' works with empty rvec_dbl", {
    x <- rvec_dbl()
    ans_obtained <- sum(x)
    ans_expected <- rvec_dbl(matrix(0L, nr = 1L))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'sum' works with empty rvec_int", {
    x <- rvec_int()
    ans_obtained <- sum(x)
    ans_expected <- rvec_int(matrix(0L, nr = 1L))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'sum' works with empty rvec_lgl", {
    x <- rvec_lgl()
    ans_obtained <- sum(x)
    ans_expected <- rvec_int(matrix(0L, nr = 1L))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'all' works with non-empty double", {
    x <- rvec_dbl(rbind(1:4, 5:8))
    ans_obtained <- all(x)
    ans_expected <- rvec_lgl(matrix(TRUE, nr = 1, nc = 4))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'all' works with non-empty integer", {
    x <- rvec_int(rbind(1:4, 5:8))
    ans_obtained <- all(x)
    ans_expected <- rvec_lgl(matrix(TRUE, nr = 1, nc = 4))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'all' works with non-empty logical", {
    x <- rvec_lgl(rbind(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, TRUE)))
    ans_obtained <- all(x)
    ans_expected <- rvec_lgl(matrix(c(FALSE, FALSE, TRUE), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'all' works with empty rvec_dbl", {
    x <- rvec_dbl()
    ans_obtained <- all(x)
    ans_expected <- rvec_lgl(matrix(TRUE, nr = 1L))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'all' works with empty rvec_int", {
    x <- rvec_int()
    ans_obtained <- all(x)
    ans_expected <- rvec_lgl(matrix(TRUE, nr = 1L))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'all' works with empty rvec_lgl", {
    x <- rvec_lgl()
    ans_obtained <- all(x)
    ans_expected <- rvec_lgl(matrix(TRUE, nr = 1L))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mean' works with non-empty double", {
    x <- rvec_dbl(rbind(1:4, 5:8))
    ans_obtained <- mean(x)
    ans_expected <- rvec_dbl(matrix(0.5 * (1:4) + 0.5 * (5:8), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mean' works with non-empty integer", {
    x <- rvec_int(rbind(1:4, 5:8))
    ans_obtained <- mean(x)
    ans_expected <- rvec_dbl(matrix(0.5 * (1:4) + 0.5 * (5:8), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mean' works with non-empty logical", {
    x <- rvec_lgl(rbind(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, TRUE)))
    ans_obtained <- mean(x)
    ans_expected <- rvec_dbl(matrix(c(0.5, 0.5, 1), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mean' works with empty vector", {
    x <- rvec_int()
    ans_obtained <- mean(x)
    ans_expected <- rvec_dbl(matrix(NaN, nr = 1L))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mean' works with non-empty double", {
    x <- rvec_dbl(rbind(1:4, 5:8))
    ans_obtained <- mean(x)
    ans_expected <- rvec_dbl(matrix(0.5 * (1:4) + 0.5 * (5:8), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mean' works with non-empty integer", {
    x <- rvec_int(rbind(1:4, 5:8))
    ans_obtained <- mean(x)
    ans_expected <- rvec_dbl(matrix(0.5 * (1:4) + 0.5 * (5:8), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mean' works with non-empty logical", {
    x <- rvec_lgl(rbind(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, TRUE)))
    ans_obtained <- mean(x)
    ans_expected <- rvec_dbl(matrix(c(0.5, 0.5, 1), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mean' works with empty rvec_dbl", {
    x <- rvec_dbl()
    ans_obtained <- mean(x)
    ans_expected <- rvec_dbl(matrix(NaN, nr = 1L))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mean' works with empty rvec_int", {
    x <- rvec_int()
    ans_obtained <- mean(x)
    ans_expected <- rvec_dbl(matrix(NaN, nr = 1L))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mean' works with empty rvec_lgl", {
    x <- rvec_lgl()
    ans_obtained <- mean(x)
    ans_expected <- rvec_dbl(matrix(NaN, nr = 1L))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'cumprod' returns rvec_dbl with integer inputs", {
    x <- rvec_int(rbind(1:4, 5:8))
    ans <- cumprod(x)
    expect_s3_class(ans, "rvec_dbl")
})

test_that("'cummax' works with non-empty double", {
    x <- rvec_dbl(rbind(1:4, 5:8))
    ans_obtained <- cummax(x)
    ans_expected <- x
    expect_equal(ans_obtained, ans_expected)
})

test_that("'cummax' works with non-empty integer", {
    x <- rvec_int(rbind(1:4, 5:8))
    ans_obtained <- cummax(x)
    ans_expected <- x
    expect_equal(ans_obtained, ans_expected)
})

test_that("'cummax' works with non-empty logical", {
    x <- rvec_lgl(rbind(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, TRUE)))
    ans_obtained <- cummax(x)
    ans_expected <- rvec_int(rbind(c(1, 0, 1),
                                   c(1, 1, 1)))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'cummax' works with empty rvec_dbl", {
    x <- rvec_dbl()
    ans_obtained <- cummax(x)
    ans_expected <- x
    expect_equal(ans_obtained, ans_expected)
})

test_that("'cummax' works with empty rvec_int", {
    x <- rvec_int()
    ans_obtained <- cummax(x)
    ans_expected <- x
    expect_equal(ans_obtained, ans_expected)
})

test_that("'cummax' works with empty rvec_lgl", {
    x <- rvec_lgl()
    ans_obtained <- cummax(x)
    ans_expected <- rvec_int()
    expect_equal(ans_obtained, ans_expected)
})

test_that("'cummax' works with non-empty double", {
    x <- rvec_dbl(rbind(1:4, 5:8))
    ans_obtained <- cummax(x)
    ans_expected <- x
    expect_equal(ans_obtained, ans_expected)
})

test_that("'cummax' works with non-empty integer", {
    x <- rvec_int(rbind(1:4, 5:8))
    ans_obtained <- cummax(x)
    ans_expected <- x
    expect_equal(ans_obtained, ans_expected)
})

test_that("'cummax' works with non-empty logical", {
    x <- rvec_lgl(rbind(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, TRUE)))
    ans_obtained <- cummax(x)
    ans_expected <- rvec_int(rbind(c(1, 0, 1),
                                   c(1, 1, 1)))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'cummax' works with empty rvec_dbl", {
    x <- rvec_dbl()
    ans_obtained <- cummax(x)
    ans_expected <- x
    expect_equal(ans_obtained, ans_expected)
})

test_that("'cummax' works with empty rvec_int", {
    x <- rvec_int()
    ans_obtained <- cummax(x)
    ans_expected <- x
    expect_equal(ans_obtained, ans_expected)
})

test_that("'cummax' works with empty rvec_lgl", {
    x <- rvec_lgl()
    ans_obtained <- cummax(x)
    ans_expected <- rvec_int()
    expect_equal(ans_obtained, ans_expected)
})





