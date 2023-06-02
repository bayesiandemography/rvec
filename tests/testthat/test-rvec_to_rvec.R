
## 'rvec_to_rvec_chr' ---------------------------------------------------------

test_that("'rvec_to_rvec_chr.rvec' works when n_draw(x) = n_draw", {
    x <- rvec(matrix(1, nr = 1, nc = 3))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_chr(x, n_draw = n_draw)
    ans_expected <- rvec(matrix("1", nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rvec_to_rvec_chr.rvec' works when n_draw(x) = 1", {
    x <- rvec(matrix(1, nr = 1, nc = 1))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_chr(x, n_draw = n_draw)
    ans_expected <- rvec(matrix("1", nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})


## 'rvec_to_rvec_dbl' ---------------------------------------------------------

test_that("'rvec_to_rvec_dbl.rvec_dbl' works when n_draw(x) = n_draw", {
    x <- rvec_dbl(matrix(1, nr = 1, nc = 3))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_dbl(x, n_draw = n_draw)
    ans_expected <- rvec_dbl(matrix(1, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rvec_to_rvec_dbl.rvec_dbl' works when n_draw(x) = 1", {
    x <- rvec_dbl(matrix(1, nr = 1, nc = 1))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_dbl(x, n_draw = n_draw)
    ans_expected <- rvec_dbl(matrix(1, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rvec_to_rvec_dbl.rvec_int' works when n_draw(x) = n_draw", {
    x <- rvec_int(matrix(1L, nr = 1, nc = 3))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_dbl(x, n_draw = n_draw)
    ans_expected <- rvec_dbl(matrix(1, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rvec_to_rvec_dbl.rvec_int' works when n_draw(x) = 1", {
    x <- rvec_int(matrix(1L, nr = 1, nc = 1))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_dbl(x, n_draw = n_draw)
    ans_expected <- rvec_dbl(matrix(1, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rvec_to_rvec_dbl.rvec_lgl' works when n_draw(x) = n_draw", {
    x <- rvec_lgl(matrix(TRUE, nr = 1, nc = 3))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_dbl(x, n_draw = n_draw)
    ans_expected <- rvec_dbl(matrix(1, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rvec_to_rvec_dbl.rvec_lgl' works when n_draw(x) = 1", {
    x <- rvec_lgl(matrix(FALSE, nr = 1, nc = 1))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_dbl(x, n_draw = n_draw)
    ans_expected <- rvec_dbl(matrix(0, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})


## 'rvec_to_rvec_int' ---------------------------------------------------------

test_that("'rvec_to_rvec_int.rvec_dbl' works when n_draw(x) = n_draw", {
    x <- rvec_dbl(matrix(1, nr = 1, nc = 3))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_int(x, n_draw = n_draw)
    ans_expected <- rvec_int(matrix(1L, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rvec_to_rvec_int.rvec_dbl' works when n_draw(x) = 1", {
    x <- rvec_dbl(matrix(1, nr = 1, nc = 1))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_int(x, n_draw = n_draw)
    ans_expected <- rvec_int(matrix(1L, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rvec_to_rvec_int.rvec_int' works when n_draw(x) = n_draw", {
    x <- rvec_int(matrix(1, nr = 1, nc = 3))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_int(x, n_draw = n_draw)
    ans_expected <- rvec_int(matrix(1L, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rvec_to_rvec_int.rvec_int' works when n_draw(x) = 1", {
    x <- rvec_int(matrix(1, nr = 1, nc = 1))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_int(x, n_draw = n_draw)
    ans_expected <- rvec_int(matrix(1L, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rvec_to_rvec_int.rvec_lgl' works when n_draw(x) = n_draw", {
    x <- rvec_lgl(matrix(TRUE, nr = 1, nc = 3))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_int(x, n_draw = n_draw)
    ans_expected <- rvec_int(matrix(1L, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rvec_to_rvec_int.rvec_lgl' works when n_draw(x) = 1", {
    x <- rvec_lgl(matrix(FALSE, nr = 1, nc = 1))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_int(x, n_draw = n_draw)
    ans_expected <- rvec_int(matrix(0L, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})


## 'rvec_to_rvec_lgl' ---------------------------------------------------------

test_that("'rvec_to_rvec_lgl.rvec_dbl' works when n_draw(x) = n_draw", {
    x <- rvec_dbl(matrix(1, nr = 1, nc = 3))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_lgl(x, n_draw = n_draw)
    ans_expected <- rvec_lgl(matrix(TRUE, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rvec_to_rvec_lgl.rvec_dbl' works when n_draw(x) = 1", {
    x <- rvec_dbl(matrix(1, nr = 1, nc = 1))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_lgl(x, n_draw = n_draw)
    ans_expected <- rvec_lgl(matrix(TRUE, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rvec_to_rvec_lgl.rvec_int' works when n_draw(x) = n_draw", {
    x <- rvec_int(matrix(1L, nr = 1, nc = 3))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_lgl(x, n_draw = n_draw)
    ans_expected <- rvec_lgl(matrix(TRUE, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rvec_to_rvec_lgl.rvec_int' works when n_draw(x) = 1", {
    x <- rvec_int(matrix(0L, nr = 1, nc = 1))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_lgl(x, n_draw = n_draw)
    ans_expected <- rvec_lgl(matrix(FALSE, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rvec_to_rvec_lgl.rvec_lgl' works when n_draw(x) = n_draw", {
    x <- rvec_lgl(matrix(TRUE, nr = 1, nc = 3))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_lgl(x, n_draw = n_draw)
    ans_expected <- rvec_lgl(matrix(TRUE, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rvec_to_rvec_lgl.rvec_lgl' works when n_draw(x) = 1", {
    x <- rvec_lgl(matrix(FALSE, nr = 1, nc = 1))
    n_draw <- 3L
    ans_obtained <- rvec_to_rvec_lgl(x, n_draw = n_draw)
    ans_expected <- rvec_lgl(matrix(FALSE, nr = 1, nc = 3))
    expect_identical(ans_obtained, ans_expected)
})


## 'data_matrix' --------------------------------------------------------------

test_that("'data_matrix' works with valid inputs - no recycling", {
    data_new_vec <- c(1L, 4L, 2L, 5L, 3L, 6L)
    data_old <- rbind(a = c(1, 2, 3),
                      b = c(4, 5, 6))
    n_draw <- 3L
    ans_obtained <- data_matrix(data_new_vec = data_new_vec,
                                data_old = data_old,
                                n_draw = n_draw)
    ans_expected <- rbind(a = c(1L, 2L, 3L),
                          b = c(4L, 5L, 6L))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'data_matrix' works with valid inputs - with recycling", {
    data_new_vec <- c(1L, 4L)
    data_old <- rbind(a = 1,
                      b = 4)
    n_draw <- 3L
    ans_obtained <- data_matrix(data_new_vec = data_new_vec,
                                data_old = data_old,
                                n_draw = n_draw)
    ans_expected <- rbind(a = c(1L, 1L, 1L),
                          b = c(4L, 4L, 4L))
    expect_identical(ans_obtained, ans_expected)
})

    

