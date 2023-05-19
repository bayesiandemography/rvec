
## 'atomic_to_rvec_chr' -------------------------------------------------------

test_that("atomic_to_rvec_chr works with character", {
    expect_identical(atomic_to_rvec_chr(c(x = "a", y = "b"), n_draw = 2),
                     rvec_chr(matrix(c("a","b"), nr = 2, nc = 2,
                                     dimnames = list(c("x", "y"), NULL))))
})

test_that("atomic_to_rvec_chr works with double", {
    expect_identical(atomic_to_rvec_chr(c(x = 1, y = 2), n_draw = 2),
                     rvec_chr(matrix(c("1", 2), nr = 2, nc = 2,
                                     dimnames = list(c("x", "y"), NULL))))
})

test_that("atomic_to_rvec_chr works with integer", {
    expect_identical(atomic_to_rvec_chr(c(x = 1, y = 2), n_draw = 2),
                     rvec_chr(matrix(c("1", 2), nr = 2, nc = 2,
                                     dimnames = list(c("x", "y"), NULL))))
})

test_that("atomic_to_rvec_chr works with logical", {
    expect_identical(atomic_to_rvec_chr(c(x = TRUE, y = FALSE), n_draw = 2),
                     rvec_chr(matrix(c("TRUE", FALSE), nr = 2, nc = 2,
                                     dimnames = list(c("x", "y"), NULL))))
})

                                        
## 'atomic_to_rvec_dbl' -------------------------------------------------------

test_that("atomic_to_rvec_dbl works with double", {
    expect_identical(atomic_to_rvec_dbl(c(x = 1, y = 2), n_draw = 2),
                     rvec_dbl(matrix(c(1, 2), nr = 2, nc = 2,
                                     dimnames = list(c("x", "y"), NULL))))
})

test_that("atomic_to_rvec_dbl works with integer", {
    expect_identical(atomic_to_rvec_dbl(c(x = 1L, y = 2L), n_draw = 2),
                     rvec_dbl(matrix(c(1, 2), nr = 2, nc = 2,
                                     dimnames = list(c("x", "y"), NULL))))
})

test_that("atomic_to_rvec_dbl works with logical", {
    expect_identical(atomic_to_rvec_dbl(c(x = TRUE, y = FALSE), n_draw = 2),
                     rvec_dbl(matrix(c(1, 0), nr = 2, nc = 2,
                                     dimnames = list(c("x", "y"), NULL))))
})


## 'atomic_to_rvec_int' -------------------------------------------------------

test_that("atomic_to_rvec_int works with double", {
    expect_identical(atomic_to_rvec_int(c(x = 1, y = 2), n_draw = 2),
                     rvec_int(matrix(c(1L, 2L), nr = 2, nc = 2,
                                     dimnames = list(c("x", "y"), NULL))))
})

test_that("atomic_to_rvec_int works with integer", {
    expect_identical(atomic_to_rvec_int(c(x = 1L, y = 2L), n_draw = 2),
                     rvec_int(matrix(c(1L, 2L), nr = 2, nc = 2,
                                     dimnames = list(c("x", "y"), NULL))))
})

test_that("atomic_to_rvec_int works with logical", {
    expect_identical(atomic_to_rvec_int(c(x = TRUE, y = FALSE), n_draw = 2),
                     rvec_int(matrix(c(1L, 0L), nr = 2, nc = 2,
                                     dimnames = list(c("x", "y"), NULL))))
})


## 'atomic_to_rvec_lgl' -------------------------------------------------------

test_that("atomic_to_rvec_lgl works with double", {
    expect_identical(atomic_to_rvec_lgl(c(x = 1, y = 0), n_draw = 2),
                     rvec_lgl(matrix(c(TRUE, FALSE), nr = 2, nc = 2,
                                     dimnames = list(c("x", "y"), NULL))))
})

test_that("atomic_to_rvec_lgl works with integer", {
    expect_identical(atomic_to_rvec_lgl(c(x = 1L, y = 0L), n_draw = 2),
                     rvec_lgl(matrix(c(1L, 0L), nr = 2, nc = 2,
                                     dimnames = list(c("x", "y"), NULL))))
})

test_that("atomic_to_rvec_lgl works with logical", {
    expect_identical(atomic_to_rvec_lgl(c(x = TRUE, y = FALSE), n_draw = 2),
                     rvec_lgl(matrix(c(TRUE, FALSE), nr = 2, nc = 2,
                                     dimnames = list(c("x", "y"), NULL))))
})


