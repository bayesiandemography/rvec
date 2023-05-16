

test_that("'vec_arith' works with rvec_dbl", {
    ## rvec_dbl
    expect_identical(rvec_dbl(matrix(1:2, nr = 1)) + rvec_dbl(matrix(2:5, nr = 2)),
                     rvec_dbl(matrix(c(3, 4, 6, 7), nr = 2)))
    ## rvec_int
    expect_identical(rvec_dbl(matrix(1:2, nr = 1)) + rvec_int(matrix(2:1, nr = 1)),
                     rvec_dbl(matrix(c(3L, 3L), nr = 1)))
    expect_identical(rvec_int(matrix(1:2, nr = 1)) - rvec_dbl(matrix(c(1, NA), nr = 1)),
                     rvec_dbl(matrix(c(0, NA), nr = 1)))
    ## rvec_lgl
    expect_identical(rvec_dbl(matrix(1:4, nr = 2)) +
                     rvec_lgl(matrix(c(TRUE, FALSE), nr = 1)),
                     rvec_dbl(matrix(c(2, 3, 3, 4), nr = 2)))
    expect_identical(rvec_lgl(matrix(TRUE)) + rvec_dbl(matrix(-1)),
                     rvec_dbl(matrix(0)))
    ## double 
    expect_identical(rvec_dbl(matrix(2:3, nr = 1)) + c(0.2, 0.3),
                     rvec_dbl(matrix(c(2.2, 2.3, 3.2, 3.3), nr = 2)))
    expect_identical(-1 * rvec_dbl(matrix(-1)),
                     rvec_dbl(matrix(1)))
    ## integer 
    expect_identical(rvec_dbl(matrix(2:3, nr = 1)) + 1L,
                     rvec_dbl(matrix(c(3, 4), nr = 1)))
    expect_identical(-1L * rvec_dbl(matrix(-1)),
                     rvec_dbl(matrix(1)))
    ## logical
    expect_identical(rvec_dbl(matrix(2:5, nr = 1)) * FALSE,
                     rvec_dbl(matrix(rep(0, 4), nr = 1)))
    expect_identical(c(TRUE, FALSE) - rvec_dbl(matrix(2:5, nr = 1)),
                     rvec_dbl(rbind(-(1:4),
                                    -(2:5))))
})

test_that("'vec_arith' works with rvec_int", {
    ## rvec_dbl
    expect_identical(rvec_int(matrix(1:2, nr = 1)) + rvec_dbl(matrix(2:3, nr = 1)),
                     rvec_dbl(matrix(c(3, 5), nr = 1)))
    expect_identical(rvec_dbl(matrix(1:2, nr = 1)) + rvec_int(matrix(2:3, nr = 1)),
                     rvec_dbl(matrix(c(3, 5), nr = 1)))
    ## rvec_int
    expect_identical(rvec_int(matrix(1:2, nr = 1)) + rvec_int(rbind(2:1, 1:2)),
                     rvec_int(rbind(c(3L, 3L),
                                    c(2L, 4L))))
    expect_identical(rvec_int(matrix(1:2, nr = 1)) - rvec_int(matrix(c(1, NA), nr = 1)),
                     rvec_int(matrix(c(0L, NA), nr = 1)))
    ## rvec_lgl
    expect_identical(rvec_int(matrix(2:3, nr = 1)) +
                     rvec_lgl(matrix(c(TRUE, FALSE), nr = 1)),
                     rvec_int(matrix(c(3, 3), nr = 1)))
    expect_identical(rvec_lgl(matrix(TRUE)) + rvec_int(matrix(-1)),
                     rvec_int(matrix(0)))
    ## double 
    expect_identical(rvec_int(matrix(2:3, nr = 1)) + c(0.2, 0.3),
                     rvec_dbl(matrix(c(2.2, 2.3, 3.2, 3.3), nr = 2)))
    expect_identical(-1 * rvec_int(matrix(-1)),
                     rvec_dbl(matrix(1)))
    ## integer 
    expect_identical(rvec_int(matrix(2:3, nr = 1)) + 1L,
                     rvec_int(matrix(c(3, 4), nr = 1)))
    expect_identical(-1L * rvec_int(matrix(-1)),
                     rvec_int(matrix(1)))
    ## logical
    expect_identical(rvec_int(matrix(2:5, nr = 1)) * FALSE,
                     rvec_int(matrix(rep(0, 4), nr = 1)))
    expect_identical(c(TRUE, FALSE) - rvec_int(matrix(2:5, nr = 1)),
                     rvec_int(rbind(-(1:4),
                                    -(2:5))))
})

test_that("'vec_arith' works with rvec_lgo", {
    ## rvec_dbl
    expect_identical(rvec_lgl(matrix(c(TRUE, FALSE), nr = 1)) +
                     rvec_dbl(matrix(2:3, nr = 1)),
                     rvec_dbl(matrix(c(3, 3), nr = 1)))
    expect_identical(rvec_dbl(matrix(1:2, nr = 1)) +
                     rvec_lgl(matrix(c(TRUE, FALSE), nr = 1)),
                     rvec_dbl(matrix(c(2, 2), nr = 1)))
    ## rvec_int
    expect_identical(rvec_lgl(matrix(c(FALSE, NA), nr = 1))
                              + rvec_int(rbind(2:1, 1:2)),
                     rvec_int(rbind(c(2L, NA),
                                    c(1L, NA))))
    expect_identical(rvec_int(matrix(1:2, nr = 1)) -
                     rvec_lgl(matrix(c(TRUE, NA), nr = 1)),
                     rvec_int(matrix(c(0L, NA), nr = 1)))
    ## rvec_lgl
    expect_identical(rvec_lgl(matrix(c(TRUE, TRUE), nr = 1)) +
                     rvec_lgl(matrix(c(TRUE, FALSE), nr = 1)),
                     rvec_int(matrix(c(2, 1), nr = 1)))
    ## double 
    expect_identical(rvec_lgl(matrix(c(TRUE, TRUE), nr = 1)) + c(0.2, 0.3),
                     rvec_dbl(matrix(c(1.2, 1.3, 1.2, 1.3), nr = 2)))
    expect_identical(-1 * rvec_lgl(matrix(FALSE)),
                     rvec_dbl(matrix(0)))
    ## integer 
    expect_identical(rvec_lgl(matrix(c(FALSE, TRUE), nr = 1)) + 1L,
                     rvec_int(matrix(c(1, 2), nr = 1)))
    expect_identical(-1L * rvec_lgl(matrix(TRUE)),
                     rvec_int(matrix(-1)))
    ## logical
    expect_identical(rvec_lgl(matrix(c(TRUE, FALSE, NA), nr = 1)) * FALSE,
                     rvec_int(matrix(c(0, 0, NA), nr = 1)))
    expect_identical(c(TRUE, FALSE) -
                     rvec_lgl(matrix(c(TRUE, FALSE), nr = 1)),
                     rvec_int(rbind(c(0, 1L),
                                    c(-1, 0))))
})                     

