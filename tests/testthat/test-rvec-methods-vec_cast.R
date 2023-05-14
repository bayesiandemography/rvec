
test_that("'vec_cast' works vec_chr", {
    x <- rvec_chr(matrix("a"))
    expect_identical(vec_cast(x, to = rvec_chr()), x)
    expect_identical(vec_cast("a", to = rvec_chr()), x)
})

test_that("'vec_cast' works vec_dbl", {
    x <- rvec_dbl(matrix(3))
    expect_identical(vec_cast(x, to = rvec_dbl()), x)
    expect_identical(vec_cast(x, to = rvec_int()), rvec_int(matrix(3)))
    expect_identical(vec_cast(3, to = rvec_dbl()), x)
    expect_identical(vec_cast(3L, to = rvec_dbl()), x)
    expect_identical(vec_cast(FALSE, to = rvec_dbl()), rvec_dbl(matrix(0)))
})

test_that("'vec_cast' works vec_int", {
    x <- rvec_int(matrix(3))
    expect_identical(vec_cast(x, to = rvec_dbl()), rvec_dbl(matrix(3)))
    expect_identical(vec_cast(x, to = rvec_int()), x)
    expect_identical(vec_cast(3, to = rvec_int()), x)
    expect_identical(vec_cast(3L, to = rvec_int()), x)
    expect_identical(vec_cast(FALSE, to = rvec_int()), rvec_int(matrix(0)))
})

test_that("'vec_cast' works vec_lgl", {
    x <- rvec_lgl(matrix(FALSE))
    expect_identical(vec_cast(x, to = rvec_dbl()), rvec_dbl(matrix(0)))
    expect_identical(vec_cast(x, to = rvec_int()), rvec_int(matrix(0)))
    expect_identical(vec_cast(1, to = rvec_lgl()), rvec_lgl(matrix(TRUE)))
    expect_identical(vec_cast(1L, to = rvec_lgl()), rvec_lgl(matrix(TRUE)))
    expect_identical(vec_cast(FALSE, to = rvec_lgl()), x)
})
