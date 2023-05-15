

test_that("'vec_cast' works with valid casts to rvec_chr", {
    expect_identical(vec_cast(rvec_chr(matrix("a")), to = rvec_chr()),
                     rvec_chr(matrix("a")))
    expect_identical(vec_cast(rvec_dbl(matrix(1.3)), to = rvec_chr()),
                     rvec_chr(matrix("1.3")))
    expect_identical(vec_cast(rvec_int(matrix(1L)), to = rvec_chr()),
                     rvec_chr(matrix("1")))
    expect_identical(vec_cast(rvec_lgl(matrix(TRUE)), to = rvec_chr()),
                     rvec_chr(matrix("TRUE")))
})

test_that("'vec_cast' works with valid casts to rvec_dbl", {
    expect_identical(vec_cast(rvec_dbl(matrix(1.3)), to = rvec_dbl()),
                     rvec_dbl(matrix(1.3)))
    expect_identical(vec_cast(rvec_int(matrix(1L)), to = rvec_dbl()),
                     rvec_dbl(matrix(1.0)))
    expect_identical(vec_cast(rvec_lgl(matrix(TRUE)), to = rvec_dbl()),
                     rvec_dbl(matrix(1.0)))
})

test_that("'vec_cast' throws appropriate error with invalid cast to rvec_dbl", {
    expect_error(vec_cast(rvec_chr(matrix("a")), to = rvec_dbl()),
                 class = "vctrs_error_incompatible")
})

test_that("'vec_cast' works with valid casts to rvec_int", {
    expect_identical(vec_cast(rvec_dbl(matrix(2)), to = rvec_int()),
                     rvec_int(matrix(2L)))
    expect_identical(vec_cast(rvec_int(matrix(2L)), to = rvec_int()),
                     rvec_int(matrix(2L)))
    expect_identical(vec_cast(rvec_lgl(matrix(TRUE)), to = rvec_int()),
                     rvec_int(matrix(1L)))
})

test_that("'vec_cast' throws appropriate error with invalid cast to rvec_int", {
    expect_error(vec_cast(rvec_chr(matrix("a")), to = rvec_int()),
                 class = "vctrs_error_incompatible")
    expect_error(vec_cast(rvec_dbl(matrix(1.1)), to = rvec_int()),
                 class = "vctrs_error_incompatible")
})

test_that("'vec_cast' works with valid casts to rvec_lgl", {
    expect_identical(vec_cast(rvec_dbl(matrix(1)), to = rvec_lgl()),
                     rvec_lgl(matrix(TRUE)))
    expect_identical(vec_cast(rvec_int(matrix(1L)), to = rvec_lgl()),
                     rvec_lgl(matrix(TRUE)))
    expect_identical(vec_cast(rvec_lgl(matrix(TRUE)), to = rvec_lgl()),
                     rvec_lgl(matrix(TRUE)))
})

test_that("'vec_cast' throws appropriate error with invalid cast to rvec_lgl", {
    expect_error(vec_cast(rvec_chr(matrix("a")), to = rvec_lgl()),
                 class = "vctrs_error_incompatible")
    expect_error(vec_cast(rvec_dbl(matrix(1.1)), to = rvec_lgl()),
                 class = "vctrs_error_incompatible")
    expect_error(vec_cast(rvec_int(matrix(2L)), to = rvec_lgl()),
                 class = "vctrs_error_incompatible")
})


