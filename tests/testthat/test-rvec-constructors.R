
## 'rvec' ---------------------------------------------------------------------

test_that("'rvec' works with valid matrices", {
    m <- matrix(1:6, 3)
    ans <- rvec(m)
    expect_s3_class(ans, "rvec_int")
    expect_identical(field(ans, "data"), m)
    m <- matrix("a", nrow = 3, ncol = 1)
    ans <- rvec(m)
    expect_s3_class(ans, "rvec_chr")
    m <- matrix(NA, nrow = 0, ncol = 1)
    ans <- rvec(m)
    expect_s3_class(ans, "rvec_lgl")
    m <- matrix(c(1, NA), nrow = 2, ncol = 3)
    ans <- rvec(m)
    expect_s3_class(ans, "rvec_dbl")
})

test_that("'rvec' works with valid vectors", {
    expect_s3_class(rvec(1L), "rvec_int")
    expect_s3_class(rvec(letters), "rvec_chr")
    expect_s3_class(rvec(NA), "rvec_lgl")
    expect_s3_class(rvec(NA_real_), "rvec_dbl")
})


## 'rvec_chr' -----------------------------------------------------------------

test_that("'rvec_chr' works with valid inputs", {
    expect_s3_class(rvec_chr(matrix("a", 2, 3)), "rvec_chr")
    expect_s3_class(rvec_chr("a"), "rvec_chr")
    expect_s3_class(rvec_chr("1"), "rvec_chr")
})

test_that("'rvec_chr' throws error with invalid inputs", {
    expect_error(rvec_chr(matrix(1, 2, 3)))
    expect_error(rvec_chr(TRUE))
})


## 'rvec_dbl' -----------------------------------------------------------------

test_that("'rvec_dbl' works with valid inputs", {
    expect_s3_class(rvec_dbl(matrix(Inf, 1, 3)), "rvec_dbl")
    expect_s3_class(rvec_dbl(1L), "rvec_dbl")
    expect_s3_class(rvec_dbl(TRUE), "rvec_dbl")
})

test_that("'rvec_dbl' throws error with invalid inputs", {
    expect_error(rvec_dbl(matrix("a", 2, 3)))
    expect_error(rvec_dbl(""))
})


## 'rvec_int' -----------------------------------------------------------------

test_that("'rvec_int' works with valid inputs", {
    expect_s3_class(rvec_int(matrix(1L, 2, 3)), "rvec_int")
    expect_s3_class(rvec_int(1L), "rvec_int")
    expect_s3_class(rvec_int(TRUE), "rvec_int")
})

test_that("'rvec_int' throws error with invalid inputs", {
    expect_error(rvec_int(matrix("a", 2, 3)))
    expect_error(rvec_int(3.1))
})


## 'rvec_lgl' -----------------------------------------------------------------

test_that("'rvec_lgl' works with valid inputs", {
    expect_s3_class(rvec_lgl(matrix(FALSE, 2, 3)), "rvec_lgl")
    expect_s3_class(rvec_lgl(NA), "rvec_lgl")
    expect_s3_class(rvec_lgl(TRUE), "rvec_lgl")
})

test_that("'rvec_lgl' throws error with invalid inputs", {
    expect_error(rvec_lgl(matrix("a", 2, 3)))
    expect_error(rvec_lgl(3.1))
})


## 'new_rvec' -----------------------------------------------------------------

test_that("'new_rvec' works", {
    m <- matrix(TRUE, nr = 3, nc = 4)
    x <- new_rvec(m)
    expect_s3_class(x, c("rvec_lgl", "rvec"))
})


## 'new_chr' -----------------------------------------------------------------

test_that("'new_rvec_chr' works", {
    m <- matrix("a", nr = 3, nc = 4)
    x <- new_rvec_chr(m)
    expect_s3_class(x, c("rvec_chr", "rvec"))
})


## 'new_dbl' -----------------------------------------------------------------

test_that("'new_rvec_dbl' works", {
    m <- matrix(NA_real_, nr = 3, nc = 4)
    x <- new_rvec_dbl(m)
    expect_s3_class(x, c("rvec_dbl", "rvec"))
})


## 'new_int' -----------------------------------------------------------------

test_that("'new_rvec_int' works", {
    m <- matrix(NA_integer_, nr = 3, nc = 4)
    x <- new_rvec_int(m)
    expect_s3_class(x, c("rvec_int", "rvec"))
})


## 'new_lgl' -----------------------------------------------------------------

test_that("'new_rvec_lgl' works", {
    m <- matrix(NA, nr = 3, nc = 4)
    x <- new_rvec_lgl(m)
    expect_s3_class(x, c("rvec_lgl", "rvec"))
})


## 'rvec_inner' ---------------------------------------------------------------

test_that("'rvec_inner' throws expected error with invalid input", {
    expect_error(rvec_inner(list()),
                 paste("`x` must be a matrix, a character, double, integer,",
                       "or logical vector, or NULL"))
})

    



                
    
