

## is.na ----------------------------------------------------------------------

test_that("is.na works with length > 0", {
    x <- rvec(list(c(1, NA),
                   c(NA, 1),
                   c(NaN, Inf)))
    ans_obtained <- is.na(x)
    ans_expected <- rvec(list(c(FALSE, TRUE),
                              c(TRUE, FALSE),
                              c(TRUE, FALSE)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("is.na works with length 0", {
    x <- rvec_int()
    ans_obtained <- is.na(x)
    ans_expected <- rvec_lgl()
    expect_identical(ans_obtained, ans_expected)
})


## anyNA ----------------------------------------------------------------------

test_that("anyNA works with length > 0", {
    x <- rvec(list(c(1, NA),
                   c(3, 1),
                   c(NaN, Inf)))
    expect_true(anyNA(x))
    expect_false(anyNA(x[2]))
})

test_that("is.na works with length 0", {
    x <- rvec_int()
    expect_false(anyNA(x))
})

              
          
