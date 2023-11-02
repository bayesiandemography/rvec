
## 'n_draw' -------------------------------------------------------------------

test_that("'n_draw' method for rvec works when 'x' is an rvec", {
    x <- rvec_dbl()
    expect_identical(n_draw(x), 1L)
    x <- rvec(matrix(1:10, nr = 1))
    expect_identical(n_draw(x), 10L)
})

test_that("'n_draw' throws correct error for object without random draws", {
    expect_error(n_draw(1:4),
                 "`x` does not appear to hold random draws.")
})

