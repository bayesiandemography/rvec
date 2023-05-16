



## 'n_draw' -------------------------------------------------------------------

test_that("'n_draw' method for rvec works", {
    x <- rvec_dbl()
    expect_identical(n_draw(x), 1L)
    x <- rvec(matrix(1:10, nr = 1))
    expect_identical(n_draw(x), 10L)
})
