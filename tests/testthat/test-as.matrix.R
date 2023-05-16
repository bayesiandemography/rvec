


## 'as.matrix' ----------------------------------------------------------------

test_that("'as.matrix' method for rvec works", {
    m <- matrix(c(T, F), nr = 1)
    x <- rvec(m)
    expect_identical(as.matrix(x), m)
    m <- matrix(character(), nr = 0, nc = 3)
    x <- rvec(m)
    expect_identical(as.matrix(x), m)
})
