

test_that("sd.default works", {
    expect_identical(sd(1:5), stats::sd(1:5))
    expect_identical(sd(numeric()), stats::sd(numeric()))
})

test_that("sd.rvec works, nrow > 0", {
    m <- matrix(1:6, nr = 3)
    x <- rvec(m)
    expect_identical(sd(x),
                     rvec_dbl(matrix(c(sd(m[,1]), sd(m[,2])), nr = 1)))
})

test_that("sd.rvec works, nrow == 0", {
    m <- matrix(0, nr = 0, nc = 3)
    x <- rvec(m)
    expect_identical(sd(x), 
                     rvec_dbl(matrix(rep(NA, 3), nr = 1)))
})

test_that("sd.chr throws correct error", {
    x <- rvec(matrix(letters, nr = 1))
    expect_error(sd(x), 
                 "Standard deviation not defined for character vectors")
})
