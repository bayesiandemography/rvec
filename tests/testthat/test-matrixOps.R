

test_that("'%*%' works when x is an rvec and y is an rvec", {
    x <- rvec(matrix(1:9, nr = 3))
    y <- rvec(matrix(11:19, nr = 3))
    ans_obtained <- x %*% y
    ans_expected <- rvec(sum(x * y))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'%*%' works when x is a matrix and y is an rvec", {
    x <- matrix(1:9, nr = 3)
    y <- rvec(matrix(1:6, nr = 3))
    ans_obtained <- x %*% y
    ans_expected <- rvec(matrix(1:9, 3) %*% matrix(1:6, 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'%*%' works when x is an rvec and y is a matrix", {
    m <- matrix(1:6, nr = 3)
    x <- rvec(m)
    y <- matrix(11:16, nr = 3)
    ans_obtained <- x %*% y
    ans_expected <- rvec(t(t(m) %*% y))
    expect_identical(ans_obtained, ans_expected)
})


