

test_that("'==' works with two rvecs, same length", {
    x <- rvec(list(1:2, 3:4, 5:6))
    y <- rvec(list(1:2, c(3L, 5L), c(5L, NA)))
    ans_obtained <- x == y
    ans_expected <- rvec(list(c(TRUE, TRUE),
                              c(TRUE, FALSE),
                              c(TRUE, NA)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'!=' works with two rvecs, different length", {
    x <- rvec(list(1:2))
    y <- rvec(list(1:2, c(3L, 5L), c(5L, NA)))
    ans_obtained <- x != y
    ans_expected <- rvec(list(c(FALSE, FALSE),
                              c(TRUE, TRUE),
                              c(TRUE, NA)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'<' works with two rvecs, different length", {
    x <- rvec(list(c("a", "b")))
    y <- rvec(list(c("a", "b"), c("c", "d"), c("e", NA)))
    ans_obtained <- x < y
    ans_expected <- rvec(list(c(FALSE, FALSE),
                              c(TRUE, TRUE),
                              c(TRUE, NA)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'<=' works with two rvecs, different length", {
    x <- rvec(list(c("a", "b")))
    y <- rvec(list(c("a", "b"), c("c", "d"), c("e", NA)))
    ans_obtained <- x <= y
    ans_expected <- rvec(list(c(TRUE, TRUE),
                              c(TRUE, TRUE),
                              c(TRUE, NA)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'>=' works with scalar and rvec ", {
    x <- 2.3
    y <- rvec(list(1:2, 3:4, c(NA, -1L)))
    ans_obtained <- x >= y
    ans_expected <- rvec(list(c(TRUE, TRUE),
                              c(FALSE, FALSE),
                              c(NA, TRUE)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'>' works with numeric and rvec ", {
    x <- c(1, 2, 3)
    y <- rvec(list(1:2, 3:4, c(NA, -1L)))
    ans_obtained <- x > y
    ans_expected <- rvec(list(c(FALSE, FALSE),
                              c(FALSE, FALSE),
                              c(NA, TRUE)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'compare_rvec' works with two rvecs", {
    x <- rvec(list(1:2, 3:4, c(NA, -1L)))
    ans_obtained <- compare_rvec(x, x, "==")
    ans_expected <- rvec(list(c(TRUE, TRUE),
                              c(TRUE, TRUE),
                              c(NA, TRUE)))
    expect_identical(ans_obtained, ans_expected)
})






                         
    
