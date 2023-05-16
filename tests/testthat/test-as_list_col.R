
test_that("'as_list_col' works with rvecs", {
    m <- rbind(a = 1:3,
               b = 4:6)
    x <- rvec(m)
    ans_obtained <- as_list_col(x)
    ans_expected <- list(a = 1:3, b = 4:6)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'as_list_col' works with matrix", {
    x <- rbind(a = 1:3,
               b = 4:6)
    ans_obtained <- as_list_col(x)
    ans_expected <- list(a = 1:3, b = 4:6)
    expect_identical(ans_obtained, ans_expected)
})
