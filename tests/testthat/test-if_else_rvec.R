
test_that("'if_else_rvec' works with valid inputs - no NA, no missing arg", {
    w <- rvec(matrix(c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE), 3))
    x <- rvec(matrix(1:6, 3))
    y <- 100
    ans_obtained <- if_else_rvec(w, x, y)
    ans_expected <- rvec(matrix(c(1, 100, 3, 4, 5, 100), nr = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'if_else_rvec' works with valid inputs - has NA, no missing arg", {
    w <- rvec(matrix(c(TRUE, FALSE, TRUE, TRUE, NA, FALSE), 3))
    x <- rvec(matrix(1:6, 3))
    y <- 100
    ans_obtained <- if_else_rvec(w, x, y)
    ans_expected <- rvec(matrix(c(1, 100, 3, 4, NA, 100), nr = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'if_else_rvec' works with valid inputs - has missing", {
    w <- rvec(matrix(c(TRUE, FALSE, TRUE, TRUE, NA, FALSE), 3))
    x <- rvec(matrix(1:6, 3))
    y <- 100
    missing <- rvec(matrix(11:16, nr = 3))
    ans_obtained <- if_else_rvec(w, x, y, missing)
    ans_expected <- rvec(matrix(c(1, 100, 3, 4, 15, 100), nr = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'if_else_rvec' works with valid inputs - rvec arguments different lengths", {
    w <- rvec(matrix(c(TRUE, FALSE, TRUE, TRUE, NA, FALSE), 3))
    x <- 1:3
    y <- rvec(list(101:102))
    ans_obtained <- if_else_rvec(w, x, y)
    ans_expected <- rvec(matrix(c(1L, 101L, 3L, 1L, NA, 102L), nr = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'if_else_rvec' works with valid inputs - arguments length 0", {
    w <- rvec(matrix(NA, nr = 0, nc = 2))
    x <- integer()
    y <- rvec_lgl()
    ans_obtained <- if_else_rvec(w, x, y)
    ans_expected <- rvec_int(matrix(nr = 0, nc = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'if_else_rvec' works with valid inputs - rvec argument has 1 draw", {
    w <- rvec(matrix(c(TRUE, FALSE, TRUE, TRUE, NA, FALSE), 3))
    x <- 1:3
    y <- rvec(101:103)
    ans_obtained <- if_else_rvec(w, x, y)
    ans_expected <- rvec(matrix(c(1L, 102L, 3L, 1L, NA, 103L), nr = 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'if_else_rvec' preserves branch types when branches are unused", {
    condition <- rvec(matrix(TRUE, nrow = 2, ncol = 2))

    ans <- if_else_rvec(condition, 1:2, c(3, 4))
    expect_identical(ans, rvec_dbl(matrix(c(1, 2, 1, 2), nrow = 2)))

    ans <- if_else_rvec(condition, 1:2, 3:4, missing = c("a", "b"))
    expect_identical(ans, rvec_chr(matrix(c("1", "2", "1", "2"), nrow = 2)))
})

test_that("'if_else_rvec' selects from compact branch inputs", {
    condition <- rvec(matrix(c(TRUE, FALSE, NA,
                               FALSE, TRUE, NA), nrow = 3))
    true <- c(one = 1L, two = 2L, three = 3L)
    false <- rvec(c(one = 11L, two = 12L, three = 13L))
    missing <- rvec(matrix(21:26, nrow = 3))

    ans <- if_else_rvec(condition, true, false, missing)
    expected <- rvec(matrix(c(1L, 12L, 23L,
                              11L, 2L, 26L), nrow = 3))
    expect_identical(ans, expected)
})

test_that("'if_else_rvec' throws correct error with non-rvec condition", {
    w <- c(TRUE, FALSE, TRUE)
    x <- 1:3
    y <- rvec(101:103)
    expect_error(if_else_rvec(w, x, y),
                 "`condition` is not an rvec.")
})

test_that("'if_else_rvec' throws correct error with non-logical rvec condition", {
    w <- rvec(matrix(1:6, nr = 3))
    x <- 1:3
    y <- rvec(101:103)
    expect_error(if_else_rvec(w, x, y),
                 "`condition` is not a logical rvec.")
})

test_that("'if_else_rvec' throws correct error when condition, true have different draws", {
    w <- rvec(c(TRUE, FALSE, TRUE))
    x <- rvec(matrix(1:6, nr = 3))
    y <- rvec(101:103)
    expect_error(if_else_rvec(w, x, y),
                 "`condition` has 1 draw but `true` has 2 draws.")
})

test_that("'if_else_rvec' throws correct error when condition, false have different draws", {
    w <- rvec(matrix(c(TRUE, FALSE, TRUE, FALSE, TRUE, NA), nr = 3))
    x <- rvec(matrix(1:6, nr = 3))
    y <- rvec(matrix(101:109, 3))
    expect_error(if_else_rvec(w, x, y),
                 "`condition` has 2 draws but `false` has 3 draws.")
})

test_that("'if_else_rvec' throws correct error when condition, missing have different draws", {
    w <- rvec(matrix(c(TRUE, FALSE, TRUE, FALSE, TRUE, NA), nr = 3))
    x <- rvec(matrix(1:6, nr = 3))
    y <- 1:3
    z <- rvec(matrix(101:109, 3))
    expect_error(if_else_rvec(w, x, y, z),
                 "`condition` has 2 draws but `missing` has 3 draws.")
})




                 
    



