
test_that("'summary' works with 'rvec_dbl'", {
    x <- rvec(list(c(0.1, 0.1), c(0.2, 0.3), c(0.3, 0.4)))
    ans_obtained <- summary(x)
    ans_expected <- c(Length = 3L,
                      Class = "rvec_dbl",
                      Mode = "numeric",
                      n_draw = 2L)
    class(ans_expected) <- c("summaryDefault", "table")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'summary' works with 'rvec_chr'", {
    x <- rvec(list(c("a", "b"), c("c", "d")))
    ans_obtained <- summary(x)
    ans_expected <- c(Length = 2L,
                      Class = "rvec_chr",
                      Mode = "character",
                      n_draw = 2L)
    class(ans_expected) <- c("summaryDefault", "table")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'summary' works with 'rvec_int'", {
    x <- rvec_int()
    ans_obtained <- summary(x)
    ans_expected <- c(Length = 0L,
                      Class = "rvec_int",
                      Mode = "numeric",
                      n_draw = 1L)
    class(ans_expected) <- c("summaryDefault", "table")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'summary' works with 'rvec_chr'", {
    x <- rvec(c(TRUE, FALSE, NA))
    ans_obtained <- summary(x)
    ans_expected <- c(Length = 3L,
                      Class = "rvec_lgl",
                      Mode = "logical",
                      n_draw = 1L)
    class(ans_expected) <- c("summaryDefault", "table")
    expect_identical(ans_obtained, ans_expected)
})



