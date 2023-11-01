
## 'order' --------------------------------------------------------------------

test_that("'order' works when n_draw is 1, length is 0", {
    x <- rvec_lgl()
    expect_identical(order(x), integer())
})

test_that("'order' works when n_draw is 1, length is 3", {
    x <- rvec_lgl(list(TRUE, FALSE, NA))
    expect_identical(order(x), order(c(TRUE, FALSE, NA)))
})

test_that("'order' throws expected error when n_draw is not 1", {
    x <- rvec(list(c(TRUE, FALSE)))
    expect_error(order(x),
                 "Sorting of rvec only defined when `n_draw` is 1.")
})


## 'rank' --------------------------------------------------------------------

test_that("existing version of 'rank' still works as normal", {
    x <- c(3, 2, 8, 1)
    ans_obtained <- rank(x)
    ans_expected <- c(3, 2, 4, 1)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rank' works when 'na.last' is TRUE and there are NA", {
    x <- rvec(list(c("a", NA), c("b", "z")))
    ans_obtained <- rank(x)
    ans_expected <- rvec(list(c(1L, 2L), c(2L, 1L)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rank' works when 'na.last' is TRUE and there are no NA, but have character", {
    x <- rvec(list(c("a", "y"), c("b", "z")))
    ans_obtained <- rank(x)
    ans_expected <- rvec(list(c(1L, 1L), c(2L, 2L)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rank' works when 'na.last' is TRUE and there are no NA, or character", {
    x <- rvec(list(c(100, 60), c(10, Inf), c(0, -Inf)))
    ans_obtained <- rank(x)
    ans_expected <- rvec(list(c(3L, 2L), c(2L, 3L), c(1L, 1L)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rank' works when 'na.last' is 'keep' and there are NA", {
    x <- rvec(list(c("a", NA), c("b", "z")))
    ans_obtained <- rank(x, na.last = "keep")
    ans_expected <- rvec(list(c(1L, NA), c(2L, 1L)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rank' works when 'na.last' is 'keep' and there are no NA, but have character", {
    x <- rvec(list(c("a", "y"), c("b", "z")))
    ans_obtained <- rank(x, na.last = "keep")
    ans_expected <- rvec(list(c(1L, 1L), c(2L, 2L)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rank' works when 'na.last' is 'keep' and there are no NA, or character", {
    x <- rvec(list(c(100, 60), c(10, Inf), c(0, -Inf)))
    ans_obtained <- rank(x, na.last = "keep")
    ans_expected <- rvec(list(c(3L, 2L), c(2L, 3L), c(1L, 1L)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rank' throws appropriate error when 'na.last' invalid", {
    x <- rvec(list(c(100, 60), c(10, Inf), c(0, -Inf)))
    expect_error(rank(x, na.last = "wrong"),
                 "`na.last` is \"wrong\"")
})


## 'sort' --------------------------------------------------------------------

test_that("'sort' works when n_draw is 1, length is 0", {
    x <- rvec_lgl()
    expect_identical(sort(x), x)
})

test_that("'sort' works when n_draw is 1, length is 3", {
    x <- rvec_lgl(list(TRUE, FALSE, NA))
    expect_identical(sort(x), rvec(c(FALSE, TRUE)))
    expect_identical(sort(x, decreasing = TRUE), rvec(c(TRUE, FALSE)))
})

test_that("'sort' throws expected error when n_draw is not 1", {
    x <- rvec(list(c(TRUE, FALSE)))
    expect_error(sort(x),
                 "Sorting of rvec only defined when `n_draw` is 1.")
})


## 'xtfrm' --------------------------------------------------------------------

test_that("'xtfrm' works when n_draw is 1", {
    x <- rvec(list("a", "c", "b"))
    ans_obtained <- xtfrm(x)
    ans_expected <- xtfrm(c("a", "c", "b"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'xtfrm' throws expected error when n_draw is not 1", {
    x <- rvec_lgl(matrix(1, nr = 3, nc = 3))
    expect_error(xtfrm(x),
                 "Sorting of rvec only defined when `n_draw` is 1.")
})

