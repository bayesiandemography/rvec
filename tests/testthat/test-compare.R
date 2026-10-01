

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

test_that("'compare_rvec' compares matching subclasses via the fallback path", {
    x <- rvec(rbind(a = c(1, NA_real_), b = c(3, 2)))
    class(x) <- c("special_rvec", class(x))
    y <- rvec(rbind(a = c(2, 0), b = c(2, 2)))
    class(y) <- class(x)

    expect_identical(compare_rvec(x, y, "<"),
                     rvec(rbind(a = c(TRUE, NA), b = c(FALSE, FALSE))))
    expect_identical(compare_rvec(y, x, "<"),
                     rvec(rbind(a = c(FALSE, NA), b = c(TRUE, FALSE))))
})






                         
    


test_that("comparisons preserve common types and compact draw alignment", {
    inputs <- list()
    for (v in list(c(1, NA_real_), c(1L, 2L), c(TRUE, NA), c("1", "a"))) {
        inputs <- c(inputs, list(v, rvec(setNames(v, c("a", "b"))),
                                 rvec(matrix(rep(v, 3L), 2L, 3L))))
    }
    for (x in inputs) for (y in inputs) {
        if (!is_rvec(x) && !is_rvec(y)) next
        args <- vec_cast_common(!!!vec_recycle_common(x, y))
        matrices <- lapply(args, as.matrix)
        for (op in c("==", "!=", "<", "<=", ">", ">=")) {
            fun <- getExportedValue("base", op)
            expected <- rvec(fun(matrices[[1L]], matrices[[2L]]))
            expect_identical(fun(x, y), expected)
        }
    }
    x <- rvec(matrix(1, 2L, 3L))
    expect_error(x == rvec(matrix(1, 2L, 2L)), "Can't align",
                 class = "vctrs_error_incompatible_type")
    expect_error(x < 1:3, class = "vctrs_error_incompatible_size")
    empty <- rvec(matrix(logical(), 0L, 3L))
    expect_identical(rvec(matrix(numeric(), 0L, 3L)) < 1, empty)
})
