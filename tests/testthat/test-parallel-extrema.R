test_that("parallel extrema apply bounds independently within each draw", {
    m <- rbind(a = c(-2, 3), b = c(4, -1))
    x <- rvec(m)
    expect_identical(as.matrix(pmax(x, 0)), rbind(a = c(0, 3), b = c(4, 0)))
    expect_identical(as.matrix(pmax(0, x)), unname(rbind(c(0, 3), c(4, 0))))
    expect_identical(as.matrix(pmin(pmax(x, 0), 1)), rbind(a = c(0, 1), b = c(1, 0)))
})

test_that("parallel extrema match base calculations across types and missing values", {
    inputs <- list(c(-Inf, NA, NaN, Inf), c(NA, NaN, 2, -1),
                   c(1L, NA_integer_, 3L, 4L), c(TRUE, FALSE, NA, TRUE),
                   c("b", "a", NA, "z"))
    for (a in inputs) for (b in inputs) {
        m <- cbind(a, rev(a))
        x <- rvec(m)
        before <- serialize(x, NULL)
        for (op in c("pmin", "pmax")) for (remove in c(FALSE, TRUE)) {
            fun <- get(op)
            base_fun <- get(op, baseenv())
            expected <- cbind(base_fun(a, b, na.rm = remove),
                              base_fun(rev(a), b, na.rm = remove))
            expect_identical(as.matrix(fun(x, b, na.rm = remove)), expected)
            expect_identical(as.matrix(fun(x, rvec(b), na.rm = remove)), expected)
            expected_reverse <- cbind(base_fun(b, a, na.rm = remove),
                                      base_fun(b, rev(a), na.rm = remove))
            expect_identical(as.matrix(fun(b, x, na.rm = remove)), expected_reverse)
        }
        expect_identical(serialize(x, NULL), before)
    }
})

test_that("parallel extrema align elements and draws without fractional recycling", {
    x <- rvec(matrix(c(1, 5), 1, 2))
    for (fun in list(pmin, pmax)) {
        base_fun <- if (identical(fun, pmin)) base::pmin else base::pmax
        expect_identical(as.matrix(fun(x, 1:3, rvec(2))),
                         cbind(base_fun(1, 1:3, 2), base_fun(5, 1:3, 2)))
        expect_error(fun(rvec(1:2), 1:3), class = "vctrs_error_incompatible_size")
        expect_error(fun(rvec(1), x, rvec(matrix(1, 1, 3))), "Can't align")
        empty <- rvec(matrix(integer(), 0, 3))
        expect_identical(fun(empty, 1L), empty)
        expect_identical(fun(rvec(1), integer()), rvec(matrix(numeric(), 0, 1)))
        expect_identical(fun(x), x)
        named_bound <- c(bound = 0)
        named_x <- rvec(rbind(a = c(-2, 3), b = c(4, -1)))
        expected <- cbind(base_fun(named_bound, c(a = -2, b = 4)),
                          base_fun(named_bound, c(a = 3, b = -1)))
        expect_identical(as.matrix(fun(named_bound, named_x)), expected)
        expect_identical(fun(value = x, bound = 0), fun(x, 0))
        expect_error(fun(x, matrix(1, 1, 1)), "unclassed")
        expect_error(fun(x, as.Date("2020-01-01")), "unclassed")
        expect_identical(fun(rvec(1), NULL), rvec(matrix(numeric(), 0, 1)))
    }
})

test_that("calls without rvecs retain base behaviour", {
    inputs <- list(list(1:3, 2), list(matrix(1:4, 2), 2),
                   list(as.Date(c("2020-01-01", "2020-02-01")), as.Date("2020-01-15")),
                   list(c(a = 1, b = NA), 2), list(integer(), 1))
    for (op in c("pmin", "pmax")) {
        for (args in inputs) for (remove in c(FALSE, TRUE)) {
            args$na.rm <- remove
            expect_identical(do.call(get(op), args), do.call(get(op, baseenv()), args))
        }
        expect_error(get(op)(), "no arguments")
        expect_warning(get(op)(1:3, 1:2), "fractionally recycled")
    }
})
