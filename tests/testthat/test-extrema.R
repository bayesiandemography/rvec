capture_extrema <- function(expr) {
    warnings <- character()
    value <- withCallingHandlers(expr, warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
    })
    list(value = value, warnings = warnings)
}

test_that("extrema match base R independently within each draw", {
    inputs <- list(
        rbind(c(3, 1, 8), c(1, 7, 3), c(2, 4, 5), c(9, 0, 6)),
        rbind(c(NA, NaN, Inf), c(NaN, NA, -Inf), c(2, 3, 4)),
        rbind(c(NA, Inf, NaN), c(NA, -Inf, NaN)),
        rbind(c(1L, NA_integer_, 4L), c(2L, NA_integer_, 3L)),
        rbind(c(TRUE, NA, FALSE), c(FALSE, NA, TRUE)),
        rbind(c("z", NA, "b"), c("a", NA, "c")),
        matrix(numeric(), 0, 3), matrix(integer(), 0, 3),
        matrix(logical(), 0, 3), matrix(character(), 0, 3),
        matrix(c(NA, NaN, Inf), 1), matrix(3:1, 3)
    )
    for (m in inputs) {
        rownames(m) <- paste0("row", seq_len(nrow(m)))[seq_len(nrow(m))]
        x <- rvec(m)
        original <- serialize(x, NULL)
        for (op in c("min", "max", "range")) {
            fun <- get(op, baseenv())
            for (na_rm in c(FALSE, TRUE)) {
                for (finite in if (op == "range") c(FALSE, TRUE) else FALSE) {
                    options <- list(na.rm = na_rm)
                    if (op == "range") options$finite <- finite
                    expected <- capture_extrema({
                        results <- lapply(seq_len(ncol(m)), function(j)
                            do.call(fun, c(list(m[, j]), options)))
                        rvec(matrix(unlist(results, use.names = FALSE), ncol = ncol(m)))
                    })
                    actual <- capture_extrema(do.call(fun, c(list(x), options)))
                    expect_identical(actual, expected,
                                     info = paste(op, typeof(m), na_rm, finite))
                    expect_null(names(actual$value))
                }
            }
        }
        expect_identical(serialize(x, NULL), original)
    }
})

test_that("extrema combine elements and align draws across arguments", {
    x <- rvec(rbind(c(3, 1), c(1, 7), c(2, 4)))
    y <- rvec(c(0, 5))
    for (op in c("min", "max", "range")) {
        fun <- get(op, baseenv())
        expected <- lapply(1:2, function(j) fun(c(as.matrix(x)[, j], 0, 5, -1, 8)))
        expected <- rvec(matrix(unlist(expected), ncol = 2))
        expect_identical(fun(x, y, c(-1, 8)), expected)
        expect_identical(fun(y, x, c(-1, 8)), expected)
        expect_identical(fun(first = x, second = y, c(-1, 8)), expected)
        expect_identical(fun(x, .fun = y, .ptype = c(-1, 8)), expected)
        expect_error(fun(x, rvec(matrix(1, 2, 3))), "Can't align")
    }
    expect_identical(min(x), rvec(matrix(c(1, 1), 1)))
    expect_identical(max(x), rvec(matrix(c(3, 7), 1)))
    expect_identical(range(x), rvec(rbind(c(1, 1), c(3, 7))))
    expect_identical(min(rvec(c("b", "a")), 1), rvec("1"))
    expect_identical(min(x, NULL), min(x))
    expect_error(range(x, finite = NULL), "length zero")
})

test_that("ordinary summaries and ordering restrictions are unchanged", {
    expect_identical(min(3, 1, 2), 1)
    expect_identical(max(3, 1, 2), 3)
    expect_identical(range(3, 1, 2), c(1, 3))
    x <- rvec(rbind(c(3, 1), c(1, 3)))
    for (fun in list(sort, order, xtfrm))
        expect_error(fun(x), "Sorting of rvec only defined")
})
