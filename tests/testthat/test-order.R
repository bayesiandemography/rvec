
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
    ans_expected <- rvec_dbl(list(c(1L, 2L), c(2L, 1L)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rank' works when 'na.last' is TRUE and there are no NA, but have character", {
    x <- rvec(list(c("a", "y"), c("b", "z")))
    ans_obtained <- rank(x)
    ans_expected <- rvec_dbl(list(c(1L, 1L), c(2L, 2L)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rank' works when 'na.last' is TRUE and there are no NA, or character", {
    x <- rvec(list(c(100, 60), c(10, Inf), c(0, -Inf)))
    ans_obtained <- rank(x)
    ans_expected <- rvec_dbl(list(c(3L, 2L), c(2L, 3L), c(1L, 1L)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rank' works when 'na.last' is 'keep' and there are NA", {
    x <- rvec(list(c("a", NA), c("b", "z")))
    ans_obtained <- rank(x, na.last = "keep")
    ans_expected <- rvec_dbl(list(c(1L, NA), c(2L, 1L)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rank' works when 'na.last' is 'keep' and there are no NA, but have character", {
    x <- rvec(list(c("a", "y"), c("b", "z")))
    ans_obtained <- rank(x, na.last = "keep")
    ans_expected <- rvec_dbl(list(c(1L, 1L), c(2L, 2L)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'rank' works when 'na.last' is 'keep' and there are no NA, or character", {
    x <- rvec(list(c(100, 60), c(10, Inf), c(0, -Inf)))
    ans_obtained <- rank(x, na.last = "keep")
    ans_expected <- rvec_dbl(list(c(3L, 2L), c(2L, 3L), c(1L, 1L)))
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



test_that("rank preserves fractional averages and integer tie methods", {
    inputs <- list(
        rbind(c(10, 30), c(10, 10), c(20, 10)),
        rbind(c(1L, 3L), c(1L, 1L), c(2L, 1L)),
        rbind(c(TRUE, FALSE), c(TRUE, TRUE), c(FALSE, TRUE)),
        rbind(c("a", "c"), c("a", "a"), c("b", "a")),
        rbind(c(1, NA), c(1, 2), c(NA, 2), c(3, NaN)),
        rbind(c("a", NA), c("a", "b"), c(NA, "b"), c("c", NA))
    )
    for (m in inputs) {
        rownames(m) <- paste0("element", seq_len(nrow(m)))
        x <- rvec(m)
        before <- serialize(x, NULL)
        for (na_last in list(TRUE, FALSE, "keep")) {
            for (ties in c("average", "first", "last", "min", "max")) {
                expected <- vapply(seq_len(ncol(m)), function(j)
                    base::rank(m[, j], na.last = na_last, ties.method = ties),
                    if (ties == "average") double(nrow(m)) else integer(nrow(m)))
                rownames(expected) <- rownames(m)
                actual <- rank(x, na.last = na_last, ties.method = ties)
                expect_identical(as.matrix(actual), expected,
                                 info = paste(typeof(m), na_last, ties))
            }
        }
        expect_identical(serialize(x, NULL), before)
    }
    expect_identical(rank(c(10, 10, 20)), c(1.5, 1.5, 3))
    expect_identical(as.matrix(rank(rvec(rbind(c(10, 30), c(10, 10), c(20, 10))))),
                     rbind(c(1.5, 3), c(1.5, 1.5), c(3, 1.5)))
})


test_that("rank retains singleton and empty draw layouts", {
    for (value in list(1, NA_real_, 1L, NA_integer_, TRUE, NA, "a", NA_character_)) {
        for (nr in 0:1) for (nd in c(1L, 3L)) {
            m <- matrix(rep(value, nr * nd), nr, nd)
            if (nr == 1L) rownames(m) <- "only"
            x <- rvec(m)
            for (na_last in list(TRUE, FALSE, "keep")) {
                for (ties in c("average", "first", "last", "random", "min", "max")) {
                    results <- lapply(seq_len(nd), function(j)
                        base::rank(m[, j], na.last = na_last, ties.method = ties))
                    expected <- matrix(unlist(results, use.names = FALSE), nr, nd,
                                       dimnames = dimnames(m))
                    actual <- rank(x, na.last = na_last, ties.method = ties)
                    expect_identical(as.matrix(actual), expected,
                                     info = paste(typeof(m), nr, nd, na_last, ties))
                }
            }
        }
    }
})
