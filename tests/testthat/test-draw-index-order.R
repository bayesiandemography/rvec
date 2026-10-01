test_that("which.min and which.max find positions within each draw", {
    inputs <- list(
        rbind(c(3, 1, 2), c(1, 3, 2), c(1, Inf, 2), c(4, -Inf, 2)),
        rbind(c(NA_integer_, 1L, 3L), c(2L, 1L, 1L),
              c(1L, 2L, NA_integer_), c(3L, 3L, 2L)),
        rbind(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, NA),
              c(TRUE, TRUE, FALSE), c(NA, FALSE, TRUE)),
        rbind(c("3", "1", "2"), c("1", "3", "2"),
              c("1", "2", "1"), c("4", "4", "4"))
    )
    for (m in inputs) {
        rownames(m) <- paste0("element", seq_len(nrow(m)))
        x <- rvec(m)
        before <- serialize(x, NULL)
        for (fun in list(which.min, which.max)) {
            base_fun <- if (identical(fun, which.min)) base::which.min
                        else base::which.max
            expected <- vapply(seq_len(ncol(m)), function(j)
                base_fun(m[, j]), integer(1))
            actual <- fun(x)
            expect_identical(as.matrix(actual), matrix(expected, nrow = 1L))
            expect_null(names(actual))
        }
        expect_identical(serialize(x, NULL), before)
    }
})

test_that("which.min and which.max warn once for draws with no valid value", {
    m <- rbind(c(NA_real_, NA_real_, NA_real_),
               c(NaN, 2, NA_real_),
               c(NA_real_, 1, NaN))
    x <- rvec(m)
    for (fun in list(which.min, which.max)) {
        warnings <- character()
        actual <- withCallingHandlers(fun(x), warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        })
        expect_identical(as.matrix(actual), matrix(c(NA_integer_,
                                                      if (identical(fun, which.min)) 3L else 2L,
                                                      NA_integer_), nrow = 1L))
        expect_length(warnings, 1L)
        expect_match(warnings, "2 of 3 draws")
    }
    all_missing <- rvec(matrix(NA_real_, nrow = 2L, ncol = 3L))
    expect_warning(ans <- which.min(all_missing), "3 of 3 draws")
    expect_identical(as.matrix(ans), matrix(rep(NA_integer_, 3L), nrow = 1L))
})

test_that("which.min and which.max return an empty rvec for empty input", {
    for (nd in c(1L, 3L)) for (type in c("logical", "integer", "double", "character")) {
        m <- matrix(vector(type, 0L), nrow = 0L, ncol = nd)
        x <- rvec(m)
        for (fun in list(which.min, which.max)) {
            expect_no_warning(actual <- fun(x))
            expect_identical(as.matrix(actual),
                             matrix(integer(), nrow = 0L, ncol = nd))
        }
    }
})

test_that("index wrappers retain singleton draw layouts and subclass dispatch", {
    x <- rvec(matrix(c(NA_integer_, 4L, 2L), nrow = 1L))
    for (fun in list(which.min, which.max)) {
        expect_warning(actual <- fun(x), "1 of 3 draws")
        expect_identical(as.matrix(actual),
                         matrix(c(NA_integer_, 1L, 1L), nrow = 1L))
    }

    y <- rvec(matrix(c(3L, 1L, 2L, 4L), nrow = 2L))
    class(y) <- c("special_rvec", class(y))
    expect_identical(as.matrix(rvec::which.min(y)), matrix(c(2L, 1L), nrow = 1L))
    expect_identical(as.matrix(rvec::which.max(y)), matrix(c(1L, 2L), nrow = 1L))

    one_draw <- rvec(matrix(c(4, NA_real_, 2, 2), ncol = 1L))
    expect_identical(as.matrix(which.min(one_draw)), matrix(3L, nrow = 1L))
    expect_identical(as.matrix(which.max(one_draw)), matrix(1L, nrow = 1L))
})

test_that("which.min and which.max retain base behavior for ordinary inputs", {
    inputs <- list(numeric(), c(NA_real_, NaN), c(1, NA, 2),
                   c(TRUE, FALSE), c("2", "1"), c("a", "1"),
                   factor(c("b", "a")))
    for (x in inputs) for (pair in list(list(which.min, base::which.min),
                                        list(which.max, base::which.max))) {
        expect_identical(suppressWarnings(pair[[1L]](x)),
                         suppressWarnings(pair[[2L]](x)))
    }
    expect_warning(which.min(c("a", "1")), "NAs introduced by coercion")
    expect_warning(which.max(c("a", "1")), "NAs introduced by coercion")
    expect_identical(base::which.min(c(NA_real_, NaN)), integer())
})

test_that("character rvecs retain base coercion and warnings", {
    x <- rvec(rbind(c("2", "a"), c("1", "b")))
    for (fun in list(which.min, which.max)) {
        warnings <- character()
        actual <- withCallingHandlers(fun(x), warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        })
        expect_identical(as.matrix(actual), matrix(c(if (identical(fun, which.min)) 2L else 1L,
                                                      NA_integer_), nrow = 1L))
        expect_true(any(grepl("NAs introduced by coercion", warnings)))
        expect_true(any(grepl("1 of 2 draws", warnings)))
    }
})
