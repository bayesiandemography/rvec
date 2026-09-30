test_that("quantile matches base algorithms within each draw", {
    inputs <- list(
        rbind(c(9, 1, -2), c(1, 7, 4), c(3, 3, 0), c(3, 8, 2)),
        matrix(c(1L, 2L, 2L, 8L, 9L, 0L), 3),
        matrix(c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE), 3),
        rbind(c(NA, NaN), c(2, NA), c(4, NA)),
        rbind(c(-Inf, 0), c(0, Inf), c(Inf, 2)),
        matrix(numeric(), 0, 3), matrix(integer(), 0, 3),
        matrix(logical(), 0, 3), matrix(5, 1, 3), matrix(1:4, 4)
    )
    for (m in inputs) {
        x <- rvec(m)
        before <- serialize(x, NULL)
        for (type in 1:9) for (named in c(TRUE, FALSE)) {
            for (probs in list(c(0, 0.25, 0.5, 0.75, 1), c(0.9, 0.1, 0.9),
                               0.5, numeric())) {
                results <- lapply(seq_len(ncol(m)), function(j)
                    stats::quantile(m[, j], probs, na.rm = TRUE,
                                    names = named, type = type))
                expected <- matrix(unlist(results, use.names = FALSE),
                                   ncol = ncol(m))
                rownames(expected) <- names(results[[1L]])
                expect_identical(as.matrix(quantile(x, probs, na.rm = TRUE,
                                                   names = named, type = type)),
                                 expected, info = paste(typeof(m), type, named))
            }
        }
        expect_identical(serialize(x, NULL), before)
    }
})

test_that("quantile preserves probability names rather than input names", {
    x <- rvec(rbind(a = c(1, 10), b = c(3, 2), c = c(2, 6)))
    expect_identical(as.matrix(quantile(x, c(0, 0.5, 1))),
                     rbind("0%" = c(1, 2), "50%" = c(2, 6), "100%" = c(3, 10)))
    expect_null(names(quantile(x, names = FALSE)))
    expect_identical(n_draw(quantile(x, numeric())), 2L)
    expect_length(quantile(x, numeric()), 0L)
    expect_identical(quantile(x), quantile(x, seq(0, 1, 0.25)))
})

test_that("quantile follows base character support and validation", {
    m <- rbind(c("c", "a"), c("a", "b"), c("b", "c"))
    for (type in c(1, 3)) {
        expected <- sapply(1:2, function(j) stats::quantile(m[, j], type = type))
        expect_identical(as.matrix(quantile(rvec(m), type = type)), expected)
    }
    for (x in list(rvec(c(1, NA)), rvec(c(1, NaN))))
        expect_error(quantile(x), "missing values")
    expect_error(quantile(rvec(1:3), probs = -0.1), "probs")
    # Older R versions use a different diagnostic for an invalid algorithm.
    base_error <- tryCatch(stats::quantile(1:3, type = 10), error = identity)
    expect_s3_class(base_error, "error")
    actual_error <- expect_error(quantile(rvec(1:3), type = 10))
    expect_identical(conditionMessage(actual_error), conditionMessage(base_error))
    expect_error(quantile(rvec(m)), "non-numeric")
    expect_identical(quantile(1:3), stats::quantile(1:3))
})

test_that("quantile forwards additional base options", {
    x <- rvec(matrix(1:8, 4))
    probs <- c(0.1234567, 0.9876543)
    expected <- sapply(1:2, function(j)
        stats::quantile(as.matrix(x)[, j], probs, digits = 3))
    expect_identical(as.matrix(quantile(x, probs, digits = 3)), expected)
})


test_that("quantile preserves missing probabilities and forwarded fuzz", {
    m <- rbind(c(1, 9), c(2, 4), c(8, 3))
    x <- rvec(m)
    probs <- c(high = 1, missing = NA_real_, low = 0, middle = 0.5)
    for (type in 1:9) {
        expected <- sapply(1:2, function(j)
            stats::quantile(m[, j], probs, type = type, fuzz = 0))
        expect_identical(as.matrix(quantile(x, probs, type = type, fuzz = 0)),
                         expected)
    }
})
