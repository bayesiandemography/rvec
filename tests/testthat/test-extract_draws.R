test_that("extract_draws preserves order, duplicates, types, and element names", {
    for (values in list(1:12, as.double(1:12), letters[1:12], rep(c(TRUE, FALSE), 6))) {
        for (nr in c(0L, 1L, 3L)) {
            m <- matrix(values[seq_len(nr * 4L)],
                        nrow = nr, ncol = 4L)
            if (nr > 0L)
                rownames(m) <- letters[seq_len(nr)]
            x <- rvec(m)
            for (i in list(2, c(4, 1, 4, 2, 4), 1:4)) {
                result <- extract_draws(x, i)
                expect_identical(result, rvec(m[, i, drop = FALSE]))
                expect_identical(n_draw(result), as.integer(length(i)))
                expect_identical(names(result), names(x))
            }
        }
    }
    x <- rvec(1:3)
    expect_identical(extract_draws(x, 1), x)
    expect_identical(n_draw(extract_draws(x, rep(1, 5))), 5L)
})


test_that("extract_draws validates required arguments and indices", {
    x <- rvec(matrix(1:12, ncol = 4))
    expect_error(extract_draws(1, 1), "x.*not an rvec")
    expect_error(extract_draws(x), "i.*must be supplied")
    invalid <- list(NULL, numeric(), NA_real_, NaN, Inf, -Inf, 0, -1,
                    1.5, 5, c(1, NA), c(1, 5), TRUE, "1", list(1),
                    matrix(1), array(1, c(1, 1, 1)))
    for (i in invalid)
        expect_error(extract_draws(x, i), "i.*must be a nonempty numeric vector")
})


test_that("thin_draws selects whole draws without replacement in original order", {
    withr::local_seed(42)
    for (values in list(1:30, as.double(1:30), letters[1:10], rep(c(TRUE, FALSE), 15))) {
        m <- matrix(values, nrow = 3, ncol = 10,
                    dimnames = list(c("a", "b", "c"), NULL))
        x <- rvec(m)
        set.seed(42)
        expected_i <- sort(sample.int(10, 4, replace = FALSE))
        set.seed(42)
        result <- thin_draws(x, 4)
        expect_identical(result, rvec(m[, expected_i, drop = FALSE]))
        expect_identical(names(result), names(x))
        expect_identical(n_draw(result), 4L)
        set.seed(42)
        expect_identical(thin_draws(x, 4), result)
        expect_identical(n_draw(thin_draws(x, 1)), 1L)
        empty <- x[integer()]
        expect_identical(thin_draws(empty, 4), rvec(m[integer(), 1:4, drop = FALSE]))
    }
    x <- rvec(matrix(1:100, nrow = 1))
    retained <- as.numeric(as.matrix(thin_draws(x, 40)))
    expect_length(retained, 40)
    expect_true(all(diff(retained) > 0))
    expect_true(all(retained %in% 1:100))
})


test_that("selection leaves random state unchanged when no sampling is needed", {
    withr::local_seed(99)
    state <- .Random.seed
    for (nd in c(1L, 4L)) {
        x <- new_rvec_int(3, nd, value = 2)
        expect_identical(thin_draws(x, nd), x)
        expect_identical(.Random.seed, state)
        empty <- x[integer()]
        expect_identical(thin_draws(empty, nd), empty)
        expect_identical(.Random.seed, state)
        extract_draws(x, c(1, 1))
        expect_identical(.Random.seed, state)
    }
})


test_that("thin_draws validates its required draw count", {
    x <- rvec(matrix(1:12, ncol = 4))
    expect_error(thin_draws(1, 1), "x.*not an rvec")
    expect_error(thin_draws(x), "n_draw_new.*must be supplied")
    invalid <- list(NULL, numeric(), NA_real_, NaN, Inf, -Inf, 0, -1,
                    1.5, 5, c(1, 2), TRUE, "1", list(1),
                    matrix(1), array(1, c(1, 1, 1)))
    for (n in invalid)
        expect_error(thin_draws(x, n), "n_draw_new.*must be a single whole number")
})
