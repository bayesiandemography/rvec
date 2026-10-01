

    
test_that("'vec_arith' works with rvec_dbl", {
    ## rvec_dbl
    expect_identical(rvec_dbl(matrix(1:2, nr = 1)) + rvec_dbl(matrix(2:5, nr = 2)),
                     rvec_dbl(matrix(c(3, 4, 6, 7), nr = 2)))
    ## rvec_int
    expect_identical(rvec_dbl(matrix(1:2, nr = 1)) + rvec_int(matrix(2:1, nr = 1)),
                     rvec_dbl(matrix(c(3L, 3L), nr = 1)))
    expect_identical(rvec_int(matrix(1:2, nr = 1)) - rvec_dbl(matrix(c(1, NA), nr = 1)),
                     rvec_dbl(matrix(c(0, NA), nr = 1)))
    ## rvec_lgl
    expect_identical(rvec_dbl(matrix(1:4, nr = 2)) +
                     rvec_lgl(matrix(c(TRUE, FALSE), nr = 1)),
                     rvec_dbl(matrix(c(2, 3, 3, 4), nr = 2)))
    expect_identical(rvec_lgl(matrix(TRUE)) + rvec_dbl(matrix(-1)),
                     rvec_dbl(matrix(0)))
    ## double 
    expect_identical(rvec_dbl(matrix(2:3, nr = 1)) + c(0.2, 0.3),
                     rvec_dbl(matrix(c(2.2, 2.3, 3.2, 3.3), nr = 2)))
    expect_identical(-1 * rvec_dbl(matrix(-1)),
                     rvec_dbl(matrix(1)))
    ## integer 
    expect_identical(rvec_dbl(matrix(2:3, nr = 1)) + 1L,
                     rvec_dbl(matrix(c(3, 4), nr = 1)))
    expect_identical(-1L * rvec_dbl(matrix(-1)),
                     rvec_dbl(matrix(1)))
    ## logical
    expect_identical(rvec_dbl(matrix(2:5, nr = 1)) * FALSE,
                     rvec_dbl(matrix(rep(0, 4), nr = 1)))
    expect_identical(c(TRUE, FALSE) - rvec_dbl(matrix(2:5, nr = 1)),
                     rvec_dbl(rbind(-(1:4),
                                    -(2:5))))
    ## missing
    m <- matrix(2:5, nr = 1)
    x <- rvec_dbl(m)
    y <- rvec_dbl(-m)
    expect_identical(-x, y)
    expect_identical(+x, x)
})

test_that("'vec_arith' works with rvec_int", {
    ## rvec_dbl
    expect_identical(rvec_int(matrix(1:2, nr = 1)) + rvec_dbl(matrix(2:3, nr = 1)),
                     rvec_dbl(matrix(c(3, 5), nr = 1)))
    expect_identical(rvec_dbl(matrix(1:2, nr = 1)) + rvec_int(matrix(2:3, nr = 1)),
                     rvec_dbl(matrix(c(3, 5), nr = 1)))
    ## rvec_int
    expect_identical(rvec_int(matrix(1:2, nr = 1)) + rvec_int(rbind(2:1, 1:2)),
                     rvec_int(rbind(c(3L, 3L),
                                    c(2L, 4L))))
    expect_identical(rvec_int(matrix(1:2, nr = 1)) - rvec_int(matrix(c(1, NA), nr = 1)),
                     rvec_int(matrix(c(0L, NA), nr = 1)))
    ## rvec_lgl
    expect_identical(rvec_int(matrix(2:3, nr = 1)) +
                     rvec_lgl(matrix(c(TRUE, FALSE), nr = 1)),
                     rvec_int(matrix(c(3, 3), nr = 1)))
    expect_identical(rvec_lgl(matrix(TRUE)) + rvec_int(matrix(-1)),
                     rvec_int(matrix(0)))
    ## double 
    expect_identical(rvec_int(matrix(2:3, nr = 1)) + c(0.2, 0.3),
                     rvec_dbl(matrix(c(2.2, 2.3, 3.2, 3.3), nr = 2)))
    expect_identical(-1 * rvec_int(matrix(-1)),
                     rvec_dbl(matrix(1)))
    ## integer 
    expect_identical(rvec_int(matrix(2:3, nr = 1)) + 1L,
                     rvec_int(matrix(c(3, 4), nr = 1)))
    expect_identical(-1L * rvec_int(matrix(-1)),
                     rvec_int(matrix(1)))
    ## logical
    expect_identical(rvec_int(matrix(2:5, nr = 1)) * FALSE,
                     rvec_int(matrix(rep(0, 4), nr = 1)))
    expect_identical(c(TRUE, FALSE) - rvec_int(matrix(2:5, nr = 1)),
                     rvec_int(rbind(-(1:4),
                                    -(2:5))))
    ## missing
    m <- matrix(2:5, nr = 1)
    x <- rvec_int(m)
    y <- rvec_int(-m)
    expect_identical(-x, y)
    expect_identical(+x, x)
})

test_that("'vec_arith' works with rvec_lgl", {
    ## rvec_dbl
    expect_identical(rvec_lgl(matrix(c(TRUE, FALSE), nr = 1)) +
                     rvec_dbl(matrix(2:3, nr = 1)),
                     rvec_dbl(matrix(c(3, 3), nr = 1)))
    expect_identical(rvec_dbl(matrix(1:2, nr = 1)) +
                     rvec_lgl(matrix(c(TRUE, FALSE), nr = 1)),
                     rvec_dbl(matrix(c(2, 2), nr = 1)))
    ## rvec_int
    expect_identical(rvec_lgl(matrix(c(FALSE, NA), nr = 1))
                              + rvec_int(rbind(2:1, 1:2)),
                     rvec_int(rbind(c(2L, NA),
                                    c(1L, NA))))
    expect_identical(rvec_int(matrix(1:2, nr = 1)) -
                     rvec_lgl(matrix(c(TRUE, NA), nr = 1)),
                     rvec_int(matrix(c(0L, NA), nr = 1)))
    ## rvec_lgl
    expect_identical(rvec_lgl(matrix(c(TRUE, TRUE), nr = 1)) +
                     rvec_lgl(matrix(c(TRUE, FALSE), nr = 1)),
                     rvec_int(matrix(c(2, 1), nr = 1)))
    ## double 
    expect_identical(rvec_lgl(matrix(c(TRUE, TRUE), nr = 1)) + c(0.2, 0.3),
                     rvec_dbl(matrix(c(1.2, 1.3, 1.2, 1.3), nr = 2)))
    expect_identical(-1 * rvec_lgl(matrix(FALSE)),
                     rvec_dbl(matrix(0)))
    ## integer 
    expect_identical(rvec_lgl(matrix(c(FALSE, TRUE), nr = 1)) + 1L,
                     rvec_int(matrix(c(1, 2), nr = 1)))
    expect_identical(-1L * rvec_lgl(matrix(TRUE)),
                     rvec_int(matrix(-1)))
    ## logical
    expect_identical(rvec_lgl(matrix(c(TRUE, FALSE, NA), nr = 1)) * FALSE,
                     rvec_int(matrix(c(0, 0, NA), nr = 1)))
    expect_identical(c(TRUE, FALSE) -
                     rvec_lgl(matrix(c(TRUE, FALSE), nr = 1)),
                     rvec_int(rbind(c(0, 1L),
                                    c(-1, 0))))
    ## missing
    m <- matrix(c(TRUE, FALSE), nr = 1)
    x <- rvec_lgl(m)
    y <- rvec_int(-m)
    z <- rvec_int(m)
    expect_identical(-x, y)
    expect_identical(+x, x)
    expect_identical(!x, rvec(matrix(c(FALSE, TRUE), nr = 1)))
    expect_identical(!(!x), x)
    expect_identical(-y, z)
    expect_identical(!z, !x)
    expect_identical(!(!z), x)
})                     



test_that("binary arithmetic preserves types and recycling across compact layouts", {
    capture <- function(expr) {
        warnings <- character()
        value <- withCallingHandlers(expr, warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        })
        list(value = value, warnings = warnings)
    }
    values <- list(c(0, 2), c(1L, 2147483647L), c(TRUE, NA),
                   c(NA_real_, NaN), c(Inf, -Inf))
    inputs <- unlist(lapply(values, function(v) {
        list(v, v[1L], rvec(v),
             rvec(matrix(v, 1L, 2L)),
             rvec(matrix(rep(v, 2L), 2L, 2L)))
    }), recursive = FALSE)
    for (x in inputs) for (y in inputs) {
        if (!is_rvec(x) && !is_rvec(y)) next
        nd <- max(if (is_rvec(x)) n_draw(x) else 1L,
                  if (is_rvec(y)) n_draw(y) else 1L)
        n <- max(length(x), length(y))
        expand <- function(z) {
            m <- if (is_rvec(z)) as.matrix(z) else matrix(z, ncol = 1L)
            m[rep(seq_len(nrow(m)), length.out = n),
              rep(seq_len(ncol(m)), length.out = nd), drop = FALSE]
        }
        # Match the original operand layouts: only two rvecs expanded draws.
        # Base R can distinguish NA from NaN differently when recycling.
        both_rvec <- is_rvec(x) && is_rvec(y)
        mx <- if (both_rvec) expand(x) else if (is_rvec(x)) as.matrix(x) else x
        my <- if (both_rvec) expand(y) else if (is_rvec(y)) as.matrix(y) else y
        original_x <- serialize(x, NULL)
        original_y <- serialize(y, NULL)
        for (op in c("+", "-", "*", "/", "^", "%%", "%/%")) {
            fun <- getExportedValue("base", op)
            expected <- capture(rvec(vctrs::vec_arith_base(op, mx, my)))
            expect_identical(capture(fun(x, y)), expected,
                             info = paste(op, paste(capture.output(dput(x)), collapse = " "),
                                          paste(capture.output(dput(y)), collapse = " ")))
        }
        expect_identical(serialize(x, NULL), original_x)
        expect_identical(serialize(y, NULL), original_y)
    }
})

test_that("binary arithmetic preserves names, empty results, and alignment errors", {
    x <- rvec(setNames(c(1, 2), c("left1", "left2")))
    y <- rvec(matrix(3:6, 2L, dimnames = list(c("right1", "right2"), NULL)))
    expect_identical(as.matrix(x - y), matrix(c(-2, -2, -4, -4), 2L,
                     dimnames = list(c("left1", "left2"), NULL)))
    expect_identical(as.matrix(y - x), matrix(c(2, 2, 4, 4), 2L,
                     dimnames = list(c("right1", "right2"), NULL)))
    empty <- rvec(matrix(numeric(), 0L, 2L))
    for (op in c("+", "-", "*", "/", "^", "%%", "%/%")) {
        fun <- getExportedValue("base", op)
        expect_identical(fun(empty, rvec(1)), empty)
        expect_identical(fun(rvec(1), empty), empty)
        expect_identical(fun(empty, 1), empty)
        expect_identical(fun(1, empty), empty)
        expect_error(fun(y, rvec(matrix(1, 2L, 3L))),
                     "Can't align", class = "vctrs_error_incompatible_type")
        expect_error(fun(y, 1:3), class = "vctrs_error_incompatible_size")
    }
    expect_error(x + "a", class = "vctrs_error_incompatible_op")
})

test_that("binary arithmetic retains subclass conversion dispatch", {
    x <- rvec(c(1, 2))
    class(x) <- c("arith_test", class(x))
    method <- function(x, n_draw) stop("subclass conversion invoked")
    vctrs::s3_register("rvec::rvec_to_rvec_dbl", "arith_test", method)
    # This class exists only in this test; remove its method on exit.
    withr::defer(rm("rvec_to_rvec_dbl.arith_test",
                   envir = asNamespace("rvec")$.__S3MethodsTable__.))
    expect_error(x + rvec(1), "subclass conversion invoked")
})


test_that("binary arithmetic converts integer subclasses before recycling draws", {
    x <- rvec_int(c(a = 1L, b = 2L))
    class(x) <- c("integer_arith_test", class(x))
    y <- rvec_int(rbind(a = c(3L, 5L), b = c(4L, NA_integer_)))
    expect_identical(x - y,
                     rvec_int(rbind(a = c(-2L, -4L), b = c(-2L, NA_integer_))))
    expect_identical(y - x,
                     rvec_int(rbind(a = c(2L, 4L), b = c(2L, NA_integer_))))
})


test_that("binary arithmetic honours forced double results", {
    x <- rvec_int(rbind(a = c(1L, NA_integer_), b = c(3L, 4L)))
    expect_identical(arith_rvec_binary("+", x, 2L, double = TRUE),
                     rvec_dbl(rbind(a = c(3, NA_real_), b = c(5, 6))))
})


test_that("binary arithmetic retains logical results", {
    x <- rvec_lgl(rbind(a = c(TRUE, NA), b = c(FALSE, TRUE)))
    y <- rvec_lgl(c(a = FALSE, b = TRUE))
    expect_identical(arith_rvec_binary("&", x, y, double = FALSE),
                     rvec_lgl(rbind(a = c(FALSE, FALSE), b = c(FALSE, TRUE))))
})


test_that("binary arithmetic rejects unsupported result types", {
    x <- rvec_dbl(rbind(a = c(1, 2), b = c(3, 4)))
    # Complex results cannot be stored in an rvec; the fallback constructor
    # must reject them rather than silently dropping their imaginary parts.
    expect_error(arith_rvec_binary("+", x, 1i, double = FALSE),
                 "must be double, integer, logical, or character")
})
