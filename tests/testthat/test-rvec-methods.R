
## 'as.matrix' ----------------------------------------------------------------

test_that("'as.matrix' method for rvec works", {
    m <- matrix(c(T, F), nr = 1)
    x <- rvec(m)
    expect_identical(as.matrix(x), m)
    m <- matrix(character(), nr = 0, nc = 3)
    x <- rvec(m)
    expect_identical(as.matrix(x), m)
})


## 'format' -------------------------------------------------------------------

test_that("'format' method for rvec works - one column", {
    x <- rvec(matrix("a", nr = 1))
    ans_obtained <- format(x)
    ans_expected <- "a"
    expect_identical(ans_obtained, ans_expected)
})

test_that("'format' method for rvec works - two columns", {
    x <- rvec(matrix(c(FALSE, TRUE), nr = 2, nc = 2))
    ans_obtained <- format(x)
    ans_expected <- c("F,F", "T,T")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'format' method for rvec works - three columns", {
    x <- rvec(matrix(1:6, nr = 2, nc = 3))
    ans_obtained <- format(x)
    ans_expected <- c("1,3,5", "2,4,6")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'format' method for rvec works - four columns", {
    x <- rvec(matrix(pi, nr = 2, nc = 4))
    ans_obtained <- format(x)
    ans_expected <- c("3.142,..,3.142", "3.142,..,3.142")
    expect_identical(ans_obtained, ans_expected)
})


## 'median' -------------------------------------------------------------------

test_that("'median' method for rvec works - chr", {
    m <- matrix("a", nr = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec(matrix(apply(m, 2, median))))
    x <- rvec_chr()
    expect_identical(median(x), rvec_chr(matrix(median(character()))))
})

test_that("'median' method for rvec works - dbl", {
    set.seed(0)
    m <- matrix(rnorm(15), nr = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec(matrix(apply(m, 2, median), nrow = 1)))
    x <- rvec_dbl()
    expect_identical(median(x), rvec_dbl(matrix(median(double()), nrow = 1)))
    m <- matrix(c(1:3, NA_real_))
    x <- rvec(m)
    expect_identical(median(x), rvec(matrix(apply(m, 2, median), nr = 1)))
    expect_identical(median(x, na.rm = TRUE),
                     rvec(matrix(apply(m, 2, median, na.rm = TRUE), nr = 1)))
})

test_that("'median' method for rvec works - int", {
    m <- matrix(1:15, nr = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec_int(matrix(apply(m, 2, median), nr = 1)))
    m <- matrix(c(1:14, 16), nr = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec_dbl(matrix(apply(m, 2, median), nr = 1)))
    x <- rvec_int()
    expect_identical(median(x), rvec_int(matrix(median(integer()), nr = 1)))
    m <- matrix(c(1:3, NA), nr = 2)
    x <- rvec(m)
    expect_identical(median(x), rvec(matrix(apply(m, 2, median), nr = 1)))
    expect_identical(median(x, na.rm = TRUE),
                     rvec(matrix(apply(m, 2, median, na.rm = TRUE), nr = 1)))
})

test_that("'median' method for rvec works - lgl", {
    m <- matrix(c(TRUE, FALSE, TRUE), nr = 3, ncol = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec_lgl(matrix(apply(m, 2, median), nr = 1)))
    m <- matrix(c(TRUE, FALSE), nr = 2, ncol = 3)
    x <- rvec(m)
    expect_identical(median(x), rvec_dbl(matrix(apply(m, 2, median), nr = 1)))
    x <- rvec_lgl()
    expect_identical(median(x), rvec_lgl(matrix(median(logical()), nr = 1)))
    m <- matrix(c(TRUE, FALSE, TRUE, NA), nr = 2)
    x <- rvec(m)
    expect_identical(median(x), rvec(matrix(apply(m, 2, median), nr = 1)))
    expect_identical(median(x, na.rm = TRUE),
                     rvec(matrix(apply(m, 2, median, na.rm = TRUE), nr = 1)))
})

## 'n_draw' -------------------------------------------------------------------

test_that("'n_draw' method for rvec works", {
    x <- rvec_dbl()
    expect_identical(n_draw(x), 1L)
    x <- rvec(matrix(1:10, nr = 1))
    expect_identical(n_draw(x), 10L)
})


## 'sd' -----------------------------------------------------------------------

test_that("sd.default works", {
    expect_identical(sd(1:5), stats::sd(1:5))
    expect_identical(sd(numeric()), stats::sd(numeric()))
})

test_that("sd.rvec works, nrow > 0", {
    m <- matrix(1:6, nr = 3)
    x <- rvec(m)
    expect_identical(sd(x),
                     rvec_dbl(matrix(c(sd(m[,1]), sd(m[,2])), nr = 1)))
})

test_that("sd.rvec works, nrow == 0", {
    m <- matrix(0, nr = 0, nc = 3)
    x <- rvec(m)
    expect_identical(sd(x), 
                     rvec_dbl(matrix(rep(NA, 3), nr = 1)))
})

test_that("sd.chr throws correct error", {
    x <- rvec(matrix(letters, nr = 1))
    expect_error(sd(x), 
                 "Standard deviation not defined for character vectors")
})


## 'var' ----------------------------------------------------------------------

test_that("var.default works with x only", {
    expect_identical(var(1:5), stats::var(1:5))
})

test_that("var.default works with non-rvec x and non-rvec y", {
    expect_identical(var(1:5, 5:1), stats::var(1:5, 5:1))
})

test_that("var.default works with non-rvec x and rvec y", {
    expect_identical(var(1:5, rvec(matrix(1))),
                     var_rvec_nonrvec(rvec(matrix(1)), 1:5, na.rm = FALSE))
})

test_that("var.rvec works with single rvec", {
    x <- rvec(matrix(1:6, 3))
    expect_identical(var(x), var_rvec(x, na.rm = FALSE))
})

test_that("var.rvec works with two rvecs", {
    x <- rvec(matrix(1:6, 3))
    expect_identical(var(x, x),
                     var_rvec_rvec(x, x, na.rm = FALSE, use = "everything"))
})

test_that("var.rvec works with rvec and non-rvec", {
    x <- rvec(matrix(1:6, 3))
    y <- -3
    expect_identical(var(x, y),
                     var_rvec_nonrvec(e1 = x,
                                      e2 = y,
                                      nm_e2 = "y",
                                      na.rm = FALSE,
                                      use = "everything"))
})

test_that("'var_rvec' works with valid inputs, nrow > 0", {
    set.seed(0)
    m <- matrix(rnorm(10), nr = 5)
    x <- rvec(m)
    ans_obtained <- var_rvec(x, na.rm = FALSE)
    ans_expected <- rvec(matrix(c(var(m[,1]), var(m[,2])), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'var_rvec' works with valid inputs, nrow == 0", {
    m <- matrix(0, nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- var_rvec(x, na.rm = FALSE)
    ans_expected <- rvec(matrix(rep(NA_real_, 5), nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'var_rvec_rvec' works with valid inputs, nrow > 0", {
    set.seed(0)
    m <- matrix(rnorm(10), nr = 5)
    x <- rvec(m)
    ans_obtained <- var_rvec_rvec(x = x, y = x, na.rm = FALSE, use = "everything")
    ans_expected <- rvec(matrix(c(var(m[,1], m[,1]), var(m[,2], m[,2])), nr = 1))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'var_rvec_rvec' works with valid inputs, nrow == 0", {
    m <- matrix(0, nr = 0, ncol = 5)
    x <- rvec(m)
    ans_obtained <- var_rvec_rvec(x = x, y = x, na.rm = FALSE, use = "everything")
    ans_expected <- rvec_dbl(matrix(c(NA, NA, NA, NA, NA), nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'var_rvec_rvec' throws expected error with character", {
    x <- rvec(matrix(rep(1, 5), nr = 1))
    y <- rvec(matrix(rep("a", 5), nr = 1))
    expect_error(var_rvec_rvec(x = x, y = y, na.rm = TRUE, use = "everything"),
                 "Variance not defined for character vectors.")
})

test_that("'var_rvec_nonrvec' works with valid inputs, nrow > 0", {
    m <- matrix(rnorm(10), nr = 5)
    x <- rvec(m)
    y <- 1:5
    ans_obtained <- var_rvec_nonrvec(e1 = x, e2 = y, nm_e2 = "y",
                                     na.rm = FALSE, use = "everything")
    ans_expected <- rvec(matrix(c(var(m[,1], y), var(m[,2], y)), nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'var_rvec_nonrvec' works with valid inputs, nrow == 0", {
    m <- matrix(0, nr = 0, ncol = 5)
    x <- rvec(m)
    y <- integer()
    ans_obtained <- var_rvec_nonrvec(e1 = x, e2 = y, nm_e2 = "y",
                                     na.rm = FALSE, use = "everything")
    ans_expected <- rvec_dbl(matrix(c(NA, NA, NA, NA, NA), nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'var_rvec_nonrvec' throws expected error with non-atomic", {
    x <- rvec(matrix(rep(1, 5), nr = 1))
    y <- list()
    expect_error(var_rvec_nonrvec(e1 = x, e2 = y, nm_e2 = "y",
                                  na.rm = TRUE, use = "everything"),
                 "`y` has class <list>.")
})
                 

## 'vec_arith' ----------------------------------------------------------------

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
})

test_that("'vec_arith' works with rvec_lgo", {
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
})                     


## 'vec_cast' -----------------------------------------------------------------

test_that("'vec_cast' works vec_chr", {
    x <- rvec_chr(matrix("a"))
    expect_identical(vec_cast(x, to = rvec_chr()), x)
    expect_identical(vec_cast("a", to = rvec_chr()), x)
})

test_that("'vec_cast' works vec_dbl", {
    x <- rvec_dbl(matrix(3))
    expect_identical(vec_cast(x, to = rvec_dbl()), x)
    expect_identical(vec_cast(x, to = rvec_int()), rvec_int(matrix(3)))
    expect_identical(vec_cast(3, to = rvec_dbl()), x)
    expect_identical(vec_cast(3L, to = rvec_dbl()), x)
    expect_identical(vec_cast(FALSE, to = rvec_dbl()), rvec_dbl(matrix(0)))
})

test_that("'vec_cast' works vec_int", {
    x <- rvec_int(matrix(3))
    expect_identical(vec_cast(x, to = rvec_dbl()), rvec_dbl(matrix(3)))
    expect_identical(vec_cast(x, to = rvec_int()), x)
    expect_identical(vec_cast(3, to = rvec_int()), x)
    expect_identical(vec_cast(3L, to = rvec_int()), x)
    expect_identical(vec_cast(FALSE, to = rvec_int()), rvec_int(matrix(0)))
})

test_that("'vec_cast' works vec_lgl", {
    x <- rvec_lgl(matrix(FALSE))
    expect_identical(vec_cast(x, to = rvec_dbl()), rvec_dbl(matrix(0)))
    expect_identical(vec_cast(x, to = rvec_int()), rvec_int(matrix(0)))
    expect_identical(vec_cast(1, to = rvec_lgl()), rvec_lgl(matrix(TRUE)))
    expect_identical(vec_cast(1L, to = rvec_lgl()), rvec_lgl(matrix(TRUE)))
    expect_identical(vec_cast(FALSE, to = rvec_lgl()), x)
})


## 'vec_math' -----------------------------------------------------------------

test_that("'sum' works with non-empty double", {
    x <- rvec_dbl(rbind(1:4, 5:8))
    ans_obtained <- sum(x)
    ans_expected <- rvec_dbl(matrix(c(6, 8, 10, 12), nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'sum' works with non-empty integer", {
    x <- rvec_int(rbind(1:4, 5:8))
    ans_obtained <- sum(x)
    ans_expected <- rvec_int(matrix(c(6L, 8L, 10L, 12L), nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'sum' works with non-empty logical", {
    x <- rvec_lgl(rbind(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, TRUE)))
    ans_obtained <- sum(x)
    ans_expected <- rvec_int(matrix(c(1, 1, 2), nr = 1L))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'sum' works with empty vector", {
    x <- rvec_int()
    ans_obtained <- sum(x)
    ans_expected <- rvec_int(matrix(0L, nr = 1L))
    expect_identical(ans_obtained, ans_expected)
})




