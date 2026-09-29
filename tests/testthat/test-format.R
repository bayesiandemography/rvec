

## 'format' -------------------------------------------------------------------

test_that("'format' method for rvec works - one column", {
    x <- rvec(matrix("a", nr = 1))
    ans_obtained <- format(x)
    ans_expected <- '"a"'
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

test_that("'format' method for rvec works - four columns, chr", {
    m <- matrix(c("b", "b", "c", "c", "d", "d", "b", "c"),
                nr = 2, nc = 4)
    x <- rvec(m)                
    ans_obtained <- format(x)
    ans_expected <- c('.."b"..', '.."c"..')
    expect_identical(ans_obtained, ans_expected)
})

test_that("'format' method for rvec works - four columns, dbl", {
    m <- matrix(rnorm(8), nr = 2, nc = 4)
    x <- rvec(m)
    ans_obtained <- format(x)
    ans_expected <- draws_ci(x)
    ans_expected <- paste0(prettyNum(ans_expected[[2L]], digits = 2),
                           " (",
                           prettyNum(ans_expected[[1L]], digits = 2),
                           ", ",
                           prettyNum(ans_expected[[3L]], digits = 2),
                           ")")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'format' method for rvec works - four columns, int", {
    m <- matrix(11:18, nr = 2, nc = 4)
    x <- rvec(m)
    ans_obtained <- format(x)
    ans_expected <- draws_ci(x)
    ans_expected <- paste0(prettyNum(ans_expected[[2L]], digits = 2),
                           " (",
                           prettyNum(ans_expected[[1L]], digits = 2),
                           ", ",
                           prettyNum(ans_expected[[3L]], digits = 2),
                           ")")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'format' method for rvec works - four columns, lgl", {
    x <- rvec(matrix(c(T,F,F,T), nr = 2, nc = 4))
    ans_obtained <- format(x)
    ans_expected <- c("p=0.5", "p=0.5")
    expect_identical(ans_obtained, ans_expected)
})


## Helpers --------------------------------------------------------------------

test_that("format_rvec_elements works with character", {
    expect_identical(format_rvec_elements(matrix(c("a", NA), nr = 1)),
                     matrix(c('"a"', NA), nr = 1))
})

test_that("format_rvec_elements works with double", {
    expect_identical(format_rvec_elements(matrix(c(1.2345, -3, 1000000, NA), nr = 1)),
                     matrix(c("1.234", "-3", "1000000", "NA"), nr = 1))
})

test_that("format_rvec_elements works with integer", {
    expect_identical(format_rvec_elements(matrix(c(1L, -3L, 1000000L, NA), nr = 1)),
                     matrix(c("1", "-3", "1000000", "NA"), nr = 1))
})

test_that("format_rvec_elements works with logical", {
    expect_identical(format_rvec_elements(matrix(c(TRUE, FALSE, NA), nr = 1)),
                     matrix(c("T", "F", NA), nr = 1))
})                     

test_that("obj_print_data.rvec works with length 0", {
    x <- rvec_dbl()
    ans_obtained <- vctrs::obj_print_data(x)
    ans_expected <- x
    expect_identical(ans_obtained, ans_expected)
})

test_that("obj_print_data.rvec works with length > 0", {
    x <- rvec_dbl(matrix(1:4, 2))
    expect_snapshot(vctrs::obj_print_data(x))
})

test_that("logical summary formatting preserves missingness, rounding, and names", {
    m <- rbind(all_true = rep(TRUE, 7L), all_false = rep(FALSE, 7L),
               all_missing = rep(NA, 7L), partial = c(TRUE, FALSE, NA, NA, NA, NA, NA),
               fraction = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
    x <- rvec_lgl(m)
    expect_identical(format(x),
                     c(all_true = "p=1", all_false = "p=0", all_missing = "p=NaN",
                       partial = "p=0.5", fraction = "p=0.1429"))
    expect_identical(vctrs::field(x, "data"), m)
    empty <- rvec_lgl(matrix(logical(), nrow = 0L, ncol = 7L))
    expect_identical(format(empty), "p=")
})

test_that("logical formatting retains double-precision refinement at rounding boundaries", {
    m <- matrix(c(rep(TRUE, 14285L), rep(FALSE, 85715L)), nrow = 1L)
    expected <- paste0("p=", formatC(matrixStats::rowMeans2(1 * m, na.rm = TRUE),
                                    format = "fg"))
    expect_identical(format(rvec_lgl(m)), expected)
})
