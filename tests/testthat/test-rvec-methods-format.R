

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


## Helpers --------------------------------------------------------------------

test_that("format_elements_rvec works with character", {
    expect_identical(format_elements_rvec(matrix(c("a", NA), nr = 1)),
                     matrix(c("a", NA), nr = 1))
})

test_that("format_elements_rvec works with double", {
    expect_identical(format_elements_rvec(matrix(c(1.2345, -3, 1000000, NA), nr = 1)),
                     matrix(c("1.234", "-3", "1000000", "NA"), nr = 1))
})

test_that("format_elements_rvec works with integer", {
    expect_identical(format_elements_rvec(matrix(c(1L, -3L, 1000000L, NA), nr = 1)),
                     matrix(c("1", "-3", "1000000", "NA"), nr = 1))
})

test_that("format_elements_rvec works with logical", {
    expect_identical(format_elements_rvec(matrix(c(TRUE, FALSE, NA), nr = 1)),
                     matrix(c("T", "F", NA), nr = 1))
})                     
