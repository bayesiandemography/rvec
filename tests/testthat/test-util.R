
## 'format_elements_rvec' -----------------------------------------------------

test_that("format_elements_rvec works with character", {
    expect_identical(format_elements_rvec(matrix(c("a", NA))),
                     matrix(c("a", "NA")))
})

test_that("format_elements_rvec works with double", {
    expect_identical(format_elements_rvec(matrix(c(1.2345, -3, 1000000, NA))),
                     matrix(c("1.234", "-3", "1000000", "NA")))
})

test_that("format_elements_rvec works with integer", {
    expect_identical(format_elements_rvec(matrix(c(1L, -3L, 1000000L, NA))),
                     matrix(c("1", "-3", "1000000", "NA")))
})

test_that("format_elements_rvec works with logical", {
    expect_identical(format_elements_rvec(matrix(c(TRUE, FALSE, NA))),
                     matrix(c("T", "F", "NA")))
})                     
                     

## 'get_colnums_all' ----------------------------------------------------------

test_that("'get_colnums_all' works with valid inputs", {
    data <- data.frame(a = -1, b = 99, c = "x")
    ans_obtained <- get_colnums_all(data)
    ans_expected <- c(a = 1L, b = 2L, c = 3L)
    expect_identical(ans_obtained, ans_expected)
})


## 'get_colnums_groups' -------------------------------------------------------

test_that("'get_colnums_groups' works with valid inputs", {
    data <- data.frame(a = -1, b = 99, c = "x")
    data <- dplyr::group_by(data, c, a)
    ans_obtained <- get_colnums_groups(data)
    ans_expected <- c(c = 3L, a = 1L)
    expect_identical(ans_obtained, ans_expected)
})


## 'get_rvec_fun' -------------------------------------------------------------

test_that("'get_rvec_fun' works with valid inputs", {
    expect_identical(get_rvec_fun("c"), new_rvec_chr)
    expect_identical(get_rvec_fun("?"), new_rvec)
})

test_that("'get_rvec_fun' thows correct error invalid inputs", {
    expect_error(get_rvec_fun("w"),
                 "Internal error: \"w\" is not a valid code.")
})


## 'get_rvec_funs' ------------------------------------------------------------

test_that("'get_rvec_funs' works with valid inputs", {
    expect_identical(get_rvec_funs("c?ldi?"),
                     list(new_rvec_chr,
                          new_rvec,
                          new_rvec_lgl,
                          new_rvec_dbl,
                          new_rvec_int,
                          new_rvec))
})


## 'is_rvec' ------------------------------------------------------------------

test_that("'is_rvec' works", {
    expect_true(is_rvec(rvec(matrix(1))))
    expect_false(is_rvec(NULL))
})


## 'make_probs' ---------------------------------------------------------------

test_that("'make_probs' works", {
    expect_equal(make_probs(0.5), c(0.25, 0.75))
    expect_equal(make_probs(c(1, 0.9, 0.8)), c(0, 0.05, 0.1, 0.9, 0.95, 1))
})    


## 'matrix_to_list_of_cols' ---------------------------------------------------

test_that("'matrix_to_list_of_cols' works with nrow > 0, ncol > 0", {
    m <- matrix(1:12, nr = 4, nc = 3)
    ans_obtained <- matrix_to_list_of_cols(m)
    ans_expected <- list(1:4, 5:8, 9:12)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_cols' works with nrow = 0, ncol > 0", {
    m <- matrix(NA, nr = 0, nc = 3)
    ans_obtained <- matrix_to_list_of_cols(m)
    ans_expected <- list(logical(), logical(), logical())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_cols' works with nrow > 0, ncol = 0", {
    m <- matrix(NA, nr = 3, nc = 0)
    ans_obtained <- matrix_to_list_of_cols(m)
    ans_expected <- list()
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_cols' works with nrow = 0, ncol = 0", {
    m <- matrix(1, nr = 0, nc = 0)
    ans_obtained <- matrix_to_list_of_cols(m)
    ans_expected <- list()
    expect_identical(ans_obtained, ans_expected)
})


## 'matrix_to_list_of_rows' ---------------------------------------------------

test_that("'matrix_to_list_of_rows' works with nrow > 0, ncol > 0", {
    m <- matrix(1:12, nr = 4, nc = 3, byrow = TRUE)
    ans_obtained <- matrix_to_list_of_rows(m)
    ans_expected <- list(1:3, 4:6, 7:9, 10:12)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_rows' works with nrow = 0, ncol > 0", {
    m <- matrix(NA, nr = 0, nc = 3)
    ans_obtained <- matrix_to_list_of_rows(m)
    ans_expected <- list()
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_rows' works with nrow > 0, ncol = 0", {
    m <- matrix(NA, nr = 3, nc = 0)
    ans_obtained <- matrix_to_list_of_rows(m)
    ans_expected <- list(logical(), logical(), logical())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_rows' works with nrow = 0, ncol = 0", {
    m <- matrix(1, nr = 0, nc = 0)
    ans_obtained <- matrix_to_list_of_rows(m)
    ans_expected <- list()
    expect_identical(ans_obtained, ans_expected)
})


## 'get_new_rvec_fun' ---------------------------------------------------------

test_that("'get_new_rvec_fun' works with valid input", {
    expect_identical(get_new_rvec_fun("a"), new_rvec_chr)
    expect_identical(get_new_rvec_fun(1L), new_rvec_int)
    expect_identical(get_new_rvec_fun(1), new_rvec_dbl)
    expect_identical(get_new_rvec_fun(NA), new_rvec_lgl)
})

test_that("'get_new_rvec_fun' throws correct error invalid input", {
    expect_error(get_new_rvec_fun(NULL),
                 "Internal error: `x` is NULL")
})
