
## 'append_col' ---------------------------------------------------------------

test_that("'append_col' works with valid inputs", {
    df <- data.frame(a = 1, b = 2, c = 3, d = 4)
    value <- "1"
    nm <- "val"
    ans_obtained <- append_col(df, value = value, after = 0L, nm = nm)
    ans_expected <- data.frame(val = "1", a = 1, b = 2, c = 3, d = 4)
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- append_col(df, value = value, after = 1L, nm = nm)
    ans_expected <- data.frame(a = 1, val = "1", b = 2, c = 3, d = 4)
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- append_col(df, value = value, after = 4L, nm = nm)
    ans_expected <- data.frame(a = 1, b = 2, c = 3, d = 4, val = "1")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'append_col' throws correct error with zero-length df", {
    expect_error(append_col(data.frame(),
                            value = 1,
                            after = 0,
                            nm = "v"),
                 "Internal error: `df` has length 0")
})

test_that("'append_col' throws correct error with zero-length df", {
    expect_error(append_col(data.frame(a = 0),
                            value = 1,
                            after = 2,
                            nm = "v"),
                 "Internal error: `after` invalid")
})

test_that("'append_col' throws correct error with zero-length df", {
    expect_error(append_col(data.frame(a = 0),
                            value = 1,
                            after = 1,
                            nm = "a"),
                 "Internal error: `df` already has column called \"a\"")
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
    colnames(m) <- c("a", "b", "c")
    ans_obtained <- matrix_to_list_of_cols(m)
    ans_expected <- list(a = 1:4, b = 5:8, c = 9:12)
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
    rownames(m) <- c("a", "b", "c", "d")
    ans_obtained <- matrix_to_list_of_rows(m)
    ans_expected <- list(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
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


## 'n_draw_df' ----------------------------------------------------------------

test_that("'n_draw_df' works with valid inputs", {
    df <- data.frame(a = 1:2)
    df$x <- rvec(matrix(1:6, nr = 2))
    df$y <- rvec(matrix(6:1, nr = 2))
    expect_identical(n_draw_df(df), 3L)
})

test_that("'n_draw_df' throws expected error when no rvecs present", {
    expect_error(n_draw_df(data.frame(a = 1)),
                 "Internal error: `df` does not contain any rvecs.")
})

test_that("'n_draw_df' throws expected error when n_draw varies", {
    df <- data.frame(a = 1:2)
    df$x <- rvec(matrix(1:6, nr = 2))
    df$y <- rvec(matrix(10:1, nr = 2))
    expect_error(n_draw_df(df),
                 "Internal error: Value for `n_draw` varies across rvecs.")
})


## 'set_cols_to_blank' --------------------------------------------------------

test_that("'set_cols_to_blank' works with valid inputs", {
    df <- data.frame(a = 1:2, b = 3:4, c = 5:6)
    ans_obtained <- set_cols_to_blank(df, colnums = c(1, 3))
    ans_expected <- data.frame(b = 3:4)
    ans_expected$a <- list(NULL, NULL)
    ans_expected$c <- list(NULL, NULL)
    ans_expected <- ans_expected[c(2, 1, 3)]
    expect_identical(ans_obtained, ans_expected)
})
