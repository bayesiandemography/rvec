
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


## 'as_ptype_rvec_chr' --------------------------------------------------------

test_that("'as_ptype_rvec_chr' works with valid inputs", {
    x <- rvec(matrix(1:6, nr = 2))
    ans_obtained <- as_ptype_rvec_chr(x)
    ans_expected <- new_rvec(matrix(character(), nrow = 0, ncol = 3))
    expect_identical(ans_obtained, ans_expected)
})


## 'as_ptype_rvec_dbl' --------------------------------------------------------

test_that("'as_ptype_rvec_dbl' works with valid inputs", {
    x <- rvec(matrix(1:6, nr = 2))
    ans_obtained <- as_ptype_rvec_dbl(x)
    ans_expected <- new_rvec(matrix(double(), nrow = 0, ncol = 3))
    expect_identical(ans_obtained, ans_expected)
})


## 'as_ptype_rvec_int' --------------------------------------------------------

test_that("'as_ptype_rvec_int' works with valid inputs", {
    x <- rvec(matrix(1:6, nr = 2))
    ans_obtained <- as_ptype_rvec_int(x)
    ans_expected <- new_rvec(matrix(integer(), nrow = 0, ncol = 3))
    expect_identical(ans_obtained, ans_expected)
})


## 'get_colnum_draw' ----------------------------------------------------------

test_that("'get_colnum_draw' works with valid inputs", {
    data <- data.frame(a = -1, b = 99, c = "x")
    ans_obtained <- get_colnum_draw(draw = "b", data = data)
    ans_expected <- c(b = 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_colnum_draw' throws correct error with invalid inputs", {
    data <- data.frame(a = -1, b = 99, c = "x")
    expect_error(get_colnum_draw(draw = "wrong", data = data),
                 "Variable specified by `draw` not found in `data`.")
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


## 'get_colnums_rvec' ---------------------------------------------------------

test_that("'get_colnums_rvec' works with valid inputs", {
    data <- data.frame(a = 1, b = rvec(matrix(1)), c = rvec(matrix("x")))
    ans_obtained <- get_colnums_rvec(data)
    ans_expected <- c(b = 2L, c = 3L)
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
    expect_identical(get_rvec_fun("c"), rvec_chr)
    expect_identical(get_rvec_fun("?"), rvec)
})

test_that("'get_rvec_fun' thows correct error invalid inputs", {
    expect_error(get_rvec_fun("w"),
                 "Internal error: \"w\" is not a valid code.")
})


## 'get_rvec_funs' ------------------------------------------------------------

test_that("'get_rvec_funs' works with valid inputs - type non-NULL", {
    expect_identical(get_rvec_funs("c?ldi?",
                                   colnums_values = c(a = 1L, b = 2L, c = 3L,
                                                      d = 4L, e = 5L, f = 6L)),
                     list(rvec_chr,
                          rvec,
                          rvec_lgl,
                          rvec_dbl,
                          rvec_int,
                          rvec))
})


test_that("'get_rvec_funs' works with valid inputs - type NULL", {
    expect_identical(get_rvec_funs(NULL,
                                   colnums_values = c(a = 1L, b = 2L)),
                     list(rvec,
                          rvec))
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


## 'n_rdist' ------------------------------------------------------------------

test_that("'n_rdist' works with an rvec arg", {
    ans_obtained <- n_rdist(n = 2L, args = list(2, rvec(matrix(1:4, 2))))
    ans_expected <- 4L
    expect_identical(ans_obtained, ans_expected)
})

test_that("'n_rdist' works with no rvec arg", {
    ans_obtained <- n_rdist(n = 2L, args = list(2, 2))
    ans_expected <- 2L
    expect_identical(ans_obtained, ans_expected)
})


## 'paste_dot' ----------------------------------------------------------------

test_that("'paste_dot' works with valid inputs", {
    df <- data.frame(a = 1:2, b = 3:4, c = 5:6)
    ans_obtained <- paste_dot(df)
    ans_expected <- c("1.3.5", "2.4.6")
    expect_identical(ans_obtained, ans_expected)
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
