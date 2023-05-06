
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


## 'matrix_to_list_of' --------------------------------------------------------

test_that("'matrix_to_list_of' works with nrow > 0, ncol > 0", {
    m <- matrix(1:12, nr = 4, nc = 3, byrow = TRUE)
    ans_obtained <- matrix_to_list_of(m)
    ans_expected <- list_of(1:3, 4:6, 7:9, 10:12)
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- matrix_to_list_of(m, .ptype = double())
    ans_expected <- list_of(1:3, 4:6, 7:9, 10:12, .ptype = double())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of' works with nrow = 0, ncol > 0", {
    m <- matrix(FALSE, nr = 0, nc = 3)
    ans_obtained <- matrix_to_list_of(m)
    ans_expected <- list_of(.ptype = logical(3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of' works with nrow > 0, ncol = 0", {
    m <- matrix("a", nr = 3, nc = 0)
    ans_obtained <- matrix_to_list_of(m)
    ans_expected <- list_of(character(), character(), character(),
                            .ptype = character(0))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of' works with nrow = 0, ncol = 0", {
    m <- matrix(1, nr = 0, nc = 0)
    ans_obtained <- matrix_to_list_of(m)
    ans_expected <- list_of(.ptype = double(0))
    expect_identical(ans_obtained, ans_expected)
})





    

