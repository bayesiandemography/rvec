
## 'check_and_tidy_width' -----------------------------------------------------

test_that("'check_and_tidy_width' works with valid inputs", {
    expect_identical(check_and_tidy_width(0.95), 0.95)
    expect_identical(check_and_tidy_width(c(0.5, 0.95)), c(0.95, 0.5))
    expect_identical(check_and_tidy_width(c(0.5, 0.5, 0.95)), c(0.95, 0.5))
})

test_that("'check_and_tidy_width' throws expected error with non-numeric width", {
    expect_error(check_and_tidy_width("a"),
                 "`width` has class character")
})

test_that("'check_and_tidy_width' throws expected error with 0-length width", {
    expect_error(check_and_tidy_width(numeric()),
                 "`width` has length 0")
})

test_that("'check_and_tidy_width' throws expected error with NAs", {
    expect_error(check_and_tidy_width(c(0.5, NA)),
                 "`width` has NAs")
})

test_that("'check_and_tidy_width' throws expected error with NAs", {
    expect_error(check_and_tidy_width(c(0.5, -1)),
                 "`width` has negative values")
})

test_that("'check_and_tidy_width' throws expected error with NAs", {
    expect_error(check_and_tidy_width(c(0.5, 1.00001)),
                 "`width` has values greater than 1")
})


## 'check_idx_dup' ---------------------------------------------------

test_that("'check_idx_dup' returns TRUE with valid inputs", {
    idx <- rbind(c(1L, 1L),
                 c(1L, 2L),
                 c(1L, 3L),
                 c(2L, 1L))
    data <- data.frame(id1 = "a",
                       id2 = c(1, 1, 1, 2),
                       dr = c(1L, 2L, 3L, 1L),
                       val = 4:1)
    expect_true(check_idx_dup(idx = idx,
                              data = data,
                              colnum_draw = c(dr = 3L),
                              colnums_id = c(id1 = 1L, id2 = 2L)))
})

test_that("'check_idx_dup' throws expected error", {
    idx <- rbind(c(1L, 1L),
                 c(1L, 1L),
                 c(1L, 2L),
                 c(3L, 1L))
    data <- data.frame(id1 = "a",
                       id2 = c(1, 1, 1, 2),
                       dr = c(1L, 1L, 2L, 1L),
                       val = 4:1)
    expect_error(check_idx_dup(idx = idx,
                               data = data,
                               colnum_draw = c(dr = 3L),
                               colnums_id = c(id1 = 1L, id2 = 2L)),
                 "Multiple rows with the same values for ID variables:")
})


## 'check_idx_gap' ---------------------------------------------------

test_that("'check_idx_gap' returns TRUE with valid inputs", {
    idx <- rbind(c(1L, 1L),
                 c(1L, 2L),
                 c(1L, 3L))
    data <- data.frame(id1 = "a",
                       id2 = c(1, 1, 1),
                       dr = c(1L, 2L, 3),
                       val = 4:2)
    expect_true(check_idx_gap(idx = idx,
                              idvars_ans = data[1, 1:2],
                              draw_ans = 1:3,
                              nm_draw = "DRAW"))
})

test_that("'check_idx_gap' throws expected error", {
    idx <- rbind(c(1L, 1L),
                 c(1L, 3L))
    data <- data.frame(id1 = "a",
                       id2 = c(1, 1, 1),
                       dr = c(1L, 2L, 3),
                       val = 4:2)
    expect_error(check_idx_gap(idx = idx,
                              idvars_ans = data[1, 1:2],
                              draw_ans = 1:3,
                              nm_draw = "DRAW"),
                 "Missing combination of values for ID and draw columns:")
})


## 'check_length_n_draw_compatible' -------------------------------------------

test_that("'check_length_n_draw_compatible' works with valid inputs", {
    expect_true(check_length_n_draw_compatible(x = 1:2,
                                               y = rvec(matrix(1:6, 3)),
                                               x_arg = "x",
                                               y_arg = "to"))
    expect_true(check_length_n_draw_compatible(x = 1,
                                               y = rvec(matrix(1:6, 3)),
                                               x_arg = "x",
                                               y_arg = "to"))
})

test_that("'check_length_n_draw_compatible' throws expected error with non-compatible length and n_draw", {
    expect_error(check_length_n_draw_compatible(x = 1:2,
                                                y = rvec(matrix(1:3, 1)),
                                                x_arg = "x",
                                                y_arg = "to"),
                 "Length of vector `x` must equal number of draws of rvec `to`.")
})


## 'check_n_draw_equal' -------------------------------------------------------

test_that("'check_n_draw_equal' works with valid inputs", {
    expect_true(check_n_draw_equal(x = rvec(matrix(1:4, 2)),
                                   y = rvec(matrix(1:6, 3)),
                                   x_arg = "x",
                                   y_arg = "to"))
})

test_that("'check_n_draw_equal' throws expected error with non-compatible length and n_draw", {
    expect_error(check_n_draw_equal(x = rvec(matrix(1:2, 1)),
                                    y = rvec(matrix(1:3, 1)),
                                    x_arg = "x",
                                    y_arg = "to"),
                 "Number of draws of rvec `x` must equal number of draws of rvec `to`.")
})


## 'check_na_rm' --------------------------------------------------------------

test_that("'check_na_rm' returns TRUE with valid inputs", {
    expect_true(check_na_rm(TRUE))
    expect_true(check_na_rm(FALSE))
})

test_that("'check_n_draw_equal' throws expected error non-length-1", {
    expect_error(check_na_rm(logical()),
                 "`na_rm` does not have length 1")
    expect_error(check_na_rm(c(TRUE, TRUE)),
                 "`na_rm` does not have length 1")
})

test_that("'check_n_draw_equal' throws expected error non-logical", {
    expect_error(check_na_rm("T"),
                 "`na_rm` has class <character>")
})

test_that("'check_n_draw_equal' throws expected error NA", {
    expect_error(check_na_rm(NA),
                 "`na_rm` is NA")
})


## 'check_types' --------------------------------------------------------------

test_that("'check_types' returns TRUE with valid inputs", {
    expect_true(check_types("cdil?"))
    expect_true(check_types("c"))
    expect_true(check_types(""))
})

test_that("'check_types' throws expected error with non-character", {
    expect_error(check_types(NULL),
                 "`types` must have class <character>.")
})

test_that("'check_types' throws expected error with wrong length", {
    expect_error(check_types(c("c", "d")),
                 "`types` must be a single string.")
})

test_that("'check_types' throws expected error with invalid character", {
    expect_error(check_types("cwd"),
                 "\"w\" is not a valid code for `types`.")
})


## 'check_x_has_at_least_one_col' ---------------------------------------------

test_that("'check_x_has_at_least_one_col' works with valid inputs", {
    expect_true(check_x_has_at_least_one_col(matrix(1:6, 3)))
    expect_true(check_x_has_at_least_one_col(matrix(1, 1)))
})

test_that("'check_x_has_at_least_one_col' throws expected error with non-numeric width", {
    expect_error(check_x_has_at_least_one_col(matrix(nrow = 3, ncol = 0)),
                 "`x` must have at least one column")
})


## 'check_x_is_matrix' --------------------------------------------------------

test_that("'check_x_is_matrix' works with valid inputs", {
    expect_true(check_x_is_matrix(matrix()))
    expect_true(check_x_is_matrix(matrix(1:6, 3)))
})

test_that("'check_x_is_matrix' throws expected error with non-numeric width", {
    expect_error(check_x_is_matrix(numeric()),
                 "`x` has class <numeric>")
})


## 'check_x_length_at_least_one' ----------------------------------------------

test_that("'check_x_length_at_least_one' works with valid inputs", {
    expect_true(check_x_length_at_least_one(0.95))
    expect_true(check_x_length_at_least_one(1:5))
})

test_that("'check_x_length_at_least_one' throws expected error with non-numeric width", {
    expect_error(check_x_length_at_least_one(numeric()),
                 "`x` has length 0")
})
