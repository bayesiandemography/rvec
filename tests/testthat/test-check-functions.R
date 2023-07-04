
## 'check_colnum_draw' --------------------------------------------------------

test_that("'check_colnum_draw' returns TRUE with valid inputs", {
    expect_true(check_colnum_draw(colnum_draw = c(draw = 3L)))
})

test_that("'check_colnum_draw' throws expected error when length 0", {
    expect_error(check_colnum_draw(colnum_draw = integer()),
                 "No `draw` variable selected")
})


test_that("'check_colnum_draw' throws expected error when length 2", {
    expect_error(check_colnum_draw(colnum_draw = c(draw = 3, sim = 4L)),
                 "More than one `draw` variable selected")
})


## 'check_colnums_values' -----------------------------------------------------

test_that("'check_colnums_values' returns TRUE with valid inputs", {
    expect_true(check_colnums_values(colnums_values = c(val = 3L)))
})

test_that("'check_colnum_draw' throws expected error when length 0", {
    expect_error(check_colnums_values(colnums_values = integer()),
                 "No `values` variables selected")
})


## 'check_flag' ---------------------------------------------------------------

test_that("'check_flag' returns TRUE with valid inputs", {
    x <- TRUE
    expect_true(check_flag(x))
    x <- FALSE
    expect_true(check_flag(x))
})

test_that("'check_flag' throws expected error non-length-1", {
    y <- logical()
    expect_error(check_flag(y),
                 "`y` does not have length 1")
    z <- c(TRUE, TRUE)
    expect_error(check_flag(z),
                 "`z` does not have length 1")
})

test_that("'check_flag' throws expected error non-logical", {
    x <- "hello"
    expect_error(check_flag(x),
                 "`x` has class <character>")
})

test_that("'check_flag' throws expected error NA", {
    x <- NA
    expect_error(check_flag(x),
                 "`x` is NA")
})


## 'check_idx_dup' ------------------------------------------------------------

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
                 "Missing combination of values for ID and draw variables:")
})


## 'check_has_no_rvecs' --------------------------------------------------------

test_that("'check_has_no_rvecs' returns TRUE with valid inputs", {
    df <- data.frame(a = 1, b = 2)
    expect_true(check_has_no_rvecs(df, nm_df = "data"))
})

test_that("'check_has_no_rvecs' raises correct error invalid inputs", {
    df <- data.frame(a = 1, b = rvec(matrix("a")))
    expect_error(check_has_no_rvecs(df, nm_df = "data"),
                 "`data` contains an rvec")
})


## 'check_length_n_draw_compatible' -------------------------------------------

test_that("'check_length_n_draw_compatible' returns TRUE with valid inputs", {
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


## 'check_lengths_equal' ------------------------------------------------------

test_that("'check_lengths_equal' returns TRUE with valid inputs", {
    expect_true(check_lengths_equal(list(1:2, 2:3)))
    expect_true(check_lengths_equal(list(1:2)))
    expect_true(check_lengths_equal(list()))
})

test_that("'check_lengths_equal' throws expected error unequal lengths", {
    expect_error(check_lengths_equal(list(1:3, 1:4)),
                 "Elements of `x` do not have equal lengths.")
})


## 'check_lengths_nonzero' ----------------------------------------------------

test_that("'check_lengths_nonzero' returns TRUE with valid inputs", {
    expect_true(check_lengths_nonzero(list(1:2, 2:3)))
    expect_true(check_lengths_nonzero(list()))
})

test_that("'check_lengths_nonzero' throws expected error unequal lengths", {
    expect_error(check_lengths_nonzero(list(1L, integer())),
                 "All elements of `x` must have non-zero length.")
})


## 'check_n' ------------------------------------------------------------------

test_that("'check_n' returns TRUE with valid inputs", {
    expect_true(check_n(15))
})

test_that("'check_n' throws expected error with NA", {
    expect_error(check_n(NA_real_),
                 "`n` is NA")
})

test_that("'check_n' throws expected error with non-integer", {
    expect_error(check_n(1.3),
                 "`n` has non-integer value \\(1.3\\).")
})

test_that("'check_n' throws expected error with 0", {
    expect_error(check_n(0),
                 "`n` equals 0.")
})


## 'check_n_draw' -------------------------------------------------------------

test_that("'check_n_draw' returns TRUE with valid inputs", {
    expect_true(check_n_draw(15))
})

test_that("'check_n_draw' throws expected error with NA", {
    expect_error(check_n_draw(NA_real_),
                 "`n_draw` is NA")
})

test_that("'check_n_draw' throws expected error with non-integer", {
    expect_error(check_n_draw(1.3),
                 "`n_draw` has non-integer value \\(1.3\\).")
})

test_that("'check_n_draw' throws expected error with 0", {
    expect_error(check_n_draw(0),
                 "`n_draw` equals 0.")
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


## 'check_nonneg_num_scalar' --------------------------------------------------

test_that("'check_nonneg_num_scalar' returns TRUE with valid inputs", {
    x <- 3
    expect_true(check_nonneg_num_scalar(x))
    x <- 0
    expect_true(check_nonneg_num_scalar(x))
    x <- 0L
    expect_true(check_nonneg_num_scalar(x))
})

test_that("'check_nonneg_num_scalar' throws expected error non-length-1", {
    y <- double()
    expect_error(check_nonneg_num_scalar(y),
                 "`y` does not have length 1")
    z <- 1:2
    expect_error(check_nonneg_num_scalar(z),
                 "`z` does not have length 1")
})

test_that("'check_nonneg_num_scalar' throws expected error if non-numeric", {
    x <- "hello"
    expect_error(check_nonneg_num_scalar(x),
                 "`x` has class <character>")
})

test_that("'check_nonneg_num_scalar' throws expected error if NA", {
    x <- NA_real_
    expect_error(check_nonneg_num_scalar(x),
                 "`x` is NA")
})

test_that("'check_nonneg_num_scalar' throws expected error if negative", {
    ncp <- -0.0000001
    expect_error(check_nonneg_num_scalar(ncp),
                 "`ncp` is negative")
})


## 'check_nonneg_num_vector' --------------------------------------------------

test_that("'check_nonneg_num_vector' returns TRUE with valid inputs", {
    x <- c(3, 2)
    expect_true(check_nonneg_num_vector(x))
    x <- 0
    expect_true(check_nonneg_num_vector(x))
    x <- 0:10
    expect_true(check_nonneg_num_vector(x))
})

test_that("'check_nonneg_num_vector' throws expected error if non-numeric", {
    x <- c("hello", "goodbye")
    expect_error(check_nonneg_num_vector(x),
                 "`x` has class <character>")
})

test_that("'check_nonneg_num_vector' throws expected error if has NAs", {
    x <- c(1, NA, 3)
    expect_error(check_nonneg_num_vector(x),
                 "`x` has 1 NA")
    x <- c(1, NA, 3, NA)
    expect_error(check_nonneg_num_vector(x),
                 "`x` has 2 NAs")
})

test_that("'check_nonneg_num_vector' throws expected error if negative", {
    ncp <- c(-0.0000001, 0, 1, 2)
    expect_error(check_nonneg_num_vector(ncp),
                 "`ncp` has 1 negative value")
    ncp <- c(-0.0000001, 0, 1, -2)
    expect_error(check_nonneg_num_vector(ncp),
                 "`ncp` has 2 negative values")
})


## 'check_not_rvec_chr' -------------------------------------------------------

test_that("'check_not_rvec_chr' returns TRUE with valid rvec", {
    expect_true(check_not_rvec_chr(arg = rvec(matrix(1:3, 1)),
                                   nm_arg = "x"))
})

test_that("'check_not_rvec_chr' throws correct error with rvec_chr", {
    expect_error(check_not_rvec_chr(arg = rvec(matrix("a")),
                                    nm_arg = "x"),
                 "`x` has class <")    
})


## 'check_overlap_draw_groups' ------------------------------------------------

test_that("'check_overlap_draw_groups' returns TRUE with valid inputs", {
    expect_true(check_overlap_draw_groups(colnum_draw = c(draw = 3L),
                                          colnums_groups = c(v1 = 1L, v2 = 5L)))
})

test_that("'check_overlap_draw_groups' throws expected error with overlap", {
    expect_error(check_overlap_draw_groups(colnum_draw = c(v2 = 3L),
                                           colnums_groups = c(v1 = 1L, v2 = 3L)),
                 "`v2` is a grouping variable, so cannot be used for `draw`.")
})


## 'check_overlap_draw_values' ------------------------------------------------

test_that("'check_overlap_draw_values' returns TRUE with valid inputs", {
    expect_true(check_overlap_draw_values(colnum_draw = c(draw = 3L),
                                          colnums_values = c(v1 = 1L, v2 = 5L)))
})

test_that("'check_overlap_draw_values' throws expected error with overlap", {
    expect_error(check_overlap_draw_values(colnum_draw = c(v2 = 3L),
                                           colnums_values = c(v1 = 1L, v2 = 3L)),
                 "`v2` used in `draw` and in `values`")
})


## 'check_overlap_values_groups' ------------------------------------------------

test_that("'check_overlap_values_groups' returns TRUE with valid inputs", {
    expect_true(check_overlap_values_groups(colnums_values = c(v1 = 1L, v2 = 5L),
                                            colnums_groups = c(v3 = 2L)))
})

test_that("'check_overlap_values_groups' throws expected error with overlap", {
    expect_error(check_overlap_values_groups(colnums_values = c(v1 = 1L, v2 = 3L),
                                             colnums_groups = c(v2 = 3L, v3 = 4L)),
                 "`v2` is a grouping variable, so cannot be included in `values`")
})


## 'check_probs' --------------------------------------------------------------

test_that("'check_probs' works with valid inputs", {
    expect_true(check_probs(0.95))
    expect_true(check_probs(c(0.5, 0.95)))
    expect_true(check_probs(c(0.5, 0.5, 0.95)))
})

test_that("'check_probs' throws expected error with non-numeric width", {
    expect_error(check_probs("a"),
                 "`probs` must be numeric")
})

test_that("'check_probs' throws expected error with 0 length", {
    expect_error(check_probs(numeric()),
                 "`probs` has length 0")
})

test_that("'check_probs' throws expected error with NAs", {
    expect_error(check_probs(c(0.5, NA)),
                 "`probs` has NAs")
})

test_that("'check_probs' throws expected error with negative values", {
    expect_error(check_probs(c(0.5, -1)),
                 "`probs` has negative values")
})

test_that("'check_probs' throws expected error with values greater than 1", {
    expect_error(check_probs(c(0.5, 1.00001)),
                 "`probs` has values greater than 1")
})


## 'check_same_length' --------------------------------------------------------

test_that("'check_same_length' returns TRUE with valid inputs", {
    expect_true(check_same_length(x = 1:3, y = 3:1, x_arg = "x", y_arg = "y"))
})

test_that("'check_same_length' returns correct error message with invalid inputs", {
    expect_error(check_same_length(x = 1:3, y = 3:0, x_arg = "x", y_arg = "y"),
                 "`x` and `y` have different lengths.")
})


## 'check_str' ----------------------------------------------------------------

test_that("'check_str' returns TRUE with valid inputs", {
    expect_true(check_str("draw", x_arg = "x"))
    expect_true(check_str("sim", x_arg = "x"))
})

test_that("'check_str' throws expected error non-length-1", {
    expect_error(check_str(character(), x_arg = "x"),
                 "`x` does not have length 1")
    expect_error(check_str(c("a", "b"), x_arg = "draw"),
                 "`draw` does not have length 1")
})

test_that("'check_str' throws expected error non-character", {
    expect_error(check_str(TRUE, x_arg = "draw"),
                 "`draw` not a string")
})

test_that("'check_str' throws expected error NA", {
    expect_error(check_str(NA_character_, x_arg = "draw"),
                 "`draw` is NA")
})

test_that("'check_str' throws expected error blank", {
    expect_error(check_str("", x_arg = "draw"),
                 "`draw` is blank")
})


## 'check_type' --------------------------------------------------------------

test_that("'check_type' returns TRUE with valid inputs", {
    expect_true(check_type("cdil?"))
    expect_true(check_type("c"))
    expect_true(check_type(""))
    expect_true(check_type(NULL))
})

test_that("'check_type' throws expected error with non-character", {
    expect_error(check_type(1L),
                 "`type` must have class <character>.")
})

test_that("'check_type' throws expected error with wrong length", {
    expect_error(check_type(c("c", "d")),
                 "`type` must be a single string.")
})

test_that("'check_type' throws expected error with invalid character", {
    expect_error(check_type("cwd"),
                 "\"w\" is not a valid code for `type`.")
})


## 'check_values_type_consistent' ---------------------------------------------

test_that("'check_values_type_consistent' returns TRUE with valid inputs", {
    expect_true(check_values_type_consistent(colnums_values = c(v = 1L),
                                             type = "d"))
    expect_true(check_values_type_consistent(colnums_values = c(v = 1L, x = 2L),
                                             type = "di"))
    expect_true(check_values_type_consistent(colnums_values = c(v = 1L, x = 2L),
                                             type = NULL))
})

test_that("'check_values_type_consistent' throws expected error with diff lengths", {
    expect_error(check_values_type_consistent(colnums_values = c(a = 1L, b = 2L),
                                              type = "dd?"),
                 "Number of characters in `type` must equal number of values variables")
    expect_error(check_values_type_consistent(colnums_values = c(a = 1L, b = 2L),
                                              type = "i"),
                 "Number of characters in `type` must equal number of values variables")
})


## 'check_width' ----------------------------------------------------------------

test_that("'check_width' returns TRUE with valid inputs", {
    expect_true(check_width(0.96))
    expect_true(check_width(0.2))
    expect_true(check_width(1))
})

test_that("'check_width' throws expected error non-length-1", {
    expect_error(check_width(c(0.3, 0.4)),
                 "`width` does not have length 1.")
    expect_error(check_width(numeric()),
                 "`width` does not have length 1.")
})

test_that("'check_width' throws expected error non-numeric", {
    expect_error(check_width(TRUE),
                 "`width` has class <logical>.")
})

test_that("'check_width' throws expected error NA", {
    expect_error(check_width(NA_real_),
                 "`width` is NA.")
})

test_that("'check_width' throws expected error too low", {
    expect_error(check_width(0),
                 "`width` not in interval \\(0, 1\\].")
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
