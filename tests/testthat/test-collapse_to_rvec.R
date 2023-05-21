
## 'collapse_to_rvec' ---------------------------------------------------------

test_that("'collapse_to_rvec' works with ordinary data frame - all defaults", {
    df <- data.frame(g = c("a", "a", "b", "b"),
                     draw = c(1:2, 1:2),
                     value = 1:4)
    ans_obtained <- collapse_to_rvec(df)
    ans_expected <- data.frame(g = c("a", "b"),
                               value = rvec_int(rbind(1:2, 3:4)))
    expect_identical(ans_obtained, ans_expected)
})


test_that("'collapse_to_rvec' works with ordinary data frame - no default", {
    df <- data.frame(g = c("a", "a", "b", "b"),
                     sim = c(1:2, 1:2),
                     value = 1:4)
    ans_obtained <- collapse_to_rvec(data = df,
                                     draw = "sim",
                                     type = "d")
    ans_expected <- data.frame(g = c("a", "b"),
                               value = rvec_dbl(rbind(1:2, 3:4)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'collapse_to_rvec' works with grouped data frame", {
    df <- data.frame(g = c("a", "a", "b", "b"),
                     draw = c(1:2, 1:2),
                     h = 1:4,
                     value = 1:4)
    df <- dplyr::group_by(df, g)
    ans_obtained <- collapse_to_rvec(df)
    ans_expected <- data.frame(g = c("a", "b"),
                               value = rvec_int(rbind(1:2, 3:4)))
    ans_expected <- dplyr::group_by(ans_expected, g)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'collapse_to_rvec' works with type", {
    df <- data.frame(g = c("a", "a", "b", "b"),
                     draw = c(1:2, 1:2),
                     h = 1:4,
                     value = 1:4)
    df <- dplyr::group_by(df, g)
    ans_obtained <- collapse_to_rvec(df, type = "c")
    ans_expected <- data.frame(g = c("a", "b"),
                               value = rvec_chr(rbind(1:2, 3:4)))
    ans_expected <- dplyr::group_by(ans_expected, g)
    expect_identical(ans_obtained, ans_expected)
})



## 'collapse_to_rvec_inner' --------------------------------------------------

test_that("'collapse_to_rvec_inner' works with ordinary data frame", {
    df <- data.frame(g = c("a", "a", "b", "b"),
                     draw = c(1:2, 1:2),
                     value = 1:4)
    ans_obtained <- collapse_to_rvec_inner(data = df,
                                           draw = "draw",
                                           colnums_values = c(value = 3L),
                                           colnums_groups = integer(),
                                           type = NULL)
    ans_expected <- data.frame(g = c("a", "b"),
                               value = rvec_int(rbind(1:2, 3:4)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'collapse_to_rvec_inner' works with grouped data frame", {
    df <- data.frame(g = c("a", "a", "b", "b"),
                     h = 4:1,
                     sim = c(1:2, 1:2),
                     value = 1:4)
    df <- dplyr::group_by(df, g)
    ans_obtained <- collapse_to_rvec_inner(data = df,
                                           draw = "sim",
                                           colnums_values = c(value = 4L),
                                           colnums_groups = c(g = 1L),
                                           type = NULL)
    ans_expected <- data.frame(g = c("a", "b"),
                               value = rvec_int(rbind(1:2, 3:4)))
    ans_expected <- dplyr::group_by(ans_expected, g)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'collapse_to_rvec_inner' works with two ID columns and two data columns", {
    df <- data.frame(g = c("a", "a", "b", "b", "a", "a", "b", "b"),
                     value1 = 1:8,
                     h = c("y", "y", "y", "y", "z", "z", "z", "z"),
                     draw = c(1:2, 1:2, 1:2, 1:2),
                     value2 = 8:1)
    ans_obtained <- collapse_to_rvec_inner(data = df,
                                           draw = "draw",
                                           colnums_values = c(value1 = 2L, value2 = 5L),
                                           colnums_groups = integer(),
                                           type = NULL)
    ans_expected <- data.frame(g = c("a", "b", "a", "b"),
                               value1 = rvec_int(rbind(1:2, 3:4, 5:6, 7:8)),
                               h = c("y", "y", "z", "z"),
                               value2 = rvec_int(rbind(8:7, 6:5, 4:3, 2:1)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'collapse_to_rvec_inner' works with nrow = 0", {
    df <- data.frame(g = character(),
                     value1 = double(),
                     h = character(),
                     draw = integer(),
                     value2 = integer())
    ans_obtained <- collapse_to_rvec_inner(data = df,
                                           draw = "draw",
                                           colnums_values = c(value1 = 2L, value2 = 5L),
                                           colnums_groups = integer(),
                                           type = NULL)
    ans_expected <- data.frame(g = character(),
                               value1 = rvec_dbl(),
                               h = character(),
                               value2 = rvec_int())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'collapse_to_rvec_inner' works with two ID columns and two data columns", {
    df <- data.frame(g = c("a", "a", "b", "b", "a", "a", "b", "b"),
                     value1 = 1:8,
                     h = c("y", "y", "y", "y", "z", "z", "z", "z"),
                     draw = c(1:2, 1:2, 1:2, 1:2),
                     value2 = 8:1)
    ans_obtained <- collapse_to_rvec_inner(data = df,
                                           draw = "draw",
                                           colnums_values = c(value1 = 2L, value2 = 5L),
                                           colnums_groups = integer(),
                                           type = "di")
    ans_expected <- data.frame(g = c("a", "b", "a", "b"),
                               value1 = rvec_dbl(rbind(1:2, 3:4, 5:6, 7:8)),
                               h = c("y", "y", "z", "z"),
                               value2 = rvec_int(rbind(8:7, 6:5, 4:3, 2:1)))
    expect_identical(ans_obtained, ans_expected)
})


## 'expand_from_rvec' ---------------------------------------------------------

test_that("'expand_from_rvec' works with ordinary data frame", {
    df <- data.frame(g = c("a", "b"),
                     value = rvec_int(rbind(1:2, 3:4)))
    ans_obtained <- expand_from_rvec(data = df)
    ans_expected <- data.frame(g = c("a", "a", "b", "b"),
                               draw = c(1:2, 1:2),
                               value = 1:4)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'expand_from_rvec' works with grouped data frame", {
    df <- data.frame(g = c("a", "b"),
                     value = rvec_int(rbind(1:2, 3:4)),
                     h = 1:2)
    df <- dplyr::group_by(df, g)
    ans_obtained <- expand_from_rvec(df)
    ans_expected <- data.frame(g = c("a", "a", "b", "b"),
                               draw = c(1:2, 1:2),
                               value = 1:4,
                               h = c(1L, 1L, 2L, 2L))
    ans_expected <- dplyr::group_by(ans_expected, g)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'expand_from_rvec' works with two ID columns and two data columns", {
    df <- data.frame(g = c("a", "b", "a", "b"),
                     value1 = rvec_int(rbind(1:2, 3:4, 5:6, 7:8)),
                     h = c("y", "y", "z", "z"),
                     value2 = rvec_int(rbind(8:7, 6:5, 4:3, 2:1)))
    ans_obtained <- expand_from_rvec(df)
    ans_expected <- data.frame(g = c("a", "a", "b", "b", "a", "a", "b", "b"),
                               draw = c(1:2, 1:2, 1:2, 1:2),
                               value1 = 1:8,
                               h = c("y", "y", "y", "y", "z", "z", "z", "z"),
                               value2 = 8:1)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'expand_from_rvec' works with nrow = 0", {
    df <- data.frame(g = character(),
                     value1 = rvec_int(),
                     value2 = rvec_dbl())
    ans_obtained <- expand_from_rvec(df)
    ans_expected <- data.frame(g = character(),
                               draw = integer(),
                               value1 = integer(),
                               value2 = double())
    expect_identical(ans_obtained, ans_expected)
})


## 'expand_from_rvec_inner' ---------------------------------------------------

test_that("'expand_from_rvec_inner' works with ordinary data frame", {
    df <- data.frame(g = c("a", "b"),
                     value = rvec_int(rbind(1:2, 3:4)))
    ans_obtained <- expand_from_rvec_inner(data = df,
                                           draw = "draw",
                                           colnums_values = c(value = 2L))
    ans_expected <- data.frame(g = c("a", "a", "b", "b"),
                               draw = c(1:2, 1:2),
                               value = 1:4)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'expand_from_rvec_inner' works with grouped data frame", {
    df <- data.frame(g = c("a", "b"),
                     value = rvec_int(rbind(1:2, 3:4)),
                     h = 1:2)
    df <- dplyr::group_by(df, g)
    ans_obtained <- expand_from_rvec_inner(data = df,
                                           draw = "draw",
                                           colnums_values = c(value = 2L))
    ans_expected <- data.frame(g = c("a", "a", "b", "b"),
                               draw = c(1:2, 1:2),
                               value = 1:4,
                               h = c(1L, 1L, 2L, 2L))
    ans_expected <- dplyr::group_by(ans_expected, g)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'expand_from_rvec_inner' works with two ID columns and two data columns", {
    df <- data.frame(g = c("a", "b", "a", "b"),
                     value1 = rvec_int(rbind(1:2, 3:4, 5:6, 7:8)),
                     h = c("y", "y", "z", "z"),
                     value2 = rvec_int(rbind(8:7, 6:5, 4:3, 2:1)))
    ans_obtained <- expand_from_rvec_inner(data = df,
                                           draw = "draw",
                                           colnums_values = c(value1 = 2L, value2 = 4L))
    ans_expected <- data.frame(g = c("a", "a", "b", "b", "a", "a", "b", "b"),
                               draw = c(1:2, 1:2, 1:2, 1:2),
                               value1 = 1:8,
                               h = c("y", "y", "y", "y", "z", "z", "z", "z"),
                               value2 = 8:1)
    expect_identical(ans_obtained, ans_expected)
})



    







                   
                   
