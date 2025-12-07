
## 'pool_draws' ---------------------------------------------------------

test_that("'pool_draws' works with one by variable", {
  df <- data.frame(a = c(1, 1, 1, 2, 2, 2),
                   x = rvec(1:6),
                   y = rvec(matrix(1:12, nr = 6)))
  ans_obtained <- pool_draws(df, by = a)
  ans_expected <- tibble::tibble(a = c(1, 2),
                                 x = rvec(rbind(1:3, 4:6)),
                                 y = rvec(rbind(c(1:3, 7:9),
                                                c(4:6, 10:12))))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'pool_draws' works with one grouping variable", {
  df <- data.frame(a = c(1, 1, 1, 2, 2, 2),
                   x = rvec(1:6),
                   y = rvec(matrix(1:12, nr = 6))) |>
    dplyr::group_by(a)
  ans_obtained <- pool_draws(df)
  ans_expected <- tibble::tibble(a = c(1, 2),
                                 x = rvec(rbind(1:3, 4:6)),
                                 y = rvec(rbind(c(1:3, 7:9),
                                                c(4:6, 10:12))))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'pool_draws_inner' works with two by variables", {
  df <- data.frame(a = c(1, 1, 1, 2, 2, 2),
                   b = c(1:3, 1:3),
                   x = rvec(1:6),
                   y = rvec(matrix(1:12, nr = 6)))
  ans_obtained <- pool_draws(df, by = c(a, b))
  ans_expected <- tibble::tibble(df)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'pool_draws_inner' works with two by grouping", {
  df <- data.frame(a = c(1, 1, 1, 2, 2, 2),
                   b = c(1:3, 1:3),
                   x = rvec(1:6),
                   y = rvec(matrix(1:12, nr = 6)))
  dfg <- df |>
    dplyr::group_by(a,b)
  ans_obtained <- pool_draws(dfg)
  ans_expected <- tibble::tibble(df)
  expect_identical(ans_obtained, ans_expected)
})


## 'pool_draws_inner' ---------------------------------------------------------

test_that("'pool_draws_inner' works with one by variable", {
  df <- data.frame(a = c(1, 1, 1, 2, 2, 2),
                   x = rvec(1:6),
                   y = rvec(matrix(1:12, nr = 6)))
  ans_obtained <- pool_draws_inner(df,
                                   by_colnums = c(a = 1L),
                                   groups_colnums = integer())
  ans_expected <- tibble::tibble(a = c(1, 2),
                                 x = rvec(rbind(1:3, 4:6)),
                                 y = rvec(rbind(c(1:3, 7:9),
                                                c(4:6, 10:12))))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'pool_draws_inner' works with two groups variables", {
  df <- data.frame(a = c(1, 1, 1, 2, 2, 2),
                   b = c(1:3, 1:3),
                   x = rvec(1:6),
                   y = rvec(matrix(1:12, nr = 6)))
  ans_obtained <- pool_draws_inner(df,
                                   by_colnums = integer(),
                                   groups_colnums = c(a = 1L,
                                                      b = 2L))
  ans_expected <- tibble::tibble(df)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'pool_draws_inner' works with no groups or by variables", {
  df <- data.frame(a = c(1, 1, 1, 2, 2, 2),
                   b = c(1:3, 1:3),
                   x = rvec(1:6),
                   y = rvec(matrix(1:12, nr = 6)))
  ans_obtained <- pool_draws_inner(df,
                                   by_colnums = integer(),
                                   groups_colnums = integer())
  ans_expected <- tibble::tibble(x = rvec(matrix(1:6, nr = 1)),
                                 y = rvec(matrix(1:12, nr = 1)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'pool_draws_inner' throws correct error with zero rows'", {
  df <- data.frame(a = integer(),
                   x = new_rvec_dbl(),
                   y = new_rvec_int())
  expect_error(pool_draws_inner(df,
                                by_colnums = c(a = 1L),
                                groups_colnums = integer()),
               "`data` has 0 rows.")
})

test_that("'pool_draws_inner' throws correct error with no rvecs'", {
  df <- data.frame(a = 1,
                   x = 2,
                   y = 3)
  expect_error(pool_draws_inner(df,
                                by_colnums = c(a = 1L),
                                groups_colnums = integer()),
               "`data` does not have any rvecs.")
})

test_that("'pool_draws_inner' throws correct error with one uneven 'by'", {
  df <- data.frame(a = c(1, 1, 1, 2, 2),
                   x = rvec(1:5),
                   y = rvec(matrix(1:10, nr = 5)))
  expect_error(pool_draws_inner(df,
                                by_colnums = c(a = 1L),
                                groups_colnums = integer()),
               "Some values of the 'by' variable occur more often than others.")
})

test_that("'pool_draws_inner' throws correct error with one uneven grouping", {
  df <- data.frame(a = c(1, 1, 1, 2, 2),
                   x = rvec(1:5),
                   y = rvec(matrix(1:10, nr = 5)))
  expect_error(pool_draws_inner(df,
                                groups_colnums = c(a = 1L),
                                by_colnums = integer()),
               "Some values of the grouping variable occur more often than others.")
})

test_that("'pool_draws_inner' throws correct error with uneven 'by' variables", {
  df <- data.frame(a = c(1, 1, 1, 2, 2, 2),
                   b = c(1:3, 1:2, 2),
                   x = rvec(1:6),
                   y = rvec(matrix(1:12, nr = 6)))
  expect_error(pool_draws_inner(df,
                                groups_colnums = integer(),
                                by_colnums = c(a = 1L,
                                               b = 2L)),
               "Some combinations of the 'by' variables occur more often than others.")
})

test_that("'pool_draws_inner' throws correct error with uneven grouping variables", {
  df <- data.frame(a = c(1, 1, 1, 2, 2, 2),
                   b = c(1:3, 1:2, 2),
                   x = rvec(1:6),
                   y = rvec(matrix(1:12, nr = 6)))
  expect_error(pool_draws_inner(df,
                                by_colnums = integer(),
                                groups_colnums = c(a = 1L,
                                               b = 2L)),
               "Some combinations of the grouping variables occur more often than others.")
})







## 'pool_draws_df' ------------------------------------------------------------

test_that("'pool_draws_vec' works with nrow > 0", {
  df <- data.frame(x = rvec(1:5), y = rvec(matrix(1:10, nr = 5)))
  ans_obtained <- pool_draws_df(df)
  ans_expected <- tibble::tibble(x = rvec(matrix(1:5, nr = 1)),
                                 y = rvec(matrix(1:10, nr = 1)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'pool_draws_vec' works with nrow = 0", {
  df <- data.frame(x = new_rvec_dbl())
  ans_obtained <- pool_draws_df(df)
  ans_expected <- df
  expect_identical(ans_obtained, ans_expected)
})


## 'pool_draws_vec' -----------------------------------------------------------

test_that("'pool_draws_vec' works with rvec length > 1", {
  m <- matrix(1:12, nr = 3)
  vec <- rvec(m)
  ans_obtained <- pool_draws_vec(vec)
  ans_expected <- rvec_int(matrix(as.integer(m), nr = 1))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'pool_draws_vec' works with rvec length 1", {
  m <- 1 * matrix(1:12, nr = 1)
  vec <- rvec(m)
  ans_obtained <- pool_draws_vec(vec)
  ans_expected <- vec
  expect_identical(ans_obtained, ans_expected)
})

test_that("'pool_draws_vec' works with rvec length 0", {
  vec <- new_rvec_dbl(n_draw = 5)
  ans_obtained <- pool_draws_vec(vec)
  ans_expected <- vec
  expect_identical(ans_obtained, ans_expected)
})

test_that("'pool_draws_vec' works with character rvec", {
  m <- matrix(letters[1:12], nr = 3)
  vec <- rvec(m)
  ans_obtained <- pool_draws_vec(vec)
  ans_expected <- rvec(matrix(m, nr = 1))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'pool_draws_vec' works with logical rvec", {
  m <- matrix(rep(FALSE, 12), nr = 3)
  vec <- rvec(m)
  ans_obtained <- pool_draws_vec(vec)
  ans_expected <- rvec(matrix(m, nr = 1))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'pool_draws_vec' throws error with non-rvec", {
  expect_error(pool_draws_vec(1),
               "Internal error: `vec` is not an rvec.")
})




