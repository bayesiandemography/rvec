
test_that("'%*%' works when x is an rvec and y is an rvec", {
    x <- rvec(matrix(1:9, nr = 3))
    y <- rvec(matrix(11:19, nr = 3))
    ans_obtained <- x %*% y
    ans_expected <- rvec(sum(x * y))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'%*%' works when x is a matrix and y is an rvec", {
    x <- matrix(1:9, nr = 3)
    y <- rvec(matrix(1:6, nr = 3))
    ans_obtained <- x %*% y
    ans_expected <- rvec(matrix(1:9, 3) %*% matrix(1:6, 3))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'%*%' works when x is an rvec and y is a matrix", {
    m <- matrix(1:6, nr = 3)
    x <- rvec(m)
    y <- matrix(11:16, nr = 3)
    ans_obtained <- x %*% y
    ans_expected <- rvec(t(t(m) %*% y))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'%*%' works when x is a Matrix and y is an rvec", {
  x <- matrix(1:9, nr = 3)
  x <- Matrix::Matrix(x)
  y <- rvec(matrix(1:6, nr = 3))
  ans_obtained <- x %*% y
  ans_expected <- rvec(matrix(1:9, 3) %*% matrix(1:6, 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("Matrix %*% rvec gives the same result as acting on underlying data", {
  data_mat <- matrix(
    c(1, 2,   # element 1, draws 1–2
      3, 4,   # element 2, draws 1–2
      5, 6),  # element 3, draws 1–2
    nrow = 3, byrow = TRUE
  )
  rv <- rvec(data_mat)
  M <- Matrix::Matrix(
    c(10, 10, 10,
      11, 11, 11),
    nrow = 2, byrow = TRUE
  )
  res <- M %*% rv
  expect_true(is_rvec(res))
  expected_mat <- as.matrix(M) %*% field(rv, "data")
  expect_equal(field(res, "data"), expected_mat)
})

test_that("rvec %*% Matrix gives the same result as acting on underlying data", {
  data_mat <- matrix(
    c(1, 2,
      3, 4,
      5, 6),
    nrow = 3, byrow = TRUE
  )
  rv <- rvec(data_mat)  ## length 3, 2 draws
  M <- Matrix::Matrix(
    c(10, 20,
      30, 40,
      50, 60),
    nrow = 3, byrow = TRUE
  )
  res <- rv %*% M
  expect_true(is_rvec(res))
  expected_mat <- t(crossprod(field(rv, "data"), as.matrix(M)))
  expect_equal(field(res, "data"), expected_mat)
})

test_that("Matrix %*% rvec and base-matrix %*% rvec are consistent", {
  data_mat <- matrix(
    c(1, 2, 3,
      4, 5, 6,
      7, 8, 9,
      10, 11, 12),
    nrow = 4, byrow = TRUE
  )
  rv <- rvec(data_mat)
  base_M <- matrix(
    c(1, 2, 3, 4,
      5, 6, 7, 8),
    nrow = 2, byrow = TRUE
  )
  M <- Matrix::Matrix(base_M)
  base_res <- base_M %*% rv
  Matrix_res <- M %*% rv
  expect_true(is_rvec(base_res))
  expect_true(is_rvec(Matrix_res))
  expect_equal(field(Matrix_res, "data"), field(base_res, "data"))
})

test_that("rvec %*% Matrix and rvec %*% base-matrix are consistent", {
  data_mat <- matrix(
    c(1, 2, 3,
      4, 5, 6,
      7, 8, 9,
      10, 11, 12),
    nrow = 4, byrow = TRUE
  )
  rv <- rvec(data_mat)
  base_M <- matrix(
    c(1, 2,
      3, 4,
      5, 6,
      7, 8),
    nrow = 4, byrow = TRUE
  )
  M <- Matrix::Matrix(base_M)
  base_res   <- rv %*% base_M
  Matrix_res <- rv %*% M
  expect_true(is_rvec(base_res))
  expect_true(is_rvec(Matrix_res))
  expect_equal(field(Matrix_res, "data"), field(base_res, "data"))
})

test_that("Matrix %*% rvec respects dimensions", {
  data_mat <- matrix(
    seq_len(3 * 5),
    nrow = 3, byrow = TRUE
  )
  rv <- rvec(data_mat)
  M <- Matrix::Matrix(
    c(1, 0, 0,
      0, 1, 0),
    nrow = 2, byrow = TRUE
  )
  res <- M %*% rv
  expect_true(is_rvec(res))
  out_mat <- field(res, "data")
  expect_identical(dim(out_mat), c(2L, 5L))
})

test_that("rvec %*% Matrix respects dimensions", {
  data_mat <- matrix(
    seq_len(3 * 5),
    nrow = 3, byrow = TRUE
  )
  rv <- rvec(data_mat)
  M <- Matrix::Matrix(
    c(1, 0,
      0, 1,
      0, 0),
    nrow = 3, byrow = TRUE
  )
  res <- rv %*% M
  expect_true(is_rvec(res))
  out_mat <- field(res, "data")
  expect_identical(dim(out_mat), c(2L, 5L))
})

