
test_that("'extract_draw' works with valid input", {
  set.seed(0)
  m <- matrix(rnorm(100), nr = 5)
  x <- rvec(m)
  for (i in c(1, 3, 20))
    expect_identical(extract_draw(x, i = i), m[,i])
  expect_true(is.double(extract_draw(x)))
})

test_that("'extract_draw' returns vector with expected type", {
  x <- rvec(1)
  expect_true(is.double(extract_draw(x)))
  x <- rvec(1L)
  expect_true(is.integer(extract_draw(x)))
  x <- rvec("a")
  expect_true(is.character(extract_draw(x)))
  x <- rvec(TRUE)
  expect_true(is.logical(extract_draw(x)))
})

test_that("'extract_draw' works when x has zero rows", {
  x <- rvec(matrix(0, nr = 0, nc = 1))
  ans_obtained <- extract_draw(x)
  ans_expected <- numeric()
  expect_identical(ans_obtained, ans_expected)
})

test_that("'extract_draw' throws correct error when i too high", {
  x <- rvec(matrix(1:4, nr = 2))
  expect_error(extract_draw(x, i = 3),
               "`i` is greater than the number of draws in `x`.")
})





  
