
## 'new_rvec' -----------------------------------------------------------------

test_that("'new_rvec' works with default arguments", {
  withr::local_options(lifecycle_verbosity = "quiet")
  x <- new_rvec()
  expect_s3_class(x, "rvec_dbl")
  expect_identical(length(x), 0L)
  expect_identical(n_draw(x), 1000L)
})

test_that("'new_rvec' works with character", {
  withr::local_options(lifecycle_verbosity = "quiet")
  x <- new_rvec(character(), length = 1)
  expect_s3_class(x, "rvec_chr")
  expect_identical(length(x), 1L)
  expect_identical(n_draw(x), 1000L)
})

test_that("'new_rvec' works with double", {
  withr::local_options(lifecycle_verbosity = "quiet")
  x <- new_rvec(333, length = 10, n_draw = 100)
  expect_s3_class(x, "rvec_dbl")
  expect_identical(length(x), 10L)
  expect_identical(n_draw(x), 100L)
})

test_that("'new_rvec' works with integer", {
  withr::local_options(lifecycle_verbosity = "quiet")
  x <- new_rvec(333:1000, length = 10, n_draw = 100)
  expect_s3_class(x, "rvec_int")
  expect_identical(length(x), 10L)
  expect_identical(n_draw(x), 100L)
})

test_that("'new_rvec' works with logical", {
  withr::local_options(lifecycle_verbosity = "quiet")
  x <- new_rvec(NA, length = 10, n_draw = 100)
  expect_s3_class(x, "rvec_lgl")
  expect_identical(length(x), 10L)
  expect_identical(n_draw(x), 100L)
})

test_that("'new_rvec' throws correct error with invalid type", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(new_rvec(NULL, length = 10, n_draw = 100),
               "Invalid type.")
})


## 'new_rvec_chr' -----------------------------------------------------------------

test_that("'new_rvec_chr' works with default arguments", {
  x <- new_rvec_chr()
  expect_s3_class(x, "rvec_chr")
  expect_identical(length(x), 0L)
  expect_identical(n_draw(x), 1000L)
})


## 'new_rvec_dbl' -----------------------------------------------------------------

test_that("'new_rvec_dbl' works with default arguments", {
  x <- new_rvec_dbl()
  expect_s3_class(x, "rvec_dbl")
  expect_identical(length(x), 0L)
  expect_identical(n_draw(x), 1000L)
})


## 'new_rvec_int' -----------------------------------------------------------------

test_that("'new_rvec_int' works with default arguments", {
  x <- new_rvec_int()
  expect_s3_class(x, "rvec_int")
  expect_identical(length(x), 0L)
  expect_identical(n_draw(x), 1000L)
})


## 'new_rvec_lgl' -----------------------------------------------------------------

test_that("'new_rvec_lgl' works with default arguments", {
  x <- new_rvec_lgl()
  expect_s3_class(x, "rvec_lgl")
  expect_identical(length(x), 0L)
  expect_identical(n_draw(x), 1000L)
})


test_that("new_rvec constructors fill every element and draw", {
  constructors <- list(new_rvec_chr, new_rvec_dbl, new_rvec_int, new_rvec_lgl)
  defaults <- list("", 0, 0L, FALSE)
  values <- list("ready", 2.5, 3L, TRUE)
  missing <- list(NA_character_, NA_real_, NA_integer_, NA)
  for (i in seq_along(constructors)) {
    constructor <- constructors[[i]]
    expect_identical(constructor(2, 3), rvec(matrix(defaults[[i]], 2, 3)))
    expect_identical(constructor(2, 3, value = values[[i]]),
                     rvec(matrix(values[[i]], 2, 3)))
    expect_identical(constructor(2, 3, value = NA),
                     rvec(matrix(missing[[i]], 2, 3)))
    expect_identical(constructor(2, 3, value = setNames(values[[i]], "ignored")),
                     constructor(2, 3, value = values[[i]]))
    expect_identical(constructor(0, 3, value = values[[i]]),
                     rvec(matrix(defaults[[i]], 0, 3)))
  }
})


test_that("new_rvec constructors use the existing coercion rules", {
  expect_identical(new_rvec_dbl(2, 3, value = 2L), rvec(matrix(2, 2, 3)))
  expect_identical(new_rvec_int(2, 3, value = 2), rvec(matrix(2L, 2, 3)))
  expect_identical(new_rvec_lgl(2, 3, value = 1), rvec(matrix(TRUE, 2, 3)))
  expect_identical(new_rvec_lgl(2, 3, value = 0L), rvec(matrix(FALSE, 2, 3)))
  expect_identical(new_rvec_dbl(2, 3, value = TRUE), rvec(matrix(1, 2, 3)))
  expect_identical(new_rvec_chr(2, 3, value = 42), rvec(matrix("42", 2, 3)))
  expect_identical(new_rvec_chr(2, 3, value = FALSE), rvec(matrix("FALSE", 2, 3)))
  expect_error(new_rvec_int(value = 2.5), class = "vctrs_error_cast_lossy")
  expect_error(new_rvec_lgl(value = 2), class = "vctrs_error_cast_lossy")
  expect_error(new_rvec_int(value = .Machine$integer.max + 1),
               class = "vctrs_error_cast_lossy")
  for (constructor in list(new_rvec_dbl, new_rvec_int, new_rvec_lgl))
    expect_error(constructor(value = "42"), class = "vctrs_error_cast")
  for (value in list(Inf, -Inf, NaN)) {
    expect_identical(new_rvec_dbl(2, 3, value = value), rvec(matrix(value, 2, 3)))
    expect_identical(new_rvec_chr(2, 3, value = value),
                     rvec(matrix(as.character(value), 2, 3)))
    if (is.nan(value)) {
      expect_identical(new_rvec_int(2, 3, value = value), rvec(matrix(NA_integer_, 2, 3)))
      expect_identical(new_rvec_lgl(2, 3, value = value), rvec(matrix(NA, 2, 3)))
    }
    else {
      expect_error(new_rvec_int(value = value), class = "vctrs_error_cast_lossy")
      expect_error(new_rvec_lgl(value = value), class = "vctrs_error_cast_lossy")
    }
  }
})


test_that("new_rvec constructors reject nonscalar and nonvector values", {
  invalid <- list(NULL, numeric(), c(1, 2), list(1), matrix(1),
                  array(1, c(1, 1, 1)), rvec(1), expression(1))
  for (constructor in list(new_rvec_chr, new_rvec_dbl, new_rvec_int, new_rvec_lgl)) {
    for (value in invalid) {
      for (n in c(0, 2))
        expect_error(constructor(length = n, value = value),
                     "value.*must be an atomic vector of length 1")
    }
  }
})


## 'rvec' ---------------------------------------------------------------------

test_that("'rvec' works with rvec", {
  m <- matrix(1:10, nr = 2)
  x <- rvec_dbl(m)
  expect_identical(rvec(x), rvec(1 * m))
  m_name <- rbind(a = "a", b = "b")
  x_name <- rvec_chr(m_name)
  expect_identical(rvec(x_name), rvec(m_name))
  m_int <- matrix(1:10, nr = 2)
  x_int <- rvec_int(m_int)
  expect_identical(rvec(x_int), rvec(m_int))
})

test_that("'rvec' works with valid matrices", {
  m <- matrix(1:6, 3)
  ans <- rvec(m)
  expect_s3_class(ans, "rvec_int")
  expect_identical(field(ans, "data"), m)
  m <- matrix("a", nrow = 3, ncol = 1)
  ans <- rvec(m)
  expect_s3_class(ans, "rvec_chr")
  m <- matrix(NA, nrow = 0, ncol = 1)
  ans <- rvec(m)
  expect_s3_class(ans, "rvec_lgl")
  m <- matrix(c(1, NA), nrow = 2, ncol = 3)
  ans <- rvec(m)
  expect_s3_class(ans, "rvec_dbl")
})

test_that("'rvec' works with valid Matrices", {
  m <- Matrix::Matrix(matrix(1:6, 3))
  ans <- rvec(m)
  expect_s3_class(ans, "rvec_dbl")
  expect_identical(field(ans, "data"), as.matrix(m))
  m <- Matrix::Matrix(matrix(c(1, NA), nrow = 2, ncol = 3))
  ans <- rvec(m)
  expect_s3_class(ans, "rvec_dbl")
})

test_that("'rvec' works with valid lists", {
  x <- list(1:3, 4:6)
  ans <- rvec(x)
  expect_s3_class(ans, "rvec_int")
  expect_identical(field(ans, "data"), rbind(x[[1]], x[[2]]))
  x <- list(x = "a", y = "b")
  ans <- rvec(x)
  expect_s3_class(ans, "rvec_chr")
  expect_identical(rownames(field(ans, "data")), c("x", "y"))
})

test_that("'rvec' works with valid vectors", {
  x <- c(X = "a", Y = "b")
  ans <- rvec(x)
  expect_s3_class(ans, "rvec_chr")
  expect_identical(field(ans, "data"),
                   matrix(c("a", "b"), nc = 1, dimnames = list(c("X", "Y"), NULL)))
  x <- integer()
  ans <- rvec(x)
  expect_s3_class(ans, "rvec_int")
  expect_identical(field(ans, "data"), matrix(integer(), nc = 1L))
})

test_that("'rvec' throws expected error with invalid input", {
  expect_error(rvec(list()),
               "If `x` is a list, it must have at least one element.")
  expect_error(rvec(NULL),
               "`x` must be an rvec, a matrix, a list, or an atomic vector.")
  expect_error(rvec(matrix(complex(1:3, 1:3), nr = 1)),
               "`x` must be double, integer, logical, or character")
})


## 'rvec_chr' -----------------------------------------------------------------

test_that("'rvec_chr' works with rvec", {
  m <- matrix(letters, nr = 2)
  x <- rvec_chr(m)
  expect_identical(rvec_chr(x), rvec_chr(m))
  m_name <- rbind(a = "a", b = "b")
  x_name <- rvec_chr(m_name)
  expect_identical(rvec_chr(x_name), rvec_chr(m_name))
  m_int <- matrix(1:10, nr = 2)
  x_int <- rvec_int(m_int)
  expect_identical(rvec_chr(x_int), rvec_chr(m_int))
})

test_that("'rvec_chr' works with NULL", {
  expect_s3_class(rvec_chr(NULL), "rvec_chr")
})

test_that("'rvec_chr' works with valid matrices", {
  ans <- rvec_chr(rbind(x = 1:2, y = c("a", "b")))
  expect_s3_class(ans, "rvec_chr")
  expect_identical(rownames(field(ans, "data")), c("x", "y"))
  expect_s3_class(rvec_chr(matrix("a")), "rvec_chr")
  expect_s3_class(rvec_chr(matrix("1")), "rvec_chr")
  expect_s3_class(rvec_chr(matrix(1)), "rvec_chr")
  expect_s3_class(rvec_chr(matrix(TRUE)), "rvec_chr")
})

test_that("'rvec_chr' works with valid lists", {
  ans <- rvec_chr(list(x = c("a", "b"), y = c("a", "c")))
  expect_s3_class(ans, "rvec_chr")
  expect_identical(rownames(field(ans, "data")), c("x", "y"))
  expect_identical(as.character(field(ans, "data")), c("a", "a", "b", "c"))
  expect_s3_class(rvec_chr(list("a")), "rvec_chr")
  expect_s3_class(rvec_chr(list()), "rvec_chr")
  expect_s3_class(rvec_chr(list(1)), "rvec_chr")
  expect_s3_class(rvec_chr(list(1L)), "rvec_chr")
  expect_s3_class(rvec_chr(list(TRUE)), "rvec_chr")
})

test_that("'rvec_chr' works with vectors", {
  ans <- rvec_chr(letters)
  expect_s3_class(ans, "rvec_chr")
  expect_identical(rownames(field(ans, "data")), NULL)
  ans <- rvec_chr(c(x = 1L))
  expect_s3_class(ans, "rvec_chr")
  expect_identical(rownames(field(ans, "data")), "x")
  expect_s3_class(rvec_chr(double()), "rvec_chr")
})

test_that("'rvec_chr' throws error with invalid inputs", {
  expect_error(rvec_chr(lm),
               "`x` must be an rvec, a matrix, a list, an atomic vector, or NULL.")
})


## 'rvec_dbl' -----------------------------------------------------------------

test_that("'rvec_dbl' works with rvec", {
  m <- matrix(1:10 + 0.1, nr = 2)
  x <- rvec_dbl(m)
  expect_identical(rvec_dbl(x), rvec_dbl(m))
  m_name <- rbind(a = 1:2 + 0.1, b = 3:4)
  x_name <- rvec_dbl(m_name)
  expect_identical(rvec_dbl(x_name), rvec_dbl(m_name))
  m_int <- matrix(1:10, nr = 2)
  x_int <- rvec_int(m_int)
  expect_identical(rvec_dbl(x_int), rvec_dbl(m_int))
})

test_that("'rvec_dbl' works with NULL", {
  expect_s3_class(rvec_dbl(NULL), "rvec_dbl")
})

test_that("'rvec_dbl' works with valid matrices", {
  ans <- rvec_dbl(rbind(x = 1:2, y = c(-1, Inf)))
  expect_s3_class(ans, "rvec_dbl")
  expect_identical(rownames(field(ans, "data")), c("x", "y"))
  expect_s3_class(rvec_dbl(matrix(NA_real_)), "rvec_dbl")
  expect_s3_class(rvec_dbl(matrix(1)), "rvec_dbl")
  expect_s3_class(rvec_dbl(matrix(1L)), "rvec_dbl")
  expect_s3_class(rvec_dbl(matrix(TRUE)), "rvec_dbl")
})

test_that("'rvec_dbl' works with valid Matrices", {
  m <- Matrix::Matrix(matrix(1:6, 3))
  ans <- rvec_dbl(m)
  expect_s3_class(ans, "rvec_dbl")
  expect_identical(field(ans, "data"), as.matrix(m))
  m <- Matrix::Matrix(matrix(c(1, NA), nrow = 2, ncol = 3))
  ans <- rvec_dbl(m)
  expect_s3_class(ans, "rvec_dbl")
})

test_that("'rvec_dbl' works with valid lists", {
  ans <- rvec_dbl(list(x = c(1, -Inf), y = c(NA, Inf)))
  expect_s3_class(ans, "rvec_dbl")
  expect_identical(rownames(field(ans, "data")), c("x", "y"))
  expect_s3_class(rvec_dbl(list(0.01)), "rvec_dbl")
  expect_s3_class(rvec_dbl(list()), "rvec_dbl")
  expect_s3_class(rvec_dbl(list(1)), "rvec_dbl")
  expect_s3_class(rvec_dbl(list(1L)), "rvec_dbl")
  expect_s3_class(rvec_dbl(list(TRUE)), "rvec_dbl")
  expect_identical(as.double(field(ans, "data")), c(1, NA, -Inf, Inf))
})

test_that("'rvec_dbl' works with valid vectors", {
  ans <- rvec_dbl(c(a = 1, b = 2, c = Inf))
  expect_s3_class(ans, "rvec_dbl")
  expect_identical(rownames(field(ans, "data")), c("a", "b", "c"))
  expect_s3_class(rvec_dbl(NA_real_), "rvec_dbl")
  expect_s3_class(rvec_dbl(1), "rvec_dbl")
  expect_s3_class(rvec_dbl(1L), "rvec_dbl")
  expect_s3_class(rvec_dbl(TRUE), "rvec_dbl")
})

test_that("'rvec_dbl' throws error with invalid inputs", {
  expect_error(rvec_dbl(lm),
               "`x` must be an rvec, a matrix, a list, an atomic vector, or NULL.")
})


## 'rvec_int' -----------------------------------------------------------------

test_that("'rvec_int' works with rvec", {
  m <- matrix(1:10, nr = 2)
  x <- rvec_int(m)
  expect_identical(rvec_int(x), rvec_int(m))
  x <- rvec_dbl(m)
  expect_identical(rvec_int(x), rvec_int(m))
  m_name <- rbind(a = 1:2, b = 3:4)
  x_name <- rvec_int(m_name)
  expect_identical(rvec_int(x_name), rvec_int(m_name))
})

test_that("'rvec_int' works with NULL", {
  expect_s3_class(rvec_int(NULL), "rvec_int")
})

test_that("'rvec_int' works with valid matrices", {
  ans <- rvec_int(rbind(x = 1:2, y = c(-1, NA)))
  expect_s3_class(ans, "rvec_int")
  expect_identical(rownames(field(ans, "data")), c("x", "y"))
  expect_s3_class(rvec_int(matrix(NA_real_)), "rvec_int")
  expect_s3_class(rvec_int(matrix(1)), "rvec_int")
  expect_s3_class(rvec_int(matrix(1L)), "rvec_int")
  expect_s3_class(rvec_int(matrix(TRUE)), "rvec_int")
})

test_that("'rvec_int' works with valid lists", {
  ans <- rvec_int(list(x = c(1, 1), y = c(NA, 0)))
  expect_s3_class(ans, "rvec_int")
  expect_identical(rownames(field(ans, "data")), c("x", "y"))
  expect_s3_class(rvec_int(list()), "rvec_int")
  expect_s3_class(rvec_int(list(1)), "rvec_int")
  expect_s3_class(rvec_int(list(1L)), "rvec_int")
  expect_s3_class(rvec_int(list(TRUE)), "rvec_int")
  expect_identical(as.integer(field(ans, "data")), c(1L, NA, 1L, 0L))
})

test_that("'rvec_int' works with valid vectors", {
  ans <- rvec_int(c(x = 1, y = 2, z = NA))
  expect_s3_class(ans, "rvec_int")
  expect_identical(rownames(field(ans, "data")), c("x", "y", "z"))
  expect_s3_class(rvec_int(NA_real_), "rvec_int")
  expect_s3_class(rvec_int(1), "rvec_int")
  expect_s3_class(rvec_int(1L), "rvec_int")
  expect_s3_class(rvec_int(TRUE), "rvec_int")
})

test_that("'rvec_int' throws error with invalid inputs", {
  expect_error(rvec_int(lm),
               "`x` must be an rvec, a matrix, a list, an atomic vector, or NULL.")
})


## 'rvec_lgl' -----------------------------------------------------------------

test_that("'rvec_lgl' works with rvec", {
  m <- matrix(c(T,F,F,T), nr = 2)
  x <- rvec_lgl(m)
  expect_identical(rvec_lgl(x), rvec_lgl(m))
  x <- rvec_int(m)
  expect_identical(rvec_lgl(x), rvec_lgl(m))
  m_name <- rbind(a = F, b = T)
  x_name <- rvec_lgl(m_name)
  expect_identical(rvec_lgl(x_name), rvec_lgl(m_name))
})

test_that("'rvec_lgl' works with NULL", {
  expect_s3_class(rvec_lgl(NULL), "rvec_lgl")
})

test_that("'rvec_lgl' works with valid matrices", {
  ans <- rvec_lgl(rbind(x = c(TRUE, NA), y = c(FALSE, FALSE)))
  expect_s3_class(ans, "rvec_lgl")
  expect_identical(rownames(field(ans, "data")), c("x", "y"))
  expect_s3_class(rvec_lgl(matrix(NA)), "rvec_lgl")
  expect_s3_class(rvec_lgl(matrix(FALSE)), "rvec_lgl")
  expect_s3_class(rvec_lgl(matrix(TRUE)), "rvec_lgl")
})

test_that("'rvec_lgl' works with valid lists", {
  ans <- rvec_lgl(list(x = c(NA, TRUE), y = c(NA, FALSE)))
  expect_s3_class(ans, "rvec_lgl")
  expect_identical(rownames(field(ans, "data")), c("x", "y"))
  expect_s3_class(rvec_lgl(list()), "rvec_lgl")
  expect_s3_class(rvec_lgl(list(NA)), "rvec_lgl")
  expect_s3_class(rvec_lgl(list(TRUE)), "rvec_lgl")
  expect_identical(as.logical(field(ans, "data")), c(NA, NA, TRUE, FALSE))
})

test_that("'rvec_lgl' works with valid vectors", {
  ans <- rvec_lgl(c(x = 1, y = 0, z = NA))
  expect_s3_class(ans, "rvec_lgl")
  expect_identical(rownames(field(ans, "data")), c("x", "y", "z"))
  expect_s3_class(rvec_lgl(NA_real_), "rvec_lgl")
  expect_s3_class(rvec_lgl(1), "rvec_lgl")
  expect_s3_class(rvec_lgl(1L), "rvec_lgl")
  expect_s3_class(rvec_lgl(TRUE), "rvec_lgl")
})

test_that("'rvec_lgl' throws error with invalid inputs", {
  expect_error(rvec_lgl(lm),
               "`x` must be an rvec, a matrix, a list, an atomic vector, or NULL.")
})


## '.new_rvec' -----------------------------------------------------------------

test_that("'.new_rvec' works with dbl", {
  x <- .new_rvec(type = "dbl", length = 0, n_draw = 1000)
  expect_s3_class(x, "rvec_dbl")
  expect_identical(length(x), 0L)
  expect_identical(n_draw(x), 1000L)
})

test_that("'.new_rvec' works with character", {
  x <- .new_rvec("chr", length = 1, n_draw = 1000)
  expect_s3_class(x, "rvec_chr")
  expect_identical(length(x), 1L)
  expect_identical(n_draw(x), 1000L)
})

test_that("'.new_rvec' works with integer", {
  x <- .new_rvec("int", length = 10, n_draw = 100)
  expect_s3_class(x, "rvec_int")
  expect_identical(length(x), 10L)
  expect_identical(n_draw(x), 100L)
})

test_that("'.new_rvec' works with logical", {
  x <- .new_rvec("lgl", length = 10, n_draw = 100)
  expect_s3_class(x, "rvec_lgl")
  expect_identical(length(x), 10L)
  expect_identical(n_draw(x), 100L)
})


## '.new_rvec_chr' -----------------------------------------------------------------

test_that("'.new_rvec_chr' works", {
  m <- matrix("a", nr = 3, nc = 4)
  x <- .new_rvec_chr(m)
  expect_s3_class(x, c("rvec_chr", "rvec"))
})


## '.new_rvec_dbl' -----------------------------------------------------------------

test_that("'.new_rvec_dbl' works", {
  m <- matrix(NA_real_, nr = 3, nc = 4)
  x <- .new_rvec_dbl(m)
  expect_s3_class(x, c("rvec_dbl", "rvec"))
})


## '.new_rvec_int' -----------------------------------------------------------------

test_that("'.new_rvec_int' works", {
  m <- matrix(NA_integer_, nr = 3, nc = 4)
  x <- .new_rvec_int(m)
  expect_s3_class(x, c("rvec_int", "rvec"))
})


## '.new_rvec_lgl' -----------------------------------------------------------------

test_that("'.new_rvec_lgl' works", {
  m <- matrix(NA, nr = 3, nc = 4)
  x <- .new_rvec_lgl(m)
  expect_s3_class(x, c("rvec_lgl", "rvec"))
})








test_that("same-type constructors preserve names and isolate later mutations", {
    constructors <- list(rvec_chr, rvec_dbl, rvec_int, rvec_lgl)
    values <- list(c("a", NA), c(1, NA_real_), c(1L, NA_integer_), c(TRUE, NA))
    for (i in seq_along(values)) for (n in c(0L, 2L)) {
        m <- matrix(rep(values[[i]], length.out = n * 3L), n, 3L)
        if (n > 0L)
            dimnames(m) <- list(rows = c("a", "b"), draws = letters[1:3])
        expected <- m
        attributes(expected) <- list(dim = dim(m))
        rownames(expected) <- rownames(m)
        fun <- constructors[[i]]
        for (x in list(m, rvec(m))) {
            before <- serialize(x, NULL)
            obtained <- fun(x)
            expect_identical(as.matrix(obtained), expected)
            expect_identical(serialize(x, NULL), before)
            if (n > 0L) {
                obtained[1L] <- obtained[2L]
                expect_identical(serialize(x, NULL), before)
            }
        }
        attr(m, "extra") <- "discard"
        expect_identical(as.matrix(fun(m)), expected)
    }
})
