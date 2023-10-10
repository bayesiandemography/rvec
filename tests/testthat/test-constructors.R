
## 'rvec' ---------------------------------------------------------------------

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
                 "`x` must be a matrix, a list, or an atomic vector.")
    expect_error(rvec(matrix(complex(1:3, 1:3), nr = 1)),
                 "`x` must be double, integer, logical, or character")
})


## 'rvec_chr' -----------------------------------------------------------------

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
                 "`x` must be a matrix, a list, an atomic vector, or NULL.")
})


## 'rvec_dbl' -----------------------------------------------------------------

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
                 "`x` must be a matrix, a list, an atomic vector, or NULL.")
})


## 'rvec_int' -----------------------------------------------------------------

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
                 "`x` must be a matrix, a list, an atomic vector, or NULL.")
})


## 'rvec_lgl' -----------------------------------------------------------------

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
                 "`x` must be a matrix, a list, an atomic vector, or NULL.")
})


## 'new_rvec' -----------------------------------------------------------------

test_that("'new_rvec' works", {
    m <- matrix("a", nr = 3, nc = 4)
    x <- new_rvec(m)
    expect_s3_class(x, c("rvec_chr", "rvec"))
    m <- matrix(0, nr = 3, nc = 4)
    x <- new_rvec(m)
    expect_s3_class(x, c("rvec_dbl", "rvec"))
    m <- matrix(0L, nr = 3, nc = 4)
    x <- new_rvec(m)
    expect_s3_class(x, c("rvec_int", "rvec"))
    m <- matrix(TRUE, nr = 3, nc = 4)
    x <- new_rvec(m)
    expect_s3_class(x, c("rvec_lgl", "rvec"))
})

test_that("'new_rvec' throws error with invalid inputs", {
    expect_error(new_rvec(matrix(complex(real = 0, imaginary = 0), 2, 3)),
                 "Internal error: `data` has type complex.")
})


## 'new_chr' -----------------------------------------------------------------

test_that("'new_rvec_chr' works", {
    m <- matrix("a", nr = 3, nc = 4)
    x <- new_rvec_chr(m)
    expect_s3_class(x, c("rvec_chr", "rvec"))
})


## 'new_dbl' -----------------------------------------------------------------

test_that("'new_rvec_dbl' works", {
    m <- matrix(NA_real_, nr = 3, nc = 4)
    x <- new_rvec_dbl(m)
    expect_s3_class(x, c("rvec_dbl", "rvec"))
})


## 'new_int' -----------------------------------------------------------------

test_that("'new_rvec_int' works", {
    m <- matrix(NA_integer_, nr = 3, nc = 4)
    x <- new_rvec_int(m)
    expect_s3_class(x, c("rvec_int", "rvec"))
})


## 'new_lgl' -----------------------------------------------------------------

test_that("'new_rvec_lgl' works", {
    m <- matrix(NA, nr = 3, nc = 4)
    x <- new_rvec_lgl(m)
    expect_s3_class(x, c("rvec_lgl", "rvec"))
})
    



                
    
