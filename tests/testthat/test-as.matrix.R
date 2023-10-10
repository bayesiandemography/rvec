
## 'as.character' -------------------------------------------------------------

test_that("'as.character' method for rvec works", {
    m <- matrix(1:4, nr = 2)
    x <- rvec(m)
    expect_identical(as.character(x),
                     as.character(m))
})


## 'as.double' -------------------------------------------------------------

test_that("'as.double' method for rvec works", {
    m <- matrix(1:4, nr = 2)
    x <- rvec(m)
    expect_identical(as.double(x),
                     as.double(m))
})

test_that("'as.double' method for rvec gives expected warning", {
    m <- matrix(c("a", 1, 0, -1), nr = 2)
    x <- rvec(m)
    expect_warning(as.double(x),
                   "NAs introduced by coercion")
})    


## 'as.integer' -------------------------------------------------------------

test_that("'as.integer' method for rvec works", {
    m <- matrix(1:4, nr = 2)
    x <- rvec(m)
    expect_identical(as.integer(x),
                     as.integer(m))
})

test_that("'as.integer' method for rvec gives expected warning", {
    m <- matrix(c("a", 1, 0, -1), nr = 2)
    x <- rvec(m)
    expect_warning(as.integer(x),
                   "NAs introduced by coercion")
})    

## 'as.logical' -------------------------------------------------------------

test_that("'as.logical' method for rvec works", {
    m <- matrix(c(0, 1, 0, -1), nr = 2)
    x <- rvec(m)
    expect_identical(as.logical(x),
                     as.logical(m))
})


## 'as.numeric' -------------------------------------------------------------

test_that("'as.numeric' method for rvec works", {
    m <- matrix(c(0, 1, 0, -1), nr = 2)
    x <- rvec(m)
    expect_identical(as.numeric.rvec(x),
                     as.numeric(m))
    expect_identical(as.numeric(x),
                     as.numeric(m))
})

test_that("'as.numeric' method for rvec gives expected warning", {
    m <- matrix(c("a", 1, 0, -1), nr = 2)
    x <- rvec(m)
    expect_warning(as.numeric(x),
                   "NAs introduced by coercion")
})    


## 'as.matrix' ----------------------------------------------------------------

test_that("'as.matrix' method for rvec works", {
    m <- matrix(c(T, F), nr = 1)
    x <- rvec(m)
    expect_identical(as.matrix(x), m)
    m <- matrix(character(), nr = 0, nc = 3)
    x <- rvec(m)
    expect_identical(as.matrix(x), m)
})
