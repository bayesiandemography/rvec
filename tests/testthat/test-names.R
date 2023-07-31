
## 'names' --------------------------------------------------------------------

test_that("'names' method for rvec works - names complete", {
    x <- rvec(list(a = 1:2, b = 3:4))
    expect_identical(names(x), c("a", "b"))
    y <- rvec(c(a = 1, b = 2))
    expect_identical(names(y), c("a", "b"))
})

test_that("'names' method for rvec works - names incomplete", {
    x <- rvec(list(a = 1:2, 3:4))
    expect_identical(names(x), names(c(a = 1, 2)))
})

test_that("'names' method for rvec works - no names", {
    x <- rvec(list(1:2, 3:4))
    expect_identical(names(x), names(c(1, 2)))
})

test_that("'names' method for rvec works - zero length", {
    x <- rvec_int()
    expect_identical(names(x), names(integer()))
})


## 'names<-' ------------------------------------------------------------------

test_that("'names<-' method for rvec works - remplacement complete", {
    x <- rvec(list(1:2, 3:4))
    names(x) <- c("A", "B")
    expect_identical(names(x), c("A", "B"))
})

test_that("'names<-' method for rvec works - names incomplete", {
    x <- rvec(list(1:2, 3:4))
    names(x) <- "A"
    X <- c(1, 2)
    names(X) <- "A"
    expect_identical(names(x), names(X))
})

test_that("'names<-' method for rvec works - NULL", {
    x <- rvec(list(a = 1:2, b = 3:4))
    names(x) <- NULL
    expect_identical(names(x), NULL)
})

test_that("'names<-' method throws expected error, names too long", {
    x <- rvec_int(list(a = 1:2))
    expect_error(names(x) <- c("A", "B"),
                 "Names vector too long.")
})



