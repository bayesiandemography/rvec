
test_that("'map_rvec' works with valid inputs", {
    m1 <- matrix(1:10, nr = 5)
    m2 <- matrix(21:30, nr = 5)
    x1 <- rvec(m1)
    x2 <- rvec(m2)
    .x <- list(a = x1, b = x2)
    ans_obtained <- map_rvec(.x, median)
    ans_expected <- c(rvec(matrix(apply(m1, 2, median),
                                  nr = 1,
                                  dimnames = list("a", NULL))),
                      rvec(matrix(apply(m2, 2, median),
                                  nr = 1,
                                  dimnames = list("b", NULL))))
    expect_identical(ans_obtained, ans_expected)
})


test_that("'map_rvec' throws expected error when '.x' missing", {
    expect_error(map_rvec(.f = mean),
                 "`.x` is missing, with no default.")
})

test_that("'map_rvec' throws expected error when '.x' missing", {
    expect_error(map_rvec(.x = 1:3),
                 "`.f` is missing, with no default.")
})

test_that("'map_rvec' throws expected error when '.x' is not a vector", {
    expect_error(map_rvec(.x = mean, .f = mean),
                 "`.x` is not a vector")
})

test_that("'map_rvec' throws expected error when '.f' is not a function", {
    expect_error(map_rvec(.x = 1, .f = 1),
                 "`.f` is not a function")
})

test_that("'map_rvec' throws expected error when can't apply '.f'", {
    m1 <- matrix(1:10, nr = 1)
    x1 <- rvec(m1)
    x2 <- mean
    .x <- list(a = x1, b = x2)
    expect_error(map_rvec(.x = .x, .f = log),
                 "Problem applying `log\\(\\)` to element 2 of `.x`.")
})

test_that("'map_rvec' throws expected error when return value not length 1", {
    m1 <- matrix(1:10, nr = 5)
    m2 <- matrix(21:30, nr = 5)
    x1 <- rvec(m1)
    x2 <- rvec(m2)
    .x <- list(a = x1, b = x2)
    expect_error(map_rvec(.x, log),
                 paste("Return value from applying `log\\(\\)` to element 1",
                       "of `.x` is not length 1."))
})

test_that("'map_rvec' throws expected error when return value not length 1", {
    m1 <- matrix(1:10, nr = 5)
    m2 <- matrix(21:30, nr = 5)
    x1 <- rvec(m1)
    x2 <- rvec(m2)
    .x <- list(a = x1, b = x2)
    expect_error(map_rvec(.x, mode),
                 paste("Return value from applying `mode\\(\\)` to element 1",
                       "of `.x` does not have class <rvec>."))
})







                           
