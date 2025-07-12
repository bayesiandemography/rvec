
## 'plot' ---------------------------------------------------------------------

## test_that("plot.rvec_chr works", {
##     set.seed(0)
##     m <- matrix(sample(c(letters[1:10], NA),
##                        prob = 11:1,
##                        size = 1000,
##                        replace = TRUE),
##                 nrow = 10)
##     x <- rvec(m)
##     p <- function() plot(x)
##     vdiffr::expect_doppelganger("plot.rvec_chr", p)
## })

test_that("plot.rvec_chr throws correct error when 'y' supplied", {
    set.seed(0)
    m <- matrix(sample(c(letters[1:10], NA),
                       prob = 11:1,
                       size = 1000,
                       replace = TRUE),
                nrow = 10)
    x <- rvec(m)
    expect_error(plot(x = x, y = 1:10),
                 "Value supplied for `y`.")
})

## test_that("plot.rvec_dbl works", {
##     set.seed(0)
##     m <- matrix(rnorm(1000),
##                 nrow = 10)
##     x <- rvec(m)
##     p <- function() plot(x)
##     vdiffr::expect_doppelganger("plot.rvec_dbl", p)
## })

test_that("plot.rvec_dbl throws correct error when 'y' supplied", {
    set.seed(0)
    m <- matrix(rnorm(1000),
                nrow = 10)
    x <- rvec(m)
    expect_error(plot(x = x, y = 1:10),
                 "Value supplied for `y`.")
})

## test_that("plot.rvec_int works", {
##     set.seed(0)
##     m <- matrix(rpois(1000, lambda = 10),
##                 nrow = 10)
##     x <- rvec(m)
##     p <- function() plot(x)
##     vdiffr::expect_doppelganger("plot.rvec_int", p)
## })

test_that("plot.rvec_int throws correct error when 'y' supplied", {
    set.seed(0)
    m <- matrix(rpois(1000, lambda = 10),
                nrow = 10)
    x <- rvec(m)
    expect_error(plot(x = x, y = 1:10),
                 "Value supplied for `y`.")
})

## test_that("plot.rvec_lgl works", {
##     set.seed(0)
##     m <- matrix(sample(c(TRUE, FALSE), size =1000, prob = c(5, 1), replace = TRUE),
##                 nrow = 10)
##     x <- rvec(m)
##     p <- function() plot(x)
##     vdiffr::expect_doppelganger("plot.rvec_lgl", p)
## })

test_that("plot.rvec_lgl throws correct error when 'y' supplied", {
    set.seed(0)
    m <- matrix(sample(c(TRUE, FALSE), size =1000, prob = c(5, 1), replace = TRUE),
                nrow = 10)
    x <- rvec(m)
    expect_error(plot(x = x, y = 1:10),
                 "Value supplied for `y`.")
})


## 'yval_labels_for_plot_chr' -------------------------------------------------

test_that("yval_labels_for_plot_chr gives correct answer with levels < 15", {
    set.seed(0)
    m <- matrix(sample(c(letters[1:10], NA), size = 1000, replace = TRUE),
                nrow = 10)
    ans_obtained <- yval_labels_for_plot_chr(m)
    tab <- sort(table(m, useNA = "always"), decreasing = TRUE)
    labels <- names(tab)
    yval <- 12L - match(m, labels)
    ans_expected <- list(yval = yval, labels = labels)
    expect_identical(ans_obtained, ans_expected)
})


test_that("yval_labels_for_plot_chr gives correct answer with levels > 15", {
    set.seed(0)
    m <- matrix(sample(c(letters, NA), size = 1000, replace = TRUE),
                nrow = 10)
    ans_obtained <- yval_labels_for_plot_chr(m)
    tab <- sort(table(m, useNA = "always"), decreasing = TRUE)
    labels <- names(tab)[1:14]
    labels <- c(labels, "<Other>")
    m[!(m %in% labels)] <- "<Other>"
    yval <- 16L - match(m, labels)
    ans_expected <- list(yval = yval, labels = labels)
    expect_identical(ans_obtained, ans_expected)
})

          
