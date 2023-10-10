
## 'weighted_fun_no_rvec' -----------------------------------------------------

test_that("'weighted_fun_no_rvec' works with valid inputs - no NA", {
    x <- 1:10
    wt  <- 11:20
    expect_equal(weighted_fun_no_rvec(x = x,
                                      wt = wt,
                                      na_rm = FALSE,
                                      fun = matrixStats::weightedMean),
                 weighted.mean(x = x, w = wt))
})

test_that("'weighted_fun_no_rvec' works with valid inputs - with NA", {
    x <- c(1:10, NA)
    wt  <- c(NA, 11:20)
    expect_equal(weighted_fun_no_rvec(x = x,
                                      wt = wt,
                                      na_rm = FALSE,
                                      fun = matrixStats::weightedMean),
                 weighted.mean(x = x, w = wt))
    expect_equal(weighted_fun_no_rvec(x = x,
                                      wt = wt,
                                      na_rm = TRUE,
                                      fun = matrixStats::weightedMean),
                 weighted.mean(x = x, w = wt, na.rm = TRUE))
})

test_that("'weighted_fun_no_rvec' works with valid inputs - with Inf", {
    x <- c(1:10, Inf)
    wt  <- c(Inf, 11:20)
    expect_equal(weighted_fun_no_rvec(x = x,
                                      wt = wt,
                                      na_rm = TRUE,
                                      fun = matrixStats::weightedMean),
                 weighted.mean(x = x, w = wt))
})

test_that("'weighted_fun_no_rvec' works with valid inputs - wt is NULL", {
    x <- 1:10
    wt  <- NULL
    expect_equal(weighted_fun_no_rvec(x = x,
                                      wt = wt,
                                      na_rm = TRUE,
                                      fun = matrixStats::weightedMean),
                 weighted.mean(x = x, w = rep(1, 10)))
})

test_that("'weighted_fun_no_rvec' works with valid inputs - inputs zero length", {
    x <- double()
    wt  <- double()
    expect_equal(weighted_fun_no_rvec(x = x,
                                      wt = wt,
                                      na_rm = TRUE,
                                      fun = matrixStats::weightedMean),
                 NaN)
})

test_that("'weighted_fun_no_rvec' throws expected error with different lengths", {
    x <- 1:10
    wt <- 1:5
    expect_error(weighted_mean(x = x, wt = wt),
                 "`x` and `wt` have different lengths")
})


## 'weighted_fun_has_rvec' ----------------------------------------------------

test_that("'weighted_fun_has_rvec' works with valid inputs - x is rvec, w is rvec", {
    mx <- matrix(1:20, nr = 10)
    mw <- matrix(101:120, nr = 10)
    x <- rvec(mx)
    wt  <- rvec(mw)
    ans_obtained <- weighted_fun_has_rvec(x = x,
                                          wt = wt,
                                          na_rm = FALSE,
                                          fun_vec = matrixStats::weightedMean,
                                          fun_mat = matrixStats::colWeightedMeans)
    ans_expected <- rvec(list(c(weighted.mean(x = mx[,1], w = mw[,1]),
                                weighted.mean(x = mx[,2], w = mw[,2]))))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'weighted_fun_has_rvec' works with valid inputs - x is rvec, w is not", {
    m <- matrix(1:20, nr = 10)
    x <- rvec(m)
    wt  <- 11:20
    ans_obtained <- weighted_fun_has_rvec(x = x,
                                          wt = wt,
                                          na_rm = FALSE,
                                          fun_vec = matrixStats::weightedMean,
                                          fun_mat = matrixStats::colWeightedMeans)
    ans_expected <- rvec(list(c(weighted.mean(x = m[,1], w = wt),
                                weighted.mean(x = m[,2], w = wt))))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'weighted_fun_has_rvec' works with valid inputs - x is not rvec, w is rvec", {
    x <- 1:10
    mw <- matrix(101:120, nr = 10)
    wt  <- rvec(mw)
    ans_obtained <- weighted_fun_has_rvec(x = x,
                                          wt = wt,
                                          na_rm = FALSE,
                                          fun_vec = matrixStats::weightedMean,
                                          fun_mat = matrixStats::colWeightedMeans)
    ans_expected <- rvec(list(c(weighted.mean(x = x, w = mw[,1]),
                                weighted.mean(x = x, w = mw[,2]))))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'weighted_fun_has_rvec' works with valid inputs - x is rvec, w is NULL", {
    m <- matrix(1:20, nr = 10)
    x <- rvec(m)
    wt  <- NULL
    ans_obtained <- weighted_fun_has_rvec(x = x,
                                          wt = wt,
                                          na_rm = FALSE,
                                          fun_vec = matrixStats::weightedMean,
                                          fun_mat = matrixStats::colWeightedMeans)
    ans_expected <- rvec(list(c(weighted.mean(x = m[,1], w = rep(1, 10)),
                                weighted.mean(x = m[,2], w = rep(1, 10)))))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'weighted_fun_has_rvec' works with valid inputs - x, w zero length", {
    x <- rvec_dbl()
    wt  <- rvec_dbl()
    ans_obtained <- weighted_fun_has_rvec(x = x,
                                          wt = wt,
                                          na_rm = FALSE,
                                          fun_vec = matrixStats::weightedMean,
                                          fun_mat = matrixStats::colWeightedMeans)
    ans_expected <- rvec_dbl(NaN)
    expect_equal(ans_obtained, ans_expected)
})






## 'weighted_mean' ------------------------------------------------------------

test_that("weighted_mean works with no rvecs", {
    set.seed(0)
    x <- c(rnorm(10), NA)
    wt <- c(NA, runif(10))
    expect_equal(weighted_mean(x = x, wt = wt),
                 weighted.mean(x = x, w = wt))
})

test_that("weighted_mean works with x rvec", {
    set.seed(0)
    m <- matrix(-(101:122), nc = 2)
    x <- rvec(m)
    wt <- c(NA, runif(10))
    expect_equal(weighted_mean(x = x, wt = wt),
                 rvec(matrix(c(weighted.mean(x = m[,1], w = wt),
                               weighted.mean(x = m[,2], w = wt)),
                             nrow = 1)))
})

test_that("weighted_mean works with wt rvec", {
    set.seed(0)
    x <- 1:10
    mw <- matrix(101:120, nr = 10)
    wt  <- rvec(mw)
    expect_equal(weighted_mean(x = x, wt = wt),
                 rvec(list(c(weighted.mean(x = x, w = mw[,1]),
                             weighted.mean(x = x, w = mw[,2])))))
})


## 'weighted_mad' -------------------------------------------------------------

test_that("weighted_mad works with no rvecs", {
    set.seed(0)
    x <- rnorm(10)
    wt <- runif(10)
    expect_equal(weighted_mad(x = x, wt = wt),
                 matrixStats::weightedMad(x = x, w = wt))
})

test_that("weighted_mad works with rvecs", {
    set.seed(0)
    m <- matrix(-(101:122), nc = 2)
    x <- rvec(m)
    wt <- runif(11)
    expect_equal(weighted_mad(x = x, wt = wt),
                 rvec(matrix(c(matrixStats::weightedMad(x = m[,1], w = wt),
                               matrixStats::weightedMad(x = m[,2], w = wt)),
                             nrow = 1)))
})

test_that("weighted_mad works with wt rvec", {
    set.seed(0)
    x <- 1:10
    mw <- matrix(101:120, nr = 10)
    wt  <- rvec(mw)
    expect_equal(weighted_mad(x = x, wt = wt),
                 rvec(matrix(c(matrixStats::weightedMad(x = x, w = mw[,1]),
                               matrixStats::weightedMad(x = x, w = mw[,2])),
                             nrow = 1)))
})


## 'weighted_median' ----------------------------------------------------------

test_that("weighted_median works with no rvecs", {
    set.seed(0)
    x <- c(rnorm(10), NA)
    wt <- c(NA, runif(10))
    expect_equal(weighted_median(x = x, wt = wt),
                 matrixStats::weightedMedian(x = x, w = wt))
})

test_that("weighted_median works with rvecs", {
    set.seed(0)
    m <- matrix(-(101:122), nc = 2)
    x <- rvec(m)
    wt <- c(NA, runif(10))
    expect_equal(weighted_median(x = x, wt = wt),
                 rvec(matrix(c(matrixStats::weightedMedian(x = m[,1], w = wt),
                               matrixStats::weightedMedian(x = m[,2], w = wt)),
                             nrow = 1)))
})

test_that("weighted_meidan works with wt rvec", {
    set.seed(0)
    x <- 1:10
    mw <- matrix(101:120, nr = 10)
    wt  <- rvec(mw)
    expect_equal(weighted_median(x = x, wt = wt),
                 rvec(matrix(c(matrixStats::weightedMedian(x = x, w = mw[,1]),
                               matrixStats::weightedMedian(x = x, w = mw[,2])),
                             nrow = 1)))
})


## 'weighted_sd' -------------------------------------------------------------

test_that("weighted_sd works with no rvecs", {
    set.seed(0)
    x <- rnorm(10)
    wt <- runif(10)
    expect_equal(weighted_sd(x = x, wt = wt),
                 matrixStats::weightedSd(x = x, w = wt))
})

test_that("weighted_sd works with rvecs", {
    set.seed(0)
    m <- matrix(-(101:122), nc = 2)
    x <- rvec(m)
    wt <- runif(11)
    expect_equal(weighted_sd(x = x, wt = wt),
                 rvec(matrix(c(matrixStats::weightedSd(x = m[,1], w = wt),
                               matrixStats::weightedSd(x = m[,2], w = wt)),
                             nrow = 1)))
})

test_that("weighted_median works with wt rvec", {
    set.seed(0)
    x <- 1:10
    mw <- matrix(101:120, nr = 10)
    wt  <- rvec(mw)
    expect_equal(weighted_sd(x = x, wt = wt),
                 rvec(matrix(c(matrixStats::weightedSd(x = x, w = mw[,1]),
                               matrixStats::weightedSd(x = x, w = mw[,2])),
                             nrow = 1)))
})


## 'weighted_var' -------------------------------------------------------------

test_that("weighted_var works with no rvecs", {
    set.seed(0)
    x <- rnorm(10)
    wt <- runif(10)
    expect_equal(weighted_var(x = x, wt = wt),
                 matrixStats::weightedVar(x = x, w = wt))
})

test_that("weighted_var works with rvecs", {
    set.seed(0)
    m <- matrix(-(101:122), nc = 2)
    x <- rvec(m)
    wt <- runif(11)
    expect_equal(weighted_var(x = x, wt = wt),
                 rvec(matrix(c(matrixStats::weightedVar(x = m[,1], w = wt),
                               matrixStats::weightedVar(x = m[,2], w = wt)),
                             nrow = 1)))
})

test_that("weighted_var works with wt rvec", {
    set.seed(0)
    x <- 1:10
    mw <- matrix(101:120, nr = 10)
    wt  <- rvec(mw)
    expect_equal(weighted_var(x = x, wt = wt),
                 rvec(matrix(c(matrixStats::weightedVar(x = x, w = mw[,1]),
                               matrixStats::weightedVar(x = x, w = mw[,2])),
                             nrow = 1)))
})




