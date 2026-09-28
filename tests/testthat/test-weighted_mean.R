
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





test_that("weighted summaries align one-draw rvecs in either operand", {
    functions <- c(weighted_mean = "weightedMean", weighted_median = "weightedMedian",
                   weighted_mad = "weightedMad", weighted_var = "weightedVar",
                   weighted_sd = "weightedSd")
    mx <- cbind(c(1, 3, 8, 12), c(9, 2, 5, 4), c(3, 7, 2, 10))
    mw <- cbind(c(1, 2, 4, 1), c(3, 1, 2, 5), c(2, 4, 1, 3))
    for (name in names(functions)) {
        fun <- get(name)
        reference <- getExportedValue("matrixStats", functions[[name]])
        for (single in c("x", "wt")) {
            x <- if (single == "x") mx[, 1L, drop = FALSE] else mx
            wt <- if (single == "wt") mw[, 1L, drop = FALSE] else mw
            expected <- vapply(seq_len(3L), function(j)
                reference(x[, if (single == "x") 1L else j],
                          w = wt[, if (single == "wt") 1L else j]), numeric(1))
            expect_identical(fun(rvec(x), wt = rvec(wt)),
                             rvec_dbl(matrix(expected, nrow = 1L)),
                             info = paste(name, single))
        }
    }
})

test_that("weighted summaries reuse ordinary values across weight draws", {
    functions <- c(weighted_mean = "weightedMean", weighted_median = "weightedMedian",
                   weighted_mad = "weightedMad", weighted_var = "weightedVar",
                   weighted_sd = "weightedSd")
    for (name in names(functions)) {
        fun <- get(name)
        reference <- getExportedValue("matrixStats", functions[[name]])
        for (missing in c(FALSE, TRUE)) for (na_rm in c(FALSE, TRUE)) {
            x <- c(1, 3, 8, 12)
            mw <- cbind(c(1, 2, 4, 1), c(3, 1, 2, 5), c(2, 4, 1, 3))
            if (missing) {
                x[2L] <- NA_real_
                mw[3L, 2L] <- NA_real_
            }
            wt <- rvec(mw)
            expected <- vapply(seq_len(ncol(mw)), function(j)
                reference(x, w = mw[, j], na.rm = na_rm), numeric(1))
            expect_identical(fun(x, wt = wt, na_rm = na_rm),
                             rvec_dbl(matrix(expected, nrow = 1L)),
                             info = paste(name, missing, na_rm))
            expect_identical(x, c(1, if (missing) NA_real_ else 3, 8, 12))
            expect_identical(vctrs::field(wt, "data"), mw)
        }
    }
})
