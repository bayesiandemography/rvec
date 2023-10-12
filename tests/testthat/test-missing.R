

## anyNA ----------------------------------------------------------------------

test_that("anyNA works with length > 0", {
    x <- rvec(list(c(1, NA),
                   c(3, 1),
                   c(NaN, Inf)))
    expect_true(anyNA(x))
    expect_false(anyNA(x[2]))
})

test_that("is.na works with length 0", {
    x <- rvec_int()
    expect_false(anyNA(x))
})

          
## is.na ----------------------------------------------------------------------

test_that("is.na works with length > 0", {
    x <- rvec(list(c(1, NA),
                   c(NA, 1),
                   c(NaN, Inf)))
    ans_obtained <- is.na(x)
    ans_expected <- rvec(list(c(FALSE, TRUE),
                              c(TRUE, FALSE),
                              c(TRUE, FALSE)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("is.na works with length 0", {
    x <- rvec_int()
    ans_obtained <- is.na(x)
    ans_expected <- rvec_lgl()
    expect_identical(ans_obtained, ans_expected)
})

## 'na.exclude' ---------------------------------------------------------------

test_that("'na.exclude' works", {
    x <- rvec(rbind(1:3,
                    c(4, NA, 6),
                    7:9,
                    c(NA, NA, NA)))
    ans <- na.exclude(x)
    expect_s3_class(attr(ans, "na.action"), "exclude")
    expect_true(all(attr(ans, "loc") == c(2, 4)))
    expect_identical(length(ans), 2L)
})

    

## 'na.fail' ------------------------------------------------------------------

test_that("na.omit.rvec works", {
    x <- rvec(rbind(1:3,
                    c(4, NA, 6),
                    7:9,
                    c(NA, NA, NA)))
    expect_error(na.fail(x),
                 "missing values in object")
})


## 'na.omit' ------------------------------------------------------------------

test_that("na.omit.rvec works", {
    x <- rvec(rbind(1:3,
                    c(4, NA, 6),
                    7:9,
                    c(NA, NA, NA)))
    ans <- na.omit(x)
    expect_true(all(draws_all(ans == rvec(rbind(1:3, 7:9)))))
})


## 'na_remove_rvec' -----------------------------------------------------------

test_that("'na_remove_rvec' works - contains NAs", {
    x <- rvec(rbind(1:3,
                    c(4, NA, 6),
                    7:9,
                    c(NA, NA, NA)))
    ans <- na_remove_rvec(x, class_loc = "omit")
    expect_s3_class(attr(ans, "na.action"), "omit")
    expect_true(all(attr(ans, "loc") == c(2, 4)))
    expect_identical(length(ans), 2L)
})

test_that("'na_remove_rvec' works - no NAs", {
    x <- rvec(rbind(1:3,
                    7:9))
    ans <- na_remove_rvec(x, class_loc = "omit")
    expect_identical(ans, x)
})

test_that("'na_remove_rvec' works - length 0", {
    x <- rvec_int()
    ans <- na_remove_rvec(x, class_loc = "omit")
    expect_identical(ans, x)
})


              
