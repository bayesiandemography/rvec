test_that("single-input summaries preserve values, types, and missingness", {
    for (type in c("dbl", "int", "lgl")) {
        constructor <- get(paste0("rvec_", type))
        for (nr in c(0L, 1L, 4L)) for (nc in c(1L, 3L)) {
            m <- matrix(rep(c(TRUE, FALSE, NA), length.out = nr * nc), nr, nc)
            if (nr > 0L)
                rownames(m) <- paste0("row", seq_len(nr))
            x <- constructor(m)
            original <- vctrs::field(x, "data")
            for (name in c("sum", "prod", "any", "all")) {
                fun <- get(name)
                for (na_rm in c(FALSE, TRUE)) {
                    expected <- vctrs::vec_math(name, vctrs::vec_c(x), na.rm = na_rm)
                    ans <- fun(x, na.rm = na_rm)
                    expect_identical(ans, expected, info = paste(type, nr, nc, name, na_rm))
                    vctrs::field(ans, "data")[1L, 1L] <- NA
                    expect_identical(vctrs::field(x, "data"), original)
                }
            }
        }
    }
})

test_that("summary fallbacks retain multiple-input and named-input behavior", {
    x <- rvec_dbl(matrix(c(1, NA, 3, 4), nrow = 2L))
    y <- rvec_int(matrix(c(2, 4, 6, 8), nrow = 2L))
    for (name in c("sum", "prod", "any", "all")) {
        fun <- get(name)
        for (na_rm in c(FALSE, TRUE)) {
            expect_identical(fun(x, y, na.rm = na_rm),
                             vctrs::vec_math(name, vctrs::vec_c(x, y), na.rm = na_rm))
            expect_identical(fun(x, NULL, na.rm = na_rm), fun(x, na.rm = na_rm))
            expect_identical(fun(label = x[1L], na.rm = na_rm),
                             vctrs::vec_math(name, vctrs::vec_c(label = x[1L]), na.rm = na_rm))
        }
        expect_error(fun(label = x), "Can't merge the outer name")
    }
})

test_that("custom rvec subclasses retain summary dispatch through vec_c", {
    x <- rvec_dbl(matrix(1:6, nrow = 3L))
    class(x) <- c("summary_test", class(x))
    vctrs::s3_register("vctrs::vec_math", "summary_test",
                       function(.fn, .x, ...) "custom")
    withr::defer(rm("vec_math.summary_test",
                   envir = asNamespace("vctrs")$.__S3MethodsTable__.))
    for (name in c("sum", "prod", "any", "all")) {
        fun <- get(name)
        expect_identical(fun(x), vctrs::vec_math(name, vctrs::vec_c(x), na.rm = FALSE))
    }
})
