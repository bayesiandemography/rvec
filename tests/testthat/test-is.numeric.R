
test_that("'is.numeric' true only with rvec_dbl and rvec_int", {
    expect_false(is.numeric(rvec_chr()))
    expect_true(is.numeric(rvec_dbl()))
    expect_true(is.numeric(rvec_int()))
    expect_false(is.numeric(rvec_lgl()))
})
