
test_that("vec_ptype_full works as expected", {
    expect_identical(vec_ptype_full(rvec_chr(matrix(1:3, nr = 1))),
                     "rvec_chr<3>")
    expect_identical(vec_ptype_full(rvec_dbl(matrix(1:3, nr = 1))),
                     "rvec_dbl<3>")
    expect_identical(vec_ptype_full(rvec_int(matrix(1:3, nr = 1))),
                     "rvec_int<3>")
    expect_identical(vec_ptype_full(rvec_lgl(matrix(c(1,0,1), nr = 1))),
                     "rvec_lgl<3>")
})
    
