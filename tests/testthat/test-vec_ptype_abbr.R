
test_that("vec_ptype_abbr works as expected", {
    expect_identical(vec_ptype_abbr(rvec_chr(matrix(1:3, nr = 1))),
                     "rchr<3>")
    expect_identical(vec_ptype_abbr(rvec_dbl(matrix(1:3, nr = 1))),
                     "rdbl<3>")
    expect_identical(vec_ptype_abbr(rvec_int(matrix(1:3, nr = 1))),
                     "rint<3>")
    expect_identical(vec_ptype_abbr(rvec_lgl(matrix(c(1,0,1), nr = 1))),
                     "rlgl<3>")
})
    
