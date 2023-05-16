
test_that("atomic_to_rvec_chr works", {
    expect_identical(atomic_to_rvec_chr(c(x = "a", y = "b"), n_draw = 2),
                     rvec_chr(matrix(c("a","b"), nr = 2, nc = 2,
                                     dimnames = list(c("x", "y"), NULL))))
})

test_that("atomic_to_rvec_chr works", {
    expect_identical(atomic_to_rvec_chr(c(x = 1, y = 2), n_draw = 2),
                     rvec_chr(matrix(c("1", 2), nr = 2, nc = 2,
                                     dimnames = list(c("x", "y"), NULL))))
})

                                        
