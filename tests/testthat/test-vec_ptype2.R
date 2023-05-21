
test_that("vec_ptype2 works when x is rvec_chr", {
    expect_identical(vec_ptype2(rvec_chr(matrix("a")), rvec_chr(matrix("b"))),
                     rvec_chr())
    expect_identical(vec_ptype2(rvec_chr(matrix("a")), rvec_dbl(matrix(1))),
                     rvec_chr())
    expect_identical(vec_ptype2(rvec_chr(matrix("a")), rvec_int(matrix(1L))),
                     rvec_chr())
    expect_identical(vec_ptype2(rvec_chr(matrix("a")), rvec_lgl(matrix(TRUE))),
                     rvec_chr())
    expect_identical(vec_ptype2(rvec_chr(matrix("a")), character()),
                     rvec_chr())
    expect_identical(vec_ptype2(rvec_chr(matrix("a")), double()),
                     rvec_chr())
    expect_identical(vec_ptype2(rvec_chr(matrix("a")), integer()),
                     rvec_chr())
    expect_identical(vec_ptype2(rvec_chr(matrix("a")), logical()),
                     rvec_chr())
})

test_that("vec_ptype2 works when x is rvec_dbl", {
    expect_identical(vec_ptype2(rvec_dbl(matrix(1)), rvec_chr(matrix("b"))),
                     rvec_chr())
    expect_identical(vec_ptype2(rvec_dbl(matrix(1)), rvec_dbl(matrix(1))),
                     rvec_dbl())
    expect_identical(vec_ptype2(rvec_dbl(matrix(1)), rvec_int(matrix(1L))),
                     rvec_dbl())
    expect_identical(vec_ptype2(rvec_dbl(matrix(1)), rvec_lgl(matrix(TRUE))),
                     rvec_dbl())
    expect_identical(vec_ptype2(rvec_dbl(matrix(1)), character()),
                     rvec_chr())
    expect_identical(vec_ptype2(rvec_dbl(matrix(1)), double()),
                     rvec_dbl())
    expect_identical(vec_ptype2(rvec_dbl(matrix(1)), integer()),
                     rvec_dbl())
    expect_identical(vec_ptype2(rvec_dbl(matrix(1)), logical()),
                     rvec_dbl())
})

test_that("vec_ptype2 works when x is rvec_int", {
    expect_identical(vec_ptype2(rvec_int(matrix(1L)), rvec_chr(matrix("b"))),
                     rvec_chr())
    expect_identical(vec_ptype2(rvec_int(matrix(1L)), rvec_dbl(matrix(1))),
                     rvec_dbl())
    expect_identical(vec_ptype2(rvec_int(matrix(1L)), rvec_int(matrix(1L))),
                     rvec_int())
    expect_identical(vec_ptype2(rvec_int(matrix(1L)), rvec_lgl(matrix(TRUE))),
                     rvec_int())
    expect_identical(vec_ptype2(rvec_int(matrix(1L)), character()),
                     rvec_chr())
    expect_identical(vec_ptype2(rvec_int(matrix(1L)), double()),
                     rvec_dbl())
    expect_identical(vec_ptype2(rvec_int(matrix(1L)), integer()),
                     rvec_int())
    expect_identical(vec_ptype2(rvec_int(matrix(1L)), logical()),
                     rvec_int())
})

test_that("vec_ptype2 works when x is rvec_lgl", {
    expect_identical(vec_ptype2(rvec_lgl(matrix(TRUE)), rvec_chr(matrix("b"))),
                     rvec_chr())
    expect_identical(vec_ptype2(rvec_lgl(matrix(TRUE)), rvec_dbl(matrix(1))),
                     rvec_dbl())
    expect_identical(vec_ptype2(rvec_lgl(matrix(TRUE)), rvec_int(matrix(1L))),
                     rvec_int())
    expect_identical(vec_ptype2(rvec_lgl(matrix(TRUE)), rvec_lgl(matrix(TRUE))),
                     rvec_lgl())
    expect_identical(vec_ptype2(rvec_lgl(matrix(TRUE)), character()),
                     rvec_chr())
    expect_identical(vec_ptype2(rvec_lgl(matrix(TRUE)), double()),
                     rvec_dbl())
    expect_identical(vec_ptype2(rvec_lgl(matrix(TRUE)), integer()),
                     rvec_int())
    expect_identical(vec_ptype2(rvec_lgl(matrix(TRUE)), logical()),
                     rvec_lgl())
})

test_that("vec_ptype2 works when x is character", {
    expect_identical(vec_ptype2("a", rvec_chr(matrix("b"))),
                     rvec_chr())
    expect_identical(vec_ptype2("a", rvec_dbl(matrix(1))),
                     rvec_chr())
    expect_identical(vec_ptype2("a", rvec_int(matrix(1L))),
                     rvec_chr())
    expect_identical(vec_ptype2("a", rvec_lgl(matrix(TRUE))),
                     rvec_chr())
})

test_that("vec_ptype2 works when x is double", {
    expect_identical(vec_ptype2(1, rvec_chr(matrix("b"))),
                     rvec_chr())
    expect_identical(vec_ptype2(1, rvec_dbl(matrix(1))),
                     rvec_dbl())
    expect_identical(vec_ptype2(1, rvec_int(matrix(1L))),
                     rvec_dbl())
    expect_identical(vec_ptype2(1, rvec_lgl(matrix(TRUE))),
                     rvec_dbl())
})

test_that("vec_ptype2 works when x is integer", {
    expect_identical(vec_ptype2(1L, rvec_chr(matrix("b"))),
                     rvec_chr())
    expect_identical(vec_ptype2(1L, rvec_dbl(matrix(1))),
                     rvec_dbl())
    expect_identical(vec_ptype2(1L, rvec_int(matrix(1L))),
                     rvec_int())
    expect_identical(vec_ptype2(1L, rvec_lgl(matrix(TRUE))),
                     rvec_int())
})

test_that("vec_ptype2 works when x is logical", {
    expect_identical(vec_ptype2(TRUE, rvec_chr(matrix("b"))),
                     rvec_chr())
    expect_identical(vec_ptype2(TRUE, rvec_dbl(matrix(1))),
                     rvec_dbl())
    expect_identical(vec_ptype2(TRUE, rvec_int(matrix(1L))),
                     rvec_int())
    expect_identical(vec_ptype2(TRUE, rvec_lgl(matrix(TRUE))),
                     rvec_lgl())
})





                                         
