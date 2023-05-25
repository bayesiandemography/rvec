
test_that("'rep' works with valid inputs", {
    m <- matrix(1:6, nrow = 3)
    x <- rvec(m)
    expect_identical(rep(x, times = 2),
                     rvec(rbind(m, m)))
    expect_identical(rep(x, length.out = 4),
                     rvec(rbind(m, c(1L, 4L))))
    expect_identical(rep(x, each = 2),
                     rvec(matrix(c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L),
                                nrow = 6)))
    expect_identical(rep(x, length.out = 0),
                     rvec(matrix(integer(), nr = 0, nc = 2)))
})

test_that("'rep.int' works with valid inputs", {
    m <- matrix(1:6, nrow = 3)
    x <- rvec(m)
    expect_identical(rep.int(x, times = 2),
                     rvec(rbind(m, m)))
    expect_identical(rep.int(x, times = 0),
                     rvec(matrix(integer(), nr = 0, nc = 2)))
})

test_that("'rep_len' works with valid inputs", {
    m <- matrix(1:6, nrow = 3)
    x <- rvec(m)
    expect_identical(rep_len(x, length.out = 4),
                     rvec(rbind(m, c(1L, 4L))))
    expect_identical(rep_len(x, length.out = 0),
                     rvec(matrix(integer(), nr = 0, nc = 2)))
})


                     
                                
    
    
