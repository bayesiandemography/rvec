
## 'check_to_rvecs_dup_idx' ---------------------------------------------------

test_that("'check_to_rvecs_idx_dup' throws expected error", {
    idx <- rbind(c(1L, 1L),
                 c(1L, 1L),
                 c(1L, 2L),
                 c(3L, 1L))
    data <- data.frame(id1 = "a",
                       id2 = c(1, 1, 1, 2),
                       dr = c(1L, 1L, 2L, 1L),
                       val = 4:1)
    expect_error(check_to_rvecs_idx_dup(idx = idx,
                                        data = data,
                                        colnum_draw = c(dr = 3L),
                                        colnums_id = c(id1 = 1L, id2 = 2L)),
                 "Multiple rows with the following values:")
})
