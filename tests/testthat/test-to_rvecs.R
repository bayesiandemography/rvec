
## 'to_rvecs.data.frame' ------------------------------------------------------

test_that("'to_rvecs.data.frame' works with data frame, nrow > 0, draw supplied, types not supplied", {
    df <- data.frame(g = c("a", "a", "b", "b"),
                     draw = c(1:2, 1:2),
                     value = 1:4)
    ans_obtained <- to_rvecs(df, draw = draw)
})
    







                   
                   
