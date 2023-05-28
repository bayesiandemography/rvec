
library(coda)
library(dplyr, warn.conflicts = FALSE)

data(line)

line1 <- line[[1]][101:200, ] %>%
    as.matrix()

line2 <- line[[2]][101:200, ] %>%
    as.matrix()

reg_post <- rbind(line1, line2) %>%
    t()

colnames(reg_post) <- NULL

save(reg_post,
     file = "data/reg_post.rda",
     compress = "bzip2")

    




