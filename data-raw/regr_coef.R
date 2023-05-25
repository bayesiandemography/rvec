
library(coda)
library(dplyr, warn.conflicts = FALSE)

data(line)

line1 <- line[[1]] %>%
    as.data.frame() %>%
    slice(101:200)

line2 <- line[[1]] %>%
    as.data.frame() %>%
    slice(101:200)

regr_coef <- bind_rows(line1, line2) %>%
    tibble() %>%
    mutate(draw = row_number()) %>%
    relocate(draw, .before = everything())

save(regr_coef,
     file = "data/regr_coef.rda",
     compress = "bzip2")

    




