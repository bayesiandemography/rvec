
library(BayesRates)
library(dplyr)
library(tidyr)

res <- smooth_age(nevent_df = nz_divorces_2020,
                  py_df = nz_population_2020,
                  byvar = "sex",
                  age_width_df = nz_age_width_df,
                  age_min = 15)

divorce <- res %>%
    components() %>%
    select(age, sex, .probability) %>%
    unnest(.probability) %>%
    mutate(draw = rep(1:1000, times = nrow(nz_divorces_2020))) %>%
    mutate(rate = 1000 * .probability) %>%
    select(age, sex, draw, rate)

save(divorce, file = "data/divorce.rda", compress = "bzip2")
