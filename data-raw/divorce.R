
library(readr)

divorce <- read_csv("data-raw/divorce.csv", col_types = "cciid")

save(divorce, file = "data/divorce.rda", compress = "bzip2")
