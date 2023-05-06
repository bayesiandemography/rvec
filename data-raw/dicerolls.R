

set.seed(0)

x <- sample(1:6, size = 10, replace = TRUE)
dicerolls <- matrix(FALSE,
                    nrow = 6,
                    ncol = 10,
                    dimnames = list(value = 1:6, roll = 1:10))
dicerolls[cbind(x, 1:10)] <- TRUE

save(dicerolls, file = "data/dicerolls.rda", compress = "bzip2")
