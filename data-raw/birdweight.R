

set.seed(0)

birdweight <- rbind(chicken = rnorm(n = 10, mean = 2.6, sd = 0.2),
                    duck = rnorm(n = 10, mean = 1.2, sd = 0.15),
                    goose = rnorm(n = 10, mean = 3.4, sd = 0.5))
birdweight <- round(birdweight, 2)

save(birdweight, file = "data/birdweight.rda", compress = "bzip2")
