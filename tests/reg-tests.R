## Check the hashing function against 'digest'

library(digest)

set.seed(1)
x <- rnorm(1000)

(d1 <- cacher:::sha1(x))
(d2 <- digest(x, "sha1"))

stopifnot(identical(d1, d2))

x <- 1:1000

write(x, file = "testdata")

(d1 <- cacher:::sha1_file("testdata"))
(d2 <- digest("testdata", algo = "sha1", file = TRUE))

stopifnot(identical(d1, d2))
