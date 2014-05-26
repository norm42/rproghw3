set.seed(10)
x <- rbinom(10, 10, 0.5)
e <- rnorm(10,0,20)
y <- 0.5 + 2 * x * e
