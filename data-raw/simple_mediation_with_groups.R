# Generate data
library(lavaan)
set.seed(8150511)
n <- 100
x <- rnorm(n, 5, 1)
m <- .28 * x + rnorm(n, 0, sqrt(1 - .28^2))
y <- .25 * m + rnorm(n, 0, sqrt(1 - .25^2))
m <- m * 2 + 10
y <- y * 3 + 5
set.seed(815)
city <- sample(c("City A", "City B"), n, replace = TRUE)
dat <- data.frame(x, m, y, city)
mod <-
"
m ~ x
y ~ m
"
fit <- sem(model = mod, data = dat, fixed.x = FALSE)
fitgp <- sem(model = mod, data = dat, fixed.x = FALSE,
             group = "city")
parameterEstimates(fit)
parameterEstimates(fitgp)
simple_mediation <- dat
usethis::use_data(simple_mediation, overwrite = TRUE)

