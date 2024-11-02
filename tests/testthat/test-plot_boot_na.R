library(testthat)
library(semhelpinghands)
library(lavaan)

test_that("plot_boot: Better error message", {
set.seed(5478374)
n <- 50
x <- runif(n) - .5
m <- .40 * x + rnorm(n, 0, sqrt(1 - .40))
y <- .30 * m + rnorm(n, 0, sqrt(1 - .30))
dat <- data.frame(x = x, y = y, m = m)
model <-
'
m ~ a*x
y ~ b*m
ab := a*b
'
fit <- sem(model, data = dat, fixed.x = FALSE,
           se = "boot",
           bootstrap = 50)
fit_with_boot_def <- store_boot_def(fit)
expect_error(p <- plot_boot(fit_with_boot_def, "ab", standardized = TRUE))
})
