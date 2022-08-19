library(lavaan)
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

fit <- sem(model, data = dat, fixed.x = FALSE)
fit_more <- compare_estimators(fit, estimator = c("GLS", "MLR", "ML"))
fit_GLS <- sem(model, data = dat, fixed.x = FALSE, estimator = "GLS")
fit_MLR <- sem(model, data = dat, fixed.x = FALSE, estimator = "MLR")
fit_ML  <- sem(model, data = dat, fixed.x = FALSE, estimator = "ML")


test_that("compare_estimators", {
    expect_identical(coef(fit_more$GLS),
                     coef(fit_GLS))
    expect_identical(coef(fit_more$MLR),
                     coef(fit_MLR))
    expect_identical(coef(fit_more$ML),
                     coef(fit_ML))
  })
