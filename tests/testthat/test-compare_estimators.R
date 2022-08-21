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

test_that("compare_estimators: sanity check", {
    expect_error(compare_estimators(1, estimator = c("GLS")))
    expect_error(compare_estimators(fit, estimator = c(1, 2)))
    expect_error(compare_estimators(fit))
  })

fit2 <- sem(model, data = dat, fixed.x = TRUE)
fit2_more <- compare_estimators(fit2, estimator = c("GLS", "MLR", "ML"))
fit2_se_ratio <- se_ratios(fit2_more, reference  = "ML")

test_that("se_ratios", {
    expect_identical(fit2_se_ratio$se_GLS / fit2_se_ratio$se_ML,
                     fit2_se_ratio$ratio_GLS)
    expect_identical(fit2_se_ratio$se_MLR / fit2_se_ratio$se_ML,
                     fit2_se_ratio$ratio_MLR)
    expect_identical(fit2_se_ratio$se_ML / fit2_se_ratio$se_ML,
                     fit2_se_ratio$ratio_ML)
  })
