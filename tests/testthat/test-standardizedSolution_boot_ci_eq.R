skip_on_cran()
skip("Work in progress")
skip_if(!interactive(),
        message = "standardizedSolution_boot_ci not tested if not interactive")

library(testthat)
library(semhelpinghands)

# Adapted from https://lavaan.ugent.be/tutorial/syntax2.html
options(width = 132)

library(lavaan)
data(HolzingerSwineford1939)
model <-
"
visual  =~ x1 + v2*x2 + v2*x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
"

set.seed(1234)
system.time(fit <- cfa(model = model,
                       data = HolzingerSwineford1939,
                       se = "boot",
                       bootstrap = 5000,
                       parallel ="snow", ncpus = 8))
ci_boot <- standardizedSolution_boot_ci(fit)

get_std <- function(object) {
    lavaan::standardizedSolution(object)$est.std
  }
fit2 <- update(fit, se = "none")
set.seed(1234)
boot_ci_test <- bootstrapLavaan(fit2, R = 5000,
                                FUN = get_std,
                                parallel = "snow",
                                ncpus = 8)
alpha <- .95
ci_test <- t(apply(boot_ci_test, 2, quantile, probs = c((1 - alpha) / 2,
                   1 - (1 - alpha) / 2),
                   na.rm = TRUE))
cbind(ci_boot[, c("ci.lower", "ci.upper")], ci_test)
test_that("Compare boot ci with equality constraints", {
    expect_equal(
        ci_test,
        as.matrix(ci_boot[, c("boot.ci.lower", "boot.ci.upper")]),
        tolerance = .005,
        ignore_attr = TRUE
      )
  })
