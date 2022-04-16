skip_on_cran()
skip_if(!interactive(),
        message = "standardizedSolution_boot_ci not tested if not interactive")

library(testthat)
library(semhelpinghands)

# Example from https://lavaan.ugent.be/tutorial/mediation.html
options(width = 132)

library(lavaan)
set.seed(1234)
n <- 1000
X <- runif(n) - .5
M <- 0.20*X + rnorm(n)
Y <- 0.17*M + rnorm(n)
Data <- data.frame(X = X, Y = Y, M = M)
model <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
set.seed(1234)
system.time(fit <- sem(model,
                       data = Data,
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

test_that("Compare boot ci", {
    expect_equal(
        ci_test,
        as.matrix(ci_boot[, c("boot.ci.lower", "boot.ci.upper")]),
        tolerance = .005,
        ignore_attr = TRUE
      )
  })
