# skip_on_cran()
# skip_if(!interactive(),
#         message = "standardizedSolution_boot_ci not tested if not interactive")

library(testthat)
library(semhelpinghands)

# Example from https://lavaan.ugent.be/tutorial/mediation.html

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
                       bootstrap = 100))

ci_boot <- standardizedSolution_boot_ci(fit, save_boot_est_std = TRUE)

get_std <- function(object) {
    lavaan::standardizedSolution(object)$est.std
  }
# fit2 <- update(fit, se = "none")
fit2 <- sem(model,
            data = Data,
            se = "none",
            bootstrap = 100)
set.seed(1234)
boot_ci_test <- bootstrapLavaan(fit2, R = 100,
                                FUN = get_std)

test_that("Compare boot estimates directly", {
    expect_equal(
        attr(ci_boot, "boot_est_std"),
        boot_ci_test,
        ignore_attr = TRUE
      )
  })
