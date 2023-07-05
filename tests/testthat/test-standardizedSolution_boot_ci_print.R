skip_on_cran()
skip_if(!interactive(),
        message = "print method not tested if not interactive")

library(testthat)
library(semhelpinghands)

# Example from https://lavaan.ugent.be/tutorial/mediation.html

library(lavaan)
set.seed(1234)
n <- 1000
X <- runif(n) - .5
M <- 0.20*X + rnorm(n)
Y <- 0.17*M + rnorm(n)
gp <- sample(c("Group1", "Group2"),
             n,
             replace = TRUE)
Data <- data.frame(X = X, Y = Y, M = M, gp = gp)
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
model2 <- ' # direct effect
             Y ~ c(c1, c2)*X
           # mediator
             M ~ c(a1, a2)*X
             Y ~ c(b1, b2)*M
           # indirect effect (a*b)
             a1b1 := a1*b2
             a2b2 := a2*b2
           # total effect
             total1 := c1 + (a1*b1)
             total2 := c2 + (a2*b2)
         '
set.seed(1234)
suppressWarnings(system.time(fit <- sem(model,
                       data = Data,
                       se = "boot",
                       bootstrap = 50)))
set.seed(1234)
suppressWarnings(system.time(fit2 <- sem(model2,
                       data = Data,
                       se = "boot",
                       bootstrap = 50,
                       group = "gp")))

ci_boot <- standardizedSolution_boot_ci(fit, save_boot_est_std = TRUE)
ci_boot2 <- standardizedSolution_boot_ci(fit2, save_boot_est_std = TRUE)

print(ci_boot, nd = 5)
print(ci_boot, output = "text")
print(ci_boot, output = "text", standardized_only = FALSE)

print(ci_boot2, nd = 5)
print(ci_boot2, output = "text")
print(ci_boot2, output = "text", standardized_only = FALSE)



get_std <- function(object) {
    lavaan::standardizedSolution(object)$est.std
  }
# fit2 <- update(fit, se = "none")
fit2 <- sem(model,
            data = Data,
            se = "none",
            bootstrap = 100)
set.seed(1234)
suppressWarnings(boot_ci_test <- bootstrapLavaan(fit2, R = 100,
                                FUN = get_std))

# For lavaan 0.9-13
boot_ci_test_error_idx <- attr(boot_ci_test, "error.idx")
if (!is.null(boot_ci_test_error_idx)) {
    if (length(boot_ci_test_error_idx) > 0) {
        boot_ci_test <- boot_ci_test[-boot_ci_test_error_idx, ]
      }
  }

test_that("Compare boot estimates directly", {
    expect_equal(
        attr(ci_boot, "boot_est_std"),
        boot_ci_test,
        ignore_attr = TRUE
      )
  })
