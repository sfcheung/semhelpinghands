skip("WIP")
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
# One bootstrap replication failed. Kept intentionally.
suppressWarnings(system.time(fit <- sem(model,
                       data = Data,
                       se = "boot",
                       bootstrap = 50)))

model1 <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
         '
set.seed(1234)
# One bootstrap replication failed. Kept intentionally.
suppressWarnings(system.time(fit1 <- sem(model1,
                       data = Data,
                       se = "boot",
                       bootstrap = 50)))

model2 <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
         '
set.seed(1234)
# One bootstrap replication failed. Kept intentionally.
suppressWarnings(system.time(fit2 <- sem(model2,
                       data = Data,
                       se = "boot",
                       bootstrap = 50)))


model3 <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
         '
set.seed(1234)
# One bootstrap replication failed. Kept intentionally.
suppressWarnings(system.time(fit3 <- sem(model3,
                       data = Data)))

fit_boot_def <- store_boot_def(fit)
fit_boot_def1 <- store_boot_def(fit1)

# Test store_boot_def()

boot_def_test <- get_boot_def(fit_boot_def)
boot_def_test1 <- get_boot_def(fit_boot_def1)

est_org <- coef(fit, type = "user")[colnames(boot_def_test)]
boot_tmp <- list(t0 = est_org,
                  t = boot_def_test,
                  R = nrow(boot_def_test))
# Adapted from boot
boot_ci <- sapply(seq_along(est_org), function(x) {
                      if (all(abs(boot_def_test[, x] -
                                  mean(boot_def_test[, x], na.rm = TRUE)) <
                                  1e-8) ||
                          all(is.na(boot_def_test[, x]))) {
                          return(c(NA, NA))
                        }
                      boot::boot.ci(boot_tmp,
                            index = x,
                            type = "perc",
                            conf = .95)$percent[4:5]
                    })
est <- parameterEstimates(fit)

test_that("store_boot_def", {
    expect_equal(as.vector(t(boot_ci)),
                 unlist(est[7:8, c("ci.lower", "ci.upper")]),
                 ignore_attr = TRUE)
    expect_equal(length(dim(boot_def_test1)),
                 2)
    expect_true(is.null(get_boot_def(store_boot_def(fit2))))
    expect_error(store_boot_def(fit3))
  })
