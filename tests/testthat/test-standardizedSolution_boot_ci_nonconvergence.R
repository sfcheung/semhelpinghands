skip("WIP")
# skip_on_cran()
# skip_if(!interactive(),
#         message = "standardizedSolution_boot_ci not tested if not interactive")

library(testthat)
library(semhelpinghands)

# Example from https://lavaan.ugent.be/tutorial/mediation.html

library(lavaan)
set.seed(1234)
mod0 <-
"
x1 ~~ .3*x2
x1 ~~ .3*x3
x2 ~~ .1*x3
"
dat <- simulateData(model = mod0,
                    sample.nobs = 50,
                    empirical = TRUE)
mod <- "f1 =~ x1 + x2 + x3"
set.seed(1234)
# One bootstrap replication failed. Kept intentionally.
suppressWarnings(system.time(fit <- sem(mod,
                       data = dat,
                       se = "boot",
                       bootstrap = 100)))

ci_boot <- standardizedSolution_boot_ci(fit, save_boot_est_std = TRUE)
