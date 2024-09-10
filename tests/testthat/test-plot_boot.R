skip("To be run in an interactive mode")

library(testthat)
library(semhelpinghands)

# Example from https://lavaan.ugent.be/tutorial/mediation.html

library(lavaan)
set.seed(1234)
n <- 100
X <- runif(n) - .5
M <- 0.20*X + rnorm(n)
Y <- 0.17*M + rnorm(n)
GP <- sample(c("GpA", "GpB"), replace = TRUE)
Data <- data.frame(X = 10 * X, Y = Y, M = 20 * M, GP = GP)
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
modelgp <- ' # direct effect
             Y ~ c(c1, c2)*X
           # mediator
             M ~ c(a1, a2)*X
             Y ~ c(b1, b2)*M
           # indirect effect (a*b)
             ab1 := a1*b1
             ab2 := a2*b2
           # total effect
             total1 := c1 + (a1*b1)
             total1 := c2 + (a2*b2)
         '
set.seed(1234)
# One bootstrap replication failed. Kept intentionally.
suppressWarnings(system.time(fit <- sem(model,
                       data = Data,
                       se = "boot",
                       bootstrap = 100)))
suppressWarnings(system.time(fitgp <- sem(modelgp,
                       data = Data,
                       se = "boot",
                       group = "GP",
                       bootstrap = 100)))

fit_boot <- store_boot_def(fit)
fit_boot <- store_boot_est_std(fit_boot)
fitgp_boot <- store_boot_def(fitgp)
fitgp_boot <- store_boot_est_std(fitgp_boot)

plot_boot(fit_boot, "ab", standardized = TRUE)
plot_boot(fit_boot, "ab", standardized = FALSE)
plot_boot(fit_boot, "c", standardized = TRUE)
plot_boot(fit_boot, "c", standardized = FALSE)
plot_boot(fit_boot, "b", standardized = TRUE)
plot_boot(fit_boot, "b", standardized = FALSE)
plot_boot(fit_boot, "ab", standardized = TRUE, hist_linewidth = .1)

plot_boot(fitgp_boot, "ab1", standardized = TRUE)
plot_boot(fitgp_boot, "ab2", standardized = FALSE)
plot_boot(fitgp_boot, "c1", standardized = TRUE)
plot_boot(fitgp_boot, "c2", standardized = FALSE)
plot_boot(fitgp_boot, "b2", standardized = TRUE)
plot_boot(fitgp_boot, "b1", standardized = FALSE)

test_that("Expect errors", {
    expect_error(plot_boot(fit_boot, "X~~X", standardized = TRUE))
    expect_error(plot_boot(fit_boot, "X~~X", standardized = FALSE))
    expect_error(plot_boot(fitgp_boot, "X~~X", standardized = TRUE))
    expect_error(plot_boot(fitgp_boot, "X~~X", standardized = FALSE))
  })

# Support standardizedSolution_boot_ci()

std <- standardizedSolution_boot_ci(fit)
stdgp <- standardizedSolution_boot_ci(fitgp)
coef(fitgp)

# Examine interactively

plot_boot(std, "ab")
plot_boot(fit_boot, "ab", standardized = TRUE)

plot_boot(std, "total")
plot_boot(fit_boot, "total", standardized = TRUE)

plot_boot(stdgp, "M~~M.g2")
plot_boot(fitgp_boot, "M~~M.g2", standardized = TRUE)

plot_boot(stdgp, "M~~M")
plot_boot(fitgp_boot, "M~~M", standardized = TRUE)
