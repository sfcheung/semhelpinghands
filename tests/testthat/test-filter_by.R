library(testthat)
library(semhelpinghands)

n <- 100
set.seed(4325)
mod_pop <-
"
y1 ~ .2*x1 + .3*x2 + .1*x3
y2 ~ .2*x1 + .2*x3
y3 ~ .2*y2 + .2*x2
"
library(lavaan)
dat <- simulateData(model = mod_pop, sample.nobs = n)
mod <-
"
y1 ~ x1 + x2 + x3
y2 ~ x1 + x3
y3 ~ y2 + x2
"
fit <- sem(mod, dat)

dat$gp <- sample(c("gp1", "gp2"), n, replace = TRUE)
fit_gp <- sem(mod, dat, group = "gp")

est <- parameterEstimates(fit)
ptable <- parameterTable(fit)
std <- standardizedSolution(fit)

est_gp <- parameterEstimates(fit_gp)
ptable_gp <- parameterTable(fit_gp)
std_gp <- standardizedSolution(fit_gp)

test_that("filter_by", {
    expect_identical(filter_by(est, op = "~"),
                     est[est$op == "~", ])
    expect_identical(filter_by(est, lhs = "y2"),
                     est[est$lhs == "y2", ])
    expect_identical(filter_by(est, rhs = "x3"),
                     est[est$rhs == "x3", ])
    expect_identical(filter_by(est_gp, rhs = "x3"),
                     est_gp[est_gp$rhs == "x3", ])
    expect_identical(filter_by(est_gp, group = 2),
                     est_gp[est_gp$group == 2, ])
    expect_identical(filter_by(est_gp, group = "gp2", fit = fit_gp),
                     est_gp[est_gp$group == 2, ])
    expect_identical(filter_by(est, op = c("~", "~~"),
                                    rhs = c("y2")),
                     est[est$op %in% c("~", "~~") &
                         est$rhs == "y2", ])
    expect_identical(filter_by(std_gp, group = 2),
                     std_gp[std_gp$group == 2, ])
  })
