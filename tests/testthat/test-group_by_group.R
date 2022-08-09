# skip_on_cran()
# skip_if(!interactive(),
#         message = "standardizedSolution_boot_ci not tested if not interactive")

library(testthat)
library(semhelpinghands)
set.seed(838914)
n <- 500
mod_pop <-
"
y1 ~ .2*x1 + .3*x2 + .1*x3
y2 ~ .2*x1 + .2*x3
y3 ~ .2*y2 + .2*x2
"
library(lavaan)
dat <- simulateData(model = mod_pop, sample.nobs = n)
dat$gp <- sample(c("gp1", "gp2"), n, replace = TRUE)
mod1 <-
"
y1 ~ x1 + x2 + x3
y2 ~ x1 + x3
y3 ~ y2 + x2
"
fit1 <- sem(mod1, dat, group = "gp")
mod2 <-
"
y1 ~ x1 + x2 + x3
y2 ~ x3
"
fit2 <- sem(mod2, dat, group = "gp")

mod3 <-
"
y1 ~ x1 + c(a1, a2)*x2 + c(b1, b2)*x3
y2 ~ x3
a1b1 := a1*b1
"
fit3 <- sem(mod3, dat, group = "gp")
est3 <- parameterEstimates(fit3)

est1 <- parameterEstimates(fit1)
est1gp1 <- est1[est1$group == 2, ]
est1gp2 <- est1[est1$group == 1, ]
std1 <- standardizedSolution(fit1)
std1gp1 <- std1[std1$group == 2, ]
std1gp2 <- std1[std1$group == 1, ]


out1 <- group_by_groups(fit1, col_names = c("est", "se"))
out1_chk <- group_by_models(list(gp1 = est1gp1,
                                 gp2 = est1gp2),
                            col_names = c("est", "se"))
identical(out1, out1_chk[, c(1, 2, 3, 5, 4, 7, 6)])

out1std <- group_by_groups(std1, col_names = c("est.std", "se"),
                           group_labels = lavInspect(fit1, "group.label"))
out1std_chk <- group_by_models(list(gp1 = std1gp1,
                                 gp2 = std1gp2),
                            col_names = c("est.std", "se"))
identical(out1std, out1std_chk[, c(1, 2, 3, 5, 4, 7, 6)])


test_that("Test against know results", {
    expect_equal(out1, out1_chk[, c(1, 2, 3, 5, 4, 7, 6)])
    expect_equal(out1std, out1std_chk[, c(1, 2, 3, 5, 4, 7, 6)])
  })
