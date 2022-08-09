skip("WIP")
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

est1 <- parameterEstimates(fit1)
est1gp1 <- est1[est1$group == 1, ]
est1gp2 <- est1[est1$group == 2, ]
std1 <- standardizedSolution(fit1)
std1gp1 <- std1[std1$group == 1, ]
std1gp2 <- std1[std1$group == 2, ]

out1 <- group_by_models(list(gpOne = est1gp1,
                             gpTwo = est1gp2),
                             col_names = c("est", "se"))
#head(dplyr::filter(out1, op == "~"))

out2 <- group_by_models(list(gpOne = std1gp1,
                             gpTwo = std1gp2),
                             standardized = TRUE,
                             col_names = c("est.std", "se"))
#head(dplyr::filter(out2, op == "~"))

out3 <- group_by_models(list(model1 = fit1,
                                  model2 = fit2),
                             standardized = TRUE,
                             col_names = c("std.all", "se"),
                             group_first = TRUE)
#head(dplyr::filter(out3, op == "~"))
#tail(dplyr::filter(out3, op == "~"))

# test_that("Test against know results", {
#     expect_equal(out1, out1_ans)
#     expect_equal(out2, out2_ans)
#     expect_equal(out3, out3_ans)
#   })
