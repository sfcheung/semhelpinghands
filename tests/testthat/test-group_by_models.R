library(testthat)
library(semhelpinghands)
library(lavaan)
dat <- dvs_ivs
dat$gp <- rep(c("gp1", "gp2"), each = nrow(dat) / 2)
mod1 <-
"
y1 ~ x1 + x2 + x3
y2 ~ x1 + x3
y3 ~ y2 + x2
"
fit1 <- sem(mod1, dat, group = "gp")
est1 <- parameterEstimates(fit1)
std1 <- standardizedSolution(fit1)

mod2 <-
"
y1 ~ x1 + x2 + x3
y2 ~ x3
"
fit2 <- sem(mod2, dat, group = "gp")
est2 <- parameterEstimates(fit2)
std2 <- standardizedSolution(fit2)

fit1_nogroup <- sem(mod1, dat)
fit2_nogroup <- sem(mod2, dat)
est1_nogroup <- parameterEstimates(fit1_nogroup)
std1_nogroup <- standardizedSolution(fit1_nogroup)
est2_nogroup <- parameterEstimates(fit2_nogroup)
std2_nogroup <- standardizedSolution(fit2_nogroup)

out1 <- group_by_models(list(model1 = fit1,
                                  model2 = fit2),
                             standardized = TRUE,
                             col_names = c("std.all", "se"))
out1est <- group_by_models(list(model1 = fit1,
                                  model2 = fit2),
                             col_names = c("est", "se"))
out1est_est <- group_by_models(list(model1 = est1,
                                  model2 = est2),
                             col_names = c("est", "se"))
out1_std <- group_by_models(list(model1 = std1,
                                  model2 = std1),
                             col_names = c("est.std", "se"))

out1est_names <- paste0(out1$lhs, out1$op, out1$rhs, ".", out1$group)
out1est_std_names <- paste0(out1_std$lhs, out1_std$op, out1_std$rhs, ".", out1_std$group)
common_names <- intersect(out1est_names, out1est_std_names)

#head(dplyr::filter(out1, op == "~"))

out2 <- group_by_models(list(model1 = fit1_nogroup,
                             model2 = fit2_nogroup),
                             use_standardizedSolution = TRUE,
                             col_names = c("est.std", "se"))
#head(dplyr::filter(out2, op == "~"))

out3 <- group_by_models(list(model1 = fit1,
                                  model2 = fit2),
                             use_standardizedSolution = TRUE,
                             col_names = c("est.std", "se"),
                             group_first = TRUE)
#head(dplyr::filter(out3, op == "~"))
#tail(dplyr::filter(out3, op == "~"))


test_that("Test against know results", {
    expect_identical(colnames(out1),
                     c("lhs", "op", "rhs", "group",
                       "std.all_model1", "std.all_model2",
                       "se_model1", "se_model2"))
    expect_identical(colnames(out2),
                     c("lhs", "op", "rhs",
                       "est.std_model1", "est.std_model2",
                       "se_model1", "se_model2"))
    expect_identical(colnames(out3),
                     c("group", "lhs", "op", "rhs",
                       "est.std_model1", "est.std_model2",
                       "se_model1", "se_model2"))
    tmp1 <- out1[out1$lhs == "y1" & out1$rhs == "x1" & out1$group == 2, 6]
    tmp2 <- std2[std2$lhs == "y1" & std2$rhs == "x1" & std2$group == 2, "est.std"]
    expect_equal(tmp1, tmp2)
    tmp1 <- out2[out2$lhs == "y1" & out2$rhs == "x1", 6]
    tmp2 <- std1_nogroup[std1_nogroup$lhs == "y1" & std1_nogroup$rhs == "x1", "se"]
    expect_equal(tmp1, tmp2)
    tmp1 <- out3[out3$lhs == "y1" & out3$rhs == "x3" & out3$group == 2, 6]
    tmp2 <- std2[std2$lhs == "y1" & std2$rhs == "x3" & std2$group == 2, "est.std"]
    expect_equal(tmp1, tmp2)
  })

test_that("Results from different outputs", {
    expect_identical(colnames(out1est),
                     c("lhs", "op", "rhs", "group",
                       "est_model1", "est_model2",
                       "se_model1", "se_model2"))
    tmp1 <- out1est[out1est$lhs == "y1" & out1est$rhs == "x3" & out1est$group == 2, 7]
    tmp2 <- est1[est1$lhs == "y1" & est1$rhs == "x3" & est1$group == 2, "se"]
    expect_equal(tmp1, tmp2)
  })
