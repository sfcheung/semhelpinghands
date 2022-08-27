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

out2 <- group_by_models(list(model1 = fit1_nogroup,
                             model2 = fit2_nogroup),
                             use_standardizedSolution = TRUE,
                             col_names = c("est.std", "se"))

out3 <- group_by_models(list(model1 = fit1,
                             model2 = fit2),
                             use_standardizedSolution = TRUE,
                             col_names = c("est.std", "se"),
                             group_first = TRUE)

out1_sorted <- sort_by(out1)
out1_sorted2 <- sort_by(out1, by = c("op", "rhs", "lhs"),
                        op_priority = c("~~", "~"))
out2_sorted <- sort_by(out2, number_rows = FALSE)
out3_sorted <- sort_by(out3)

test_that("sort_by", {
    expect_identical(unique(out1_sorted$op), c("~", "~~", "~1"))
    expect_identical(out1_sorted$lhs[1:7], c(rep("y1", 6), "y2"))
    expect_identical(out1_sorted$rhs[1:5], c("x1", "x1", "x2", "x2", "x3"))
    expect_true(all(out1_sorted$group[seq(from = 1, to = nrow(out1_sorted), by = 2)] == 1))
    expect_identical(rownames(out1_sorted), as.character(seq_len(nrow(out1_sorted))))
    expect_identical(unique(out1_sorted2$op), c("~~", "~", "~1"))
    expect_identical(out1_sorted2$lhs[1:7], c(rep("x1", 4), "x2", "x2", "x1"))
    expect_identical(out1_sorted2$rhs[1:5], c("x1", "x1", "x2", "x2", "x2"))
    expect_true(all(out1_sorted2$group[seq(from = 1, to = nrow(out1_sorted), by = 2)] == 1))
    expect_identical(rownames(out1_sorted2), as.character(seq_len(nrow(out1_sorted2))))
    expect_identical(unique(out2_sorted$op), c("~", "~~"))
    expect_identical(out2_sorted$lhs[1:6], c(rep("y1", 3), "y2", "y2", "y3"))
    expect_identical(out2_sorted$rhs[1:5], c("x1", "x2", "x3", "x1", "x3"))
    expect_true(identical(rownames(out2_sorted), as.character(seq_len(nrow(out2_sorted)))))
    expect_identical(unique(out3_sorted$op), c("~", "~~", "~1"))
    expect_identical(out3_sorted$lhs[1:6], c(rep("y1", 3), "y2", "y2", "y3"))
    expect_false(is.unsorted(out3_sorted$group))
  })
