library(testthat)
library(semhelpinghands)

dat <- dvs_ivs
library(lavaan)
mod <-
"
y1 ~ x1 + x2 + x3
y2 ~ x1 + x3
y3 ~ y2 + x2
"
fit <- sem(mod, dat)
est <- parameterEstimates(fit)

dat$gp <- rep(c("gp1", "gp2"), each = nrow(dat) / 2)
fit_gp <- sem(mod, dat, group = "gp")
est_gp <- parameterEstimates(fit_gp)

est_grouped <- group_by_dvs(fit)
est_grouped_gp <- group_by_dvs(fit_gp)
est_grouped_est <- group_by_dvs(est)
est_grouped_gp_est <- group_by_dvs(est_gp)

test_that("Test against know results", {
    expect_equal(colnames(est_grouped),
                 c("iv", "est_y1", "est_y2", "est_y3"))
    expect_equal(est_grouped$iv,
                 c("x1", "x2", "x3", "y2"))
    tmp <- est_grouped[, 3]
    expect_equal(sort(tmp[!is.na(tmp)]),
                 sort(est[est$lhs == "y2" & est$op == "~", "est"]),
                 ignore_attr = TRUE)
    tmp <- est_grouped[, 4]
    expect_equal(sort(tmp[!is.na(tmp)]),
                 sort(est[est$lhs == "y3" & est$op == "~", "est"]),
                 ignore_attr = TRUE)
  })

test_that("Test against know results", {
    expect_equal(colnames(est_grouped_gp),
                 c("iv", "group", "est_y1", "est_y2", "est_y3"))
    expect_equal(est_grouped_gp$iv,
                 c("x1", "x1", "x2", "x2", "x3", "x3", "y2", "y2"))
    tmp <- est_grouped_gp[, 4]
    expect_equal(sort(tmp[!is.na(tmp)]),
                 sort(est_gp[est_gp$lhs == "y2" & est_gp$op == "~", "est"]),
                 ignore_attr = TRUE)
  })


test_that("Different inputs", {
    expect_true(identical(est_grouped, est_grouped_est))
    expect_true(identical(est_grouped_gp, est_grouped_gp_est))
  })
