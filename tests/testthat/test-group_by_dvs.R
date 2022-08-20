# skip_on_cran()
# skip_if(!interactive(),
#         message = "standardizedSolution_boot_ci not tested if not interactive")

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
est <- parameterEstimates(fit)

dat$gp <- sample(c("gp1", "gp2"), n, replace = TRUE)
fit_gp <- sem(mod, dat, group = "gp")
est_gp <- parameterEstimates(fit_gp)

est_grouped <- group_by_dvs(fit)
est_grouped_gp <- group_by_dvs(fit_gp)
est_grouped_est <- group_by_dvs(est)
est_grouped_gp_est <- group_by_dvs(est_gp)

est_grouped_ans <- structure(list(iv = c("x1", "x2", "x3", "y2"), est_y1 = c(0.205762144483444,
0.380568486727359, 0.162192077222678, NA), est_y2 = c(0.149078624898469,
NA, 0.230428486323587, NA), est_y3 = c(NA, 0.295393228140568,
NA, 0.0443298135482514)), row.names = c(NA, -4L), class = "data.frame")

est_grouped_gp_ans <- structure(list(iv = c("x1", "x1", "x2", "x2", "x3", "x3", "y2",
"y2"), group = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L), est_y1 = c(0.20954050563671,
0.258194633693387, 0.242853996647183, 0.483540060686917, 0.245745030252482,
0.120904062683077, NA, NA), est_y2 = c(0.0356595509573325, 0.225628736501381,
NA, NA, 0.216741145006698, 0.260859641018626, NA, NA), est_y3 = c(NA,
NA, 0.372224774343152, 0.29070267615556, NA, NA, 0.0962380871786314,
-0.00327088762324575)), row.names = c(NA, -8L), class = "data.frame")

est_grouped_ivs <- group_by_ivs(fit)
est_grouped_gp_ivs <- group_by_ivs(fit_gp)

est_grouped_ivs_ans <- structure(list(dv = c("y1", "y2", "y3"), est_x1 = c(0.205762144483444,
0.149078624898469, NA), est_x2 = c(0.380568486727359, NA, 0.295393228140568
), est_x3 = c(0.162192077222678, 0.230428486323587, NA), est_y2 = c(NA,
NA, 0.0443298135482514)), class = c("est_table", "data.frame"
), row.names = c(NA, 3L), v_ind = 1, ivs = c("x1", "x2", "x3",
"y2"), grouped = FALSE, group_first = FALSE, by_ivs = TRUE)

est_grouped_gp_ivs_ans <- structure(list(dv = c("y1", "y2", "y3"), est_x1.1 = c(0.20954050563671,
0.0356595509573325, NA), est_x1.2 = c(0.258194633693387, 0.225628736501381,
NA), est_x2.1 = c(0.242853996647183, NA, 0.372224774343152),
    est_x2.2 = c(0.483540060686917, NA, 0.29070267615556), est_x3.1 = c(0.245745030252482,
    0.216741145006698, NA), est_x3.2 = c(0.120904062683077, 0.260859641018626,
    NA), est_y2.1 = c(NA, NA, 0.0962380871786314), est_y2.2 = c(NA,
    NA, -0.00327088762324575)), class = c("est_table", "data.frame"
), row.names = c(NA, 3L), v_ind = 1, gp_ind = 2, ivs = c("x1",
"x1", "x2", "x2", "x3", "x3", "y2", "y2"), gps = c(1L, 2L, 1L,
2L, 1L, 2L, 1L, 2L), grouped = TRUE, group_first = FALSE, by_ivs = TRUE)

test_that("Test against know results", {
    expect_true(
        all.equal(est_grouped,
                  est_grouped_ans,
                  check.attributes = FALSE)
      )
  })
test_that("Test against know results", {
    expect_true(
        all.equal(est_grouped_gp,
                  est_grouped_gp_ans,
                  check.attributes = FALSE)
      )
  })


test_that("Test against know results", {
    expect_true(
        all.equal(est_grouped_ivs,
                  est_grouped_ivs_ans,
                  check.attributes = FALSE)
      )
  })
test_that("Test against know results", {
    expect_true(
        all.equal(est_grouped_gp_ivs,
                  est_grouped_gp_ivs_ans,
                  check.attributes = FALSE)
      )
  })

test_that("Different inputs", {
    expect_true(identical(est_grouped, est_grouped_est))
    expect_true(identical(est_grouped_gp, est_grouped_gp_est))
  })