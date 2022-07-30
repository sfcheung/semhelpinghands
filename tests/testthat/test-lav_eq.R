# skip_on_cran()
# skip_if(!interactive(),
#         message = "standardizedSolution_boot_ci not tested if not interactive")

library(testthat)
library(semhelpinghands)

options(width = 132)

library(lavaan)

data(HolzingerSwineford1939)
mod <-
"
visual  =~ x1 + v2*x2 + v2*x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
"
fit <- cfa(model = mod, data = HolzingerSwineford1939)

model_gp <-
"
visual  =~ x1 + c(v2, v2)*x2 + c(v2, v3)*x3
textual =~ x4 + x5 + x6
speed   =~ x7 + c(v8, v8)*x8 + c(v9, v9)*x9
"
fit_gp <- cfa(model = model_gp, data = HolzingerSwineford1939, group = "school",
              meanstructure = TRUE)

fit_hist <- record_history(fit)
fit_gp_hist <- record_history(fit_gp)

# plot(fit_hist, params = c("v2", "textual=~x6"),
#             last_n = 10,
#             orientation = "vertical")

test_that("record_history: With equality constraints", {
    expect_equal(
        unlist(as.data.frame(fit_hist[nrow(fit_hist), -c(1:2)])),
        as.vector(coef(fit)),
        ignore_attr = TRUE,
        tolerance = 1e-5
      )
    expect_equal(
        unlist(as.data.frame(fit_gp_hist[nrow(fit_gp_hist), -c(1:2)])),
        as.vector(coef(fit_gp)),
        ignore_attr = TRUE,
        tolerance = 1e-5
      )
  })
