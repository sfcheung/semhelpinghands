skip("WIP")
library(testthat)
library(semhelpinghands)

# Example from https://lavaan.ugent.be/tutorial/mediation.html

library(lavaan)
set.seed(1234)
n <- 1000
X <- runif(n) - .5
M <- 0.20*X + rnorm(n)
Y <- 0.17*M + rnorm(n)
Data <- data.frame(X = X, Y = Y, M = M)
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
set.seed(1234)
# One bootstrap replication failed. Kept intentionally.
suppressWarnings(system.time(fit <- sem(model,
                       data = Data,
                       se = "boot",
                       bootstrap = 5000)))

fit_boot <- store_boot_def(fit)
fit_boot <- store_boot_est_std(fit_boot)
head(get_boot_def(fit_boot))
head(get_boot_est_std(fit_boot))
head(lavInspect(fit, "boot"))

plot_boot <- function(object,
                      param,
                      standardized = NULL) {
    if (is.null(standardized)) {
        stop("'standardized' must be TRUE or FALSE.")
      }
    boot_out <- param_find_boot(object = object,
                                param = param,
                                standardized = standardized)
    if (any(is.na(boot_out))) {
        stop("Bootstrap estimates not found or not stored.")
      }
    tmp <- range(boot_out$t)
    tmp <- tmp[2] - tmp[1]
    if (tmp == 0) {
        stop("Identical estimates in all bootstrap samples.")
      }
    parold <- par(mfrow = c(1, 2))
    hist(boot_out$t,
         prob = TRUE,
         col = "grey95",
         main = paste0("Histogram of ",
                       param),
         xlab = param,
         ylab = "Density")
    lines(stats::density(boot_out$t),
          lwd = 4,
          col = "black")
    abline(v = boot_out$t0,
           lwd = 4,
           col = "red",
           lty = "dotted")
    qqnorm(boot_out$t,
           cex = 2,
           col = "black",
           pch = 16,
           main = paste0("QQ-Plot of ",
                  param),
           xlab = param)
    qqline(boot_out$t,
           lwd = 2,
           col = "red",
           lty = "solid")
    par(parold)
  }

param_find_boot <- function(object,
                        param,
                        standardized) {
    boot_t0 <- NA
    boot_t <- NA
    if (standardized) {
        boot_i <- get_boot_est_std(object)
        if (param %in% colnames(boot_i)) {
            i <- match(param, std_names(object))
            boot_t0 <- setNames(lavaan::standardizedSolution(object,
                          se = FALSE)[i, "est.std"], param)
            boot_t <- boot_i[, param, drop = TRUE]
            out <- list(t0 = boot_t0,
                        t = boot_t)
          }
      } else {
        boot_i <- lavaan::lavInspect(object,
                                     what = "boot")
        error_idx <- attr(boot_i, "error.idx")
        if (length(error_idx) != 0) {
            boot_i <- boot_i[-error_idx, ]
          }
        if (param %in% colnames(boot_i)) {
            boot_t0 <- lavaan::coef(object)[param]
            boot_t <- boot_i[, param, drop = TRUE]
          } else {
            boot_i <- get_boot_def(object)
            if (param %in% colnames(boot_i)) {
                boot_t0 <- lavaan::coef(object,
                              type = "user")[param]
                boot_t <- boot_i[, param, drop = TRUE]
              }
          }
      }
    out <- list(t0 = boot_t0,
                t = boot_t)
    return(out)
  }

head(param_find_boot(fit_boot, "ab", standardized = TRUE)$t)
head(param_find_boot(fit_boot, "ab", standardized = FALSE)$t)
head(param_find_boot(fit_boot, "X~~X", standardized = TRUE)$t)
head(param_find_boot(fit_boot, "X~~X", standardized = FALSE)$t)
head(param_find_boot(fit_boot, "c", standardized = TRUE)$t)
head(param_find_boot(fit_boot, "c", standardized = FALSE)$t)

plot_boot(fit_boot, "ab", standardized = TRUE)
plot_boot(fit_boot, "ab", standardized = FALSE)
plot_boot(fit_boot, "X~~X", standardized = TRUE)
plot_boot(fit_boot, "X~~X", standardized = FALSE)
plot_boot(fit_boot, "c", standardized = TRUE)
plot_boot(fit_boot, "c", standardized = FALSE)
plot_boot(fit_boot, "b", standardized = TRUE)
plot_boot(fit_boot, "b", standardized = FALSE)
