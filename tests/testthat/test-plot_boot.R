skip("WIP")
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
head(get_boot_def(fit_boot))
head(get_boot_est_std(fit_boot))
head(lavInspect(fit, "boot"))

fitgp_boot <- store_boot_def(fitgp)
fitgp_boot <- store_boot_est_std(fitgp_boot)
head(get_boot_def(fitgp_boot))
head(get_boot_est_std(fitgp_boot))
head(lavInspect(fitgp, "boot"))


plot_boot <- function(object,
                      param,
                      standardized = NULL,
                      nclass = NULL,
                      hist_color = "lightgrey",
                      hist_linewidth = 1,
                      density_line_type = "solid",
                      density_line_color = "blue",
                      density_line_linewidth = 2,
                      est_line_type = "dotted",
                      est_line_color = "red",
                      est_line_linewidth = 2,
                      qq_dot_size = 2,
                      qq_dot_color = "black",
                      qq_dot_pch = 16,
                      qq_line_linewidth = 2,
                      qq_line_color = "black",
                      qq_line_linetype = "solid"
                      ) {
    if (is.null(standardized)) {
        stop("'standardized' must be TRUE or FALSE.")
      }
    boot_out <- param_find_boot(object = object,
                                param = param,
                                standardized = standardized)
    if (any(is.na(boot_out))) {
        stop("Bootstrap estimates not found or not stored.")
      }
    t0 <- boot_out$t0
    t <- boot_out$t
    tmp <- range(t)
    tmp <- tmp[2] - tmp[1]
    if (tmp == 0) {
        stop("Identical estimates in all bootstrap samples.")
      }
    # From plot.boot()
    if (is.null(nclass)) {
        nclass <- min(max(ceiling(length(t) / 25), 10), 100)
      }
    # From plot.boot()
    #  Calculate the breakpoints for the histogram so that one of them is
    #  exactly t0.
    rg <- range(t)
    rg[1] <- min(rg[1], t0)
    rg[2] <- max(rg[2], t0)
    rg <- rg + 0.05 * c(-1, 1) * diff(rg)
    lc <- diff(rg) / (nclass - 2)
    n1 <- ceiling((t0 - rg[1]) / lc)
    n2 <- ceiling((rg[2] - t0) / lc)
    bks <- t0 + (-n1:n2) * lc
    parold <- par(mfrow = c(1, 2))
    parold2 <- par(lwd = hist_linewidth)
    hist(t,
         probability = TRUE,
         breaks = bks,
         col = hist_color,
         main = paste0("Histogram of ",
                       param),
         xlab = param,
         ylab = "Density")
    par(parold2)
    lines(stats::density(t),
          lwd = density_line_linewidth,
          col = density_line_color,
          lty = density_line_type)
    abline(v = t0,
           lwd = est_line_linewidth,
           col = est_line_color,
           lty = est_line_type)
    qqnorm(t,
           cex = qq_dot_size,
           col = qq_dot_color,
           pch = qq_dot_pch,
           main = paste0("Normal QQ-Plot of ",
                  param),
           xlab = "Quantiles of Standard Normal",
           ylab = param)
    qqline(t,
           lwd = qq_line_linewidth,
           col = qq_line_color,
           lty = qq_line_linetype)
    par(parold)
    invisible(object)
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

plot_boot(fit_boot, "ab", standardized = TRUE, hist_linewidth = .1)

plot_boot(fitgp_boot, "ab1", standardized = TRUE)
plot_boot(fitgp_boot, "ab2", standardized = FALSE)
plot_boot(fitgp_boot, "X~~X", standardized = TRUE)
plot_boot(fitgp_boot, "X~~X", standardized = FALSE)
plot_boot(fitgp_boot, "c1", standardized = TRUE)
plot_boot(fitgp_boot, "c2", standardized = FALSE)
plot_boot(fitgp_boot, "b2", standardized = TRUE)
plot_boot(fitgp_boot, "b1", standardized = FALSE)
