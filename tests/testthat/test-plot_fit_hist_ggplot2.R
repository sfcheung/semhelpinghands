skip("WIP")
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
fit_history <- record_history(fit)

library(ggplot2)
pars1 <- c("y1~x1", "y2~x1", "y1~~y1")

to_long <- function(x, pars) {
    if (missing(pars)) {
        pars <- colnames(x)[-c(1:2)]
      }
    out <- lapply(pars, function(y) {
        out0 <- data.frame(par = y, x[, c("iteration", "f", y)])
        colnames(out0)[ncol(out0)] <- "est"
        out0
      })
    do.call(rbind, out)
  }

tmp <- to_long(fit_history, pars1)
tmp

p <- ggplot(data = tmp,
             aes(x = est, y = f)) +
      geom_path() +
      geom_point(size = 2) +
      facet_wrap(factor(tmp$par, levels = pars), scales = "free_x")
p
head(tmp[tmp$iteration <= 5, ])
plot(fit_history, params = "y1~x1", orientation = "vertical")

tmp <- to_long(fit_history)
p <- ggplot(data = tmp,
             aes(x = est, y = f)) +
      geom_path() +
      geom_point(size = 2) +
      facet_wrap(factor(tmp$par, levels = colnames(fit_history)[-c(1:2)]), scales = "free_x")
p
tail(fit_history, 5)
