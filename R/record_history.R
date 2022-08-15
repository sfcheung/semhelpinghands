#' @title Record the Minimization History
#'
#' @description Record the minimization history when a model is fitted
#'  by [lavaan::lavaan()] or its wrappers (e.g., [lavaan::sem()] or
#'  [lavaan::cfa()]).
#'
#' @details It records the minimization history when a model is fitted
#'  by [lavaan::lavaan()] or its wrappers (e.g., [lavaan::sem()] or
#'  [lavaan::cfa()]). The recorded history can then be plotted or
#'  displayed, for visualizing how the estimates
#'  of free parameters is found.
#'
#'  It will refit the model by the update method of
#'  [lavaan::lavaan-class], setting `se = "none"` and `test =
#'  "standard"` because they have no impact on the minimization
#'  process.
#'
#'  This and related functions are adapted from the package `semunpack`.
#'  The version in this package will be revised to be an
#'  advanced version intended for diagnostic purpose in real studies.
#'
#' @return
#' A `fit_history`-class object with a plot method.
#'
#' @param object A [lavaan-class] object.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' # Adapted from the example for CFA in lavaan::cfa().
#' # Using only two of the factors
#' library(lavaan)
#' HS.model <-
#' '
#' visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' '
#' fit <- cfa(HS.model, data = HolzingerSwineford1939)
#' # Refit the model with the history recorded
#' fit_h <- record_history(fit)
#' fit_h
#' plot(fit_h, params = c("visual=~x2", "visual=~x3",
#'                        "visual~~textual"),
#'             last_n = 10)
#' plot(fit_h, params = c("visual=~x2", "visual=~x3",
#'                        "visual~~textual"),
#'             last_n = 10,
#'             orientation = "vertical")
#' @importFrom lavaan coef
#' @export
#'
#' @order 1

record_history <- function(object) {
    # call_org <- getCall(fit)
    cap <- utils::capture.output(fit1 <- lavaan::update(object,
                                 control = list(trace = 1),
                                 se = "none",
                                 test = "standard"))
    k <- lavaan::lavInspect(fit1, "iterations")
    capmat <- out2matrix(out = cap[1:(k + 1)],
                         pnames = names(lavaan::coef(fit1)),
                         fit = fit1)
    attr(capmat, "fit_recorded") <- fit1
    attr(capmat, "original_call") <- stats::getCall(object)
    attr(capmat, "original_output") <- cap
    class(capmat) <- c("fit_history", class(capmat))
    capmat
  }

#' @noRd

out2matrix <- function(out, pnames, fit) {
    out1 <- lapply(out, gsub, pattern = ":", replacement = " ")
    out1 <- lapply(out1, function(x) as.numeric(
                      scan(text = x,
                          what = "",
                          quiet = TRUE)
                    ))
    out1 <- do.call(rbind, out1)
    out1a <- out1[, 1:2]
    out1b <- lav_par_expand(out1[, -c(1:2)], fit = fit)
    out1 <- cbind(out1a, out1b)
    if (!missing(pnames)) {
        colnames(out1) <- c("iteration", "f", pnames)
      }
    as.data.frame(out1)
  }

#' @noRd

plot_history_param <- function(param,
                               fit_history,
                               last_n = -1,
                               orientation = "vertical",
                               ...) {
    history_matrix_tmp <- fit_history
    if (last_n > 0) {
        history_matrix_tmp <- fit_history[
                                    seq(
                                        nrow(history_matrix_tmp) - last_n,
                                        nrow(history_matrix_tmp)
                                      ), ]
      }
    if (orientation == "vertical") {
        x <- history_matrix_tmp[, param]
        y <- history_matrix_tmp[, "f"]
        xlab <- param
        ylab <- "Discrepancy Function Value"
      } else {
        y <- history_matrix_tmp[, param]
        x <- history_matrix_tmp[, "f"]
        ylab <- param
        xlab <- "Discrepancy Function Value"
      }
    plot_args <- list(x = x,
                      y = y,
                      type = "b",
                      xlab = xlab,
                      ylab = ylab,
                      cex = 2,
                      cex.lab = 1.5,
                      lwd = 2)
    plot_args1 <- utils::modifyList(plot_args,
                                    list(...))
    do.call(plot, plot_args1)
    # plot(x, y,
    #      type = "b",
    #      xlab = xlab,
    #      ylab = ylab,
    #      cex = 2,
    #      cex.lab = 1.5,
    #      lwd = 2,
    #      ...)
  }

#' @param x A `fit_history` class object, the output of [record_history()].
#' @param params The names of parameters to be plotted. Must be the
#'               names of one or more columns in `x`.
#' @param last_n The lass `n` iterations to be plotted. Default is -1,
#'               plotting all iterations.
#' @param orientation The orientation of the plot. Either `"horizontal"`
#'               (the default) or `"vertical"`.
#' @param ... Additional arguments. To be passed to [plot.default()]
#'
#' @export
#' @describeIn record_history The plot method for the output of
#'                            [record_history()].
#' @order 2
#' @importFrom graphics axTicks axis mtext par title
plot.fit_history <- function(x,
                             params,
                             last_n = -1,
                             orientation = c("horizontal", "vertical"),
                             ...) {
    fit_history <- x
    orientation <- match.arg(orientation)
    p <- length(params)
    par_old <- par(no.readonly = TRUE)
    on.exit(par(par_old))
    if (last_n < 0) {
        ln0 <- 1
      } else {
        ln0 <- nrow(fit_history) - last_n + 1
      }
    fit_history0 <- fit_history[seq(ln0, nrow(fit_history)), ]
    if (orientation == "vertical") {
        if (p > 1) {
            xaxt0 <- c("s",
                      rep("s", p - 2),
                      "s")
            yaxt0 <- c("s",
                      rep("n", p - 2),
                      "n")
            mai0 <- c(list(    c(1,  1, .1, .1)),
                      rep(list(c(1, .1, .1, .1)), p - 2),
                      list(    c(1, .1, .1, .1)))
            par(mfrow = c(1, p),
                oma = c(5, 5, 3, 2))
            ylab0 <- c("Discrepancy Function Value",
                      rep("", p - 1))
          } else {
            xaxt0 <- c("s")
            yaxt0 <- c("s")
            mai0 <- c(list(    c(1,  1, .1, .1)))
            par(mfrow = c(1, p),
                oma = c(5, 5, 3, 2))
            ylab0 <- c("Discrepancy Function Value")
          }
        xlim0 <- NULL
        ylim0 <- range(fit_history0$f)
        xlab0 <- params
      } else {
        if (p > 1) {
            xaxt0 <- c("n",
                      rep("n", p - 2),
                      "s")
            yaxt0 <- c("s",
                      rep("s", p - 2),
                      "s")
            mai0 <- c(list(    c(.1, 5.5, .1, 2)),
                      rep(list(c(.1, 5.5, .1, 2)), p - 2),
                      list(    c(.1, 5.5, .1, 2)))
            par(mfrow = c(p, 1),
                oma = c(5, 1, 3, 0.5))
            xlab0 <- c(rep("", p - 1),
                      "Discrepancy Function Value")
          } else {
            xaxt0 <- c("s")
            yaxt0 <- c("s")
            mai0 <- c(list(    c(.1, 5.5, .1, 2)))
            par(mfrow = c(p, 1),
                oma = c(5, 1, 3, 0.5))
            xlab0 <- c("Discrepancy Function Value")
          }
        xlim0 <- rev(range(fit_history0$f))
        ylim0 <- NULL
        ylab0 <- params
      }
    for (i in seq_len(p)) {
        par(mar = mai0[[i]])
        plot_history_param(param = params[i],
                           fit_history = fit_history0,
                           last_n = -1,
                           orientation = orientation,
                          #  xaxt = xaxt0[i],
                          #  yaxt = yaxt0[i],
                           frame.plot = TRUE,
                           axes = FALSE,
                           xlim = xlim0,
                           ylim = ylim0,
                           xlab = xlab0[i],
                           ylab = ylab0[i],
                           ...)
        if (orientation == "vertical") {
            if (i == 1) {
                title("Minimization History",
                      line = 1,
                      cex.main = 1.5,
                      outer = TRUE)
                tmp <- formatC(axTicks(2), 4, format = "f")
                axis(2, at = axTicks(2), labels = tmp, las = 1)
                mtext(ylab0[i],
                      side = 2,
                      line = 4,
                      cex = .9)
              }
            tmp <- formatC(axTicks(1), 3, format = "f")
            axis(1, at = axTicks(1), labels = tmp, las = 3)
            mtext(xlab0[i],
                  side = 1,
                  line = 3,
                  cex = .9)
          } else {
            tmp <- formatC(axTicks(2), 2, format = "f")
            axis(2, at = axTicks(2), labels = tmp, las = 1)
            if (i == 1) {
                mtext("Minimization History",
                      side = 3,
                      line = 1,
                      cex = 1.5)
              }
            if (i == p) {
                tmp <- formatC(axTicks(1), 4, format = "f")
                axis(1, at = axTicks(1), labels = tmp)
                mtext(xlab0[i],
                      side = 1,
                      line = 3,
                      cex = .9)
              }
          }
      }
    invisible()
  }

#' @param n_iterations The number of iterations to print. Default is 10,
#'                     printing the first 10 iterations (or all iterations, if
#'                     the number of iterations is less than 10).
#' @param digits The number of digits to be displayed. Default is 3.
#' @param ... Optional arguments. To be passed to the print method of data frame.
#' @export
#' @describeIn record_history The print method for the output of
#'                            [record_history()].
#' @order 3

print.fit_history <- function(x, n_iterations = 10, digits = 3, ...) {
  x1 <- as.data.frame(x)
  n_iterations <- min(nrow(x1), n_iterations)
  cat("Original call:\n")
  print(attr(x, "original_call"))
  cat("The number of iterations:", nrow(x1), "\n")
  if (n_iterations == nrow(x)) {
      cat("The minimization history for all iterations:\n")
    } else {
      cat("The minimization history for the first", n_iterations,
          "iterations:\n")
    }
  cnames <- colnames(x)
  out <- x1[seq_len(n_iterations), -1]
  out <- as.data.frame(lapply(out, formatC, digits = digits, format = "f"),
                       check.names = FALSE)
  out1 <- cbind(iteration = x1[seq_len(n_iterations), 1], out)
  colnames(out1) <- cnames
  print(out1, ...)
  #fit_recorded
  invisible(x)
}
