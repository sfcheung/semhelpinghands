#' @title Diagnostics Plots of Bootstrap Estimates in 'lavaan'
#'
#' @description (TBD)
#'
#' @details (TBD)
#'
#' @return Return the original
#' [lavaan::lavaan-class] object
#' invisibly. Called for its side-effect
#' (plotting the graphs).
#'
#' @param object A [lavaan::lavaan-class]
#' object with bootstrap estimates
#' stored. For standardized solution
#' and user-defined parameters, the
#' estimates need to be stored by
#' [store_boot_est_std()] or
#' [store_boot_def()].
#'
#' @param param String. The name of
#' the parameter to be plotted, which
#' should be the name as appeared in
#' a call to `coef()`.
#'
#' @param standardized Logical. Whether
#' the estimates from the standardized
#' solution are to be plotted. Default
#' is `NULL`. This is a required parameter
#' and users need to explicitly set it
#' to `TRUE` or `FALSE`.
#'
#' @param nclass The number of breaks.
#' This argument will be passed to
#' [hist()]. Default is `NULL`.
#'
#' @param hist_color String. The color of the
#' bars in the histogram. It will be
#' passed to [hist()] for the argument
#' `col`. Default is `"lightgrey"`.
#'
#' @param hist_linewidth The width of
#' the borders of the bars in the
#' histogram. Default is 1.
#'
#' @param density_line_type String.
#' The type of the line of the density
#' curve in the histogram. It will be
#' passed to [lines()] for the argument
#' `lty`. Default is
#' `"solid"`.
#'
#' @param density_line_color String.
#' The color of the density curve in
#' the histogram. It will be
#' passed to [lines()] for the argument
#' `col`. Default is `"blue"`.
#'
#' @param density_line_linewidth The width
#' of the density curve in the histogram.
#' It will be
#' passed to [lines()] for the argument
#' `lwd`.
#' Default is 2.
#'
#' @param est_line_type String. The
#' type of the vertical line in the
#' histogram showing the point estimate
#' of the parameter. It will be
#' passed to [abline()] for the argument
#' `lty`. Default is
#' `"dotted"`,
#'
#' @param est_line_color String. The
#' color of the vertical line showing
#' the point estimate in the histogram.
#' It will be
#' passed to [abline()] for the argument
#' `col`.
#' Default is `"red"`.
#'
#' @param est_line_linewidth The width
#' of the vertical line showing the
#' point estimate in the histogram.
#' It will be
#' passed to [hist()] for the argument
#' `lwd`.  Default is 2.
#'
#' @param qq_dot_size The size of the
#' points in the normal QQ-plot.
#' It will be
#' passed to [qqnorm()] for the argument
#' `cex`. Default is 2.
#'
#' @param qq_dot_color String. The color
#' of the points in the normal QQ-plot.
#' It will be
#' passed to [qqnorm()] for the argument
#' `col`.
#' Default is `"black"`.
#'
#' @param qq_dot_pch Numeric. The shape
#' of the points in the normal QQ-plot.
#' It will be
#' passed to [qqnorm()] for the argument
#' `pch`. Default is 16.
#'
#' @param qq_line_linewidth The width
#' of the diagonal line to be drawn in
#' the normal QQ-plot.
#' It will be
#' passed to [qqline()] for the argument
#' `lwd`. Default is 2.
#'
#' @param qq_line_color String. The color
#' of the diagonal line to be drawn in
#' the normal QQ-plot.
#' It will be
#' passed to [qqline()] for the argument
#' `col`.
#' Default is `"black"`.
#'
#' @param qq_line_linetype The type of
#' the diagonal line to be drawn in the
#' normal QQ-plot. Default is `"solid"`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lavaan::fitMeasures()]
#'
#'
#' @examples
#'
#' library(lavaan)
#'
#' data(simple_mediation)
#' mod <-
#' "
#' m ~ a * x
#' y ~ b * m + x
#' ab := a * b
#' "
#' fit <- sem(mod, simple_mediation,
#'            se = "bootstrap",
#'            bootstrap = 50,
#'            iseed = 985714)
#' fit <- store_boot_est_std(fit)
#' fit <- store_boot_def(fit)
#'
#' plot_boot(fit, "a", standardized = TRUE)
#' plot_boot(fit, "ab", standardized = FALSE)
#' plot_boot(fit, "y~x", standardized = FALSE)
#'
#' @importFrom graphics abline hist lines
#' @importFrom stats qqline qqnorm setNames
#' @export

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


# Find and return the stored bootstrap estimates,if available.
#' @noRd

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
