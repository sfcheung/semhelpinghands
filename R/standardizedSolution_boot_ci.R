#' @title Percentile Bootstrap CIs for Standardized Solution
#'
#' @description It receives a [lavaan::lavaan-class] object fitted with
#'   bootstrapping standard errors requested and forms the percentile
#'   confidence intervals for the standardized solution.
#'
#' @return The output of [lavaan::standardizedSolution()], with percentile
#'   bootstrap confidence intervals appended to the right.
#'
#' @param object A [lavaan-class] object, fitted with 'se = "boot"'.
#'
#' @param level The level of confidence of the confidence intervals. Default
#'  is .95.
#'
#' @param type The type of standard estimates. The same argument of
#'  [lavaan::standardizedSolution()], and support all values supported by
#'  [lavaan::standardizedSolution()]. Default is `"std.all"`.
#' 
#' @param save_boot_est_std Whether the bootstrap estimates of the standardized
#'  solution are saved. If saved, will be stored in the attribute `boot_est_std`.
#'  Default is `FALSE`.
#'
#' @param ... Other arguments to be passed to [lavaan::standardizedSolution()].
#'
#' @author Shu Fai Cheung (shufai.cheung@gmail.com)
#'
#' @examples
#'
#' library(lavaan)
#' set.seed(5478374)
#' n <- 50
#' x <- runif(n) - .5
#' m <- .40 * x + rnorm(n, 0, sqrt(1 - .40))
#' y <- .30 * m + rnorm(n, 0, sqrt(1 - .30))
#' dat <- data.frame(x = x, y = y, m = m)
#' model <-
#' '
#' m ~ a*x
#' y ~ b*m
#' ab := a*b
#' '
#'
#' # Should set bootstrap to at least 2000 in real studies
#' fit <- sem(model, data = dat, fixed.x = FALSE,
#'            se = "boot",
#'            bootstrap = 100)
#' summary(fit)
#'
#' standardizedSolution_boot_ci(fit)
#'
#' @export

standardizedSolution_boot_ci <- function(object,
                                         level = .95,
                                         type = "std.all",
                                         save_boot_est_std = FALSE,
                                         ...) {
    if (!inherits(object, "lavaan")) {
        stop("The object must be a lavaan-class object.")
      }
    boot_est0 <- try(lavaan::lavTech(object, "boot"), silent = TRUE)
    if (inherits(boot_est0, "try-error")) {
        stop("Bootstrapping estimates not found. Was se = 'boot'?")
      }
    if (any(lavaan::parameterTable(object)$op %in% "==")) {
        stop("Models with equality constraint(s) not yet supported.")
      }
    std_args <- list(...)
    ptable <- lavaan::parameterTable(object)
    p_free <- ptable$free > 0
    p_est  <- ptable$est
    fct_i <- function(est_i, p_est, p_free) {
        p_est[p_free] <- est_i
        GLIST_i <- lavaan::lav_model_set_parameters(object@Model,
                                                           est_i)@GLIST
        std_args1 <- utils::modifyList(std_args,
                                       list(object = object,
                                            type = type,
                                            est = p_est,
                                            GLIST = GLIST_i,
                                            se = FALSE,
                                            zstat = FALSE,
                                            pvalue = FALSE,
                                            ci = FALSE,
                                            output = "data.frame"))
        do.call(lavaan::standardizedSolution, std_args1)$est.std
      }
    boot_est <- split(boot_est0, row(boot_est0))
    out_all <- t(sapply(boot_est, fct_i, p_est = p_est, p_free = p_free))
    # Could have used boot's method but quantile() is good enough.
    boot_ci <- t(apply(out_all, 2, stats::quantile, probs = c((1 - level) / 2,
                                                        1 - (1 - level) / 2),
                                             na.rm = TRUE))
    colnames(boot_ci) <- c("boot.ci.lower", "boot.ci.upper")
    out <- lavaan::standardizedSolution(object,
                                        type = type,
                                        level = level,
                                        ...)
    out_final <- cbind(out, boot_ci)
    class(out_final) <- class(out)
    if (save_boot_est_std) {
        attr(out_final, "boot_est_std") <- out_all
      }
    out_final
  }
