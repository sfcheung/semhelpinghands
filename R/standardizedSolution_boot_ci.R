#' @title Bootstrap CIs for Standardized Solution
#'
#' @description It receives a [lavaan::lavaan-class] object fitted with
#'   bootstrapping standard errors requested and forms the
#'   confidence intervals for the standardized solution.
#'
#' @return The output of [lavaan::standardizedSolution()], with
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
#'  solution are saved. If saved, they will be stored in the attribute `boot_est_std`.
#'  Default is `TRUE`.
#'
#' @param force_run If `TRUE`, will skip checks and run models without checking
#'                  the estimates.
#'                  For internal use. Default is
#'                  `FALSE`.
#'
#' @param boot_delta_ratio The ratio of (a) the distance of the bootstrap
#'                         confidence limit from the point estimate to (b)
#'                         the distance of the delta-method limit from the
#'                         point estimate.
#'
#' @param ... Other arguments to be passed to [lavaan::standardizedSolution()].
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lavaan::standardizedSolution()]
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
                                         save_boot_est_std = TRUE,
                                         force_run = FALSE,
                                         boot_delta_ratio = FALSE,
                                         ...) {
    if (!inherits(object, "lavaan")) {
        stop("The object must be a lavaan-class object.")
      }
    boot_est0 <- try(lavaan::lavTech(object, "boot"), silent = TRUE)
    if (inherits(boot_est0, "try-error")) {
        stop("Bootstrapping estimates not found. Was se = 'boot'?")
      }
    if (!force_run) {
      }
    std_args <- list(...)
    ptable <- lavaan::parameterTable(object)
    p_free <- ptable$free > 0
    p_est  <- ptable$est
    boot_est <- split(boot_est0, row(boot_est0))
    out_all <- t(sapply(boot_est, std_i,
                        p_est = p_est,
                        p_free = p_free,
                        object = object,
                        type = type,
                        std_args = std_args))
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
    if (boot_delta_ratio) {
        tmp1 <- abs(out_final$boot.ci.lower - out_final$est.std) /
                                 abs(out_final$ci.lower - out_final$est.std)
        tmp2 <- abs(out_final$boot.ci.upper - out_final$est.std) /
                                 abs(out_final$ci.upper - out_final$est.std)
        tmp1[is.infinite(tmp1) | is.nan(tmp1)] <- NA
        tmp2[is.infinite(tmp2) | is.nan(tmp2)] <- NA
        out_final$ratio.lower <- tmp1
        out_final$ratio.upper <- tmp2
      }
    class(out_final) <- class(out)
    if (save_boot_est_std) {
        attr(out_final, "boot_est_std") <- out_all
      }
    out_final
  }

# Generate the function for bootstrapping.
#' @noRd

std_i <- function(est_i, p_est, p_free, object, std_args, type) {
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

#' @noRd

check_std_i <- function(object, type, std_args) {
    # Work-in-progress
    # Not used for now
    # Do one bootstrap with bootstrapLavaan(),
    #   with est and std
    # Put est as boot, and see if std_i can reproduce std
    fct <- function(fit, std_type, std_args) {
        args0 <- utils::modifyList(std_args,
                                   list(object = fit,
                                        type = std_type,
                                        se = FALSE,
                                        zstat = FALSE,
                                        pvalue = FALSE,
                                        ci = FALSE,
                                        output = "data.frame"))
        list(coef = lavaan::coef(fit),
             est.std = do.call(lavaan::standardizedSolution, args0)$est.std)
      }
    object_noboot <- lavaan::update(object, se = "none")
    out_test <- lavaan::bootstrapLavaan(object_noboot,
                                        R = 1,
                                        type = "ordinary",
                                        FUN = fct,
                                        warn = -1L,
                                        std_type = type,
                                        std_args = std_args)
    object_test <- object
    object_test@boot$coef <- out_test[[1]]
    ptable <- lavaan::parameterTable(object)
    boot_std_test <- std_i(est_i = out_test[[1]],
                           p_est = ptable$est,
                           p_free = ptable$free > 0,
                           object = object,
                           std_args = std_args,
                           type = type)
    if (!isTRUE(all.equal(boot_std_test, out_test[[2]]))) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
