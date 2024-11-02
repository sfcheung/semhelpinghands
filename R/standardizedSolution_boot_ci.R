#' @title Bootstrap CIs for Standardized
#' Solution
#'
#' @description Functions for forming
#' bootstrap confidence intervals
#' for the standardized solution.
#'
#' @details
#'
#' [standardizedSolution_boot_ci()]
#' receives a
#' [lavaan::lavaan-class] object fitted
#' with bootstrapping standard errors
#' requested and forms the confidence
#' intervals for the standardized
#' solution.
#'
#' It works by calling
#'  [lavaan::standardizedSolution()]
#' with the bootstrap estimates
#' of free parameters in each bootstrap sample
#' to compute the standardized estimates
#' in each sample.
#'
#' A more reliable way is to use
#' function like
#' [lavaan::bootstrapLavaan()].
#' Nevertheless, this simple function is
#' good enough for some simple scenarios,
#' and does not require repeating
#' the bootstrapping step.
#'
#' [store_boot_est_std()] computes the
#' standardized solution for each bootstrap
#' sample, stores them the
#' [lavaan::lavaan-class] object, and
#' returns it. These estimates can be used
#' by other functions, such as [plot_boot()],
#' to examine the
#' estimates, without the need
#' to repeat the computation.
#'
#' [get_boot_est_std()] retrieves
#' the bootstrap estimates of the
#' standardized solution stored by
#' [store_boot_est_std()].
#'
#' @return The output of
#' [lavaan::standardizedSolution()],
#' with bootstrap confidence intervals
#' appended to the right, with class
#' set to `std_solution_boot` (since
#' version 0.1.8.4). It has
#' a print method
#' ([print.std_solution_boot()]) that
#' can be used to print the standardized
#' solution in a format similar to
#' that of the printout of
#' the [summary()] of a [lavaan::lavaan-class] object.
#'
#' [store_boot_est_std()] returns
#' the fit object set to
#' `object`, with the bootstrap values
#' of standardized solution in the
#' bootstrap samples, as a matrix,
#' stored in the
#' slot `external` under the name
#' `shh_boot_est_std`.
#'
#' [get_boot_est_std()] returns a matrix
#' of the stored bootstrap estimates
#' of standardized solution. If none is
#' stored, `NULL` is returned.
#'
#' [store_boot_est_std()] is usually used
#' with diagnostic functions such
#' as [plot_boot()].
#'
#' @param object A 'lavaan'-class
#' object, fitted with 'se = "boot"'.
#'
#' @param level The level of confidence
#' of the confidence intervals. Default
#' is .95.
#'
#' @param type The type of standard
#' estimates. The same argument of
#' [lavaan::standardizedSolution()],
#' and support all values supported by
#' [lavaan::standardizedSolution()].
#' Default is `"std.all"`.
#'
#' @param save_boot_est_std Whether the
#' bootstrap estimates of the
#' standardized solution are saved. If
#' saved, they will be stored in the
#' attribute `boot_est_std`. Default is
#' `TRUE`.
#'
#' @param force_run If `TRUE`, will skip
#' checks and run models without
#' checking the estimates. For internal
#' use. Default is `FALSE`.
#'
#' @param boot_delta_ratio The ratio of
#' (a) the distance of the bootstrap
#' confidence limit from the point
#' estimate to (b) the distance of the
#' delta-method limit from the point
#' estimate. Default is `FALSE`.
#'
#' @param boot_ci_type The type of the
#' bootstrapping confidence intervals.
#' Support percentile confidence intervals
#' (`"perc"`, the default) and
#' bias-corrected confidence intervals
#' (`"bc"` or `"bca.simple"`).
#'
#' @param ... Other arguments to be
#' passed to
#' [lavaan::standardizedSolution()].
#'
#' @author Shu Fai Cheung
#' <https://orcid.org/0000-0002-9871-9448>.
#' Originally proposed in an issue at GitHub
#' <https://github.com/simsem/semTools/issues/101#issue-1021974657>,
#' inspired by a discussion at
#' the Google group for lavaan
#' <https://groups.google.com/g/lavaan/c/qQBXSz5cd0o/m/R8YT5HxNAgAJ>.
#' [boot::boot.ci()] is used to form the
#' percentile confidence intervals in
#' this version.
#'
#'
#' @seealso [lavaan::standardizedSolution()], [plot_boot()]
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
#' std <- standardizedSolution_boot_ci(fit)
#' std
#'
#' # Print in a friendly format with only standardized solution
#' print(std, output = "text")
#'
#' # Print in a friendly format with both unstandardized
#' # and standardized solution
#' print(std, output = "text", standardized_only = FALSE)
#'
#' # plot_boot() can be used to examine the bootstrap estimates
#' # of a parameter
#' plot_boot(std, param = "ab")
#'
#' @name standardizedSolution_boot_ci
NULL

#' @rdname standardizedSolution_boot_ci
#' @export

standardizedSolution_boot_ci <- function(object,
                                         level = .95,
                                         type = "std.all",
                                         save_boot_est_std = TRUE,
                                         force_run = FALSE,
                                         boot_delta_ratio = FALSE,
                                         boot_ci_type = c("perc", "bc", "bca.simple"),
                                         ...) {
    if (!inherits(object, "lavaan")) {
        stop("The object must be a lavaan-class object.")
      }
    if (!force_run) {
      }
    out_all <- boot_est_std(object = object,
                            type = type,
                            ...)
    out <- lavaan::standardizedSolution(object,
                                        type = type,
                                        level = level,
                                        ...)
    est_org <- out$est.std
    boot_tmp <- list(t0 = est_org,
                      t = out_all,
                      R = nrow(out_all))
    # Adapted from boot
    boot_ci <- sapply(seq_along(est_org), function(x,
                                                   boot_type = boot_ci_type) {
                          if (isTRUE(all.equal(stats::var(out_all[, x], na.rm = TRUE), 0)) ||
                              all(is.na(out_all[, x]))) {
                              return(c(NA, NA))
                            }
                          # boot::boot.ci(boot_tmp,
                          #       index = x,
                          #       type = "perc",
                          #       conf = level)$percent[4:5]
                          boot_ci_internal(t0 = est_org[x],
                                           t = out_all[, x],
                                           level = level,
                                           boot_type = boot_type)
                        })
    boot_ci <- t(boot_ci)
    colnames(boot_ci) <- c("boot.ci.lower", "boot.ci.upper")
    boot_se <- apply(out_all, 2, stats::sd, na.rm = TRUE, simplify = TRUE)
    boot_se[boot_se < .Machine$double.eps] <- NA
    out_final <- cbind(out, boot_ci, `boot.se` = boot_se)
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
    class(out_final) <- c("std_solution_boot", class(out))
    if (save_boot_est_std) {
        colnames(out_all) <- std_names(object, ...)
        attr(out_final, "boot_est_std") <- out_all
      }
    fit_summary <- lavaan::summary(object)
    attr(out_final, "pe_attrib") <- attributes(fit_summary$pe)
    attr(out_final, "partable") <- lavaan::parameterTable(object)
    attr(out_final, "est") <- lavaan::parameterEstimates(object)
    attr(out_final, "level") <- level
    attr(out_final, "type") <- type
    attr(out_final, "call") <- match.call()
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

#' @title Generate bootstrap estimates
#' @noRd

boot_est_std <- function(object,
                         type,
                         ...) {
    # For lavaan 0.6-13
    # Remove bootstrap replications with error
    boot_est0 <- try(lavaan::lavTech(object, "boot"), silent = TRUE)
    if (inherits(boot_est0, "try-error")) {
        stop("Bootstrapping estimates not found. Was se = 'boot' or 'bootstrap'?")
      }
    boot_error_idx <- attr(boot_est0, "error.idx")
    if (!is.null(boot_error_idx)) {
        if (length(boot_error_idx) > 0) {
            boot_est0 <- boot_est0[-boot_error_idx, ]
          }
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
    return(out_all)
  }

#' @examples
#'
#' # store_boot_est_std() is usually used with plot_boot()
#' # First, store the bootstrap estimates of the
#' # standardized solution
#' fit_with_boot_std <- store_boot_est_std(fit)
#' # Second, plot the distribution of the bootstrap estimates of
#' # standardized 'ab'
#' plot_boot(fit_with_boot_std, "ab", standardized = TRUE)
#'
#' @rdname standardizedSolution_boot_ci
#' @export

store_boot_est_std <- function(object,
                               type = "std.all",
                               force_run = FALSE,
                               ...) {
    if (!inherits(object, "lavaan")) {
        stop("The object must be a lavaan-class object.")
      }
    if (!force_run) {
      }
    out_all <- boot_est_std(object = object,
                            type = type,
                            ...)
    colnames(out_all) <- std_names(object, ...)
    object@external$shh_boot_est_std <- out_all
    object@external$shh_boot_est_std_type <- type
    return(object)
  }


#' @rdname standardizedSolution_boot_ci
#' @export

get_boot_est_std <- function(object) {
    return(object@external$shh_boot_est_std)
  }

#' Generate names for standardized solution
#' @noRd

std_names <- function(object, ...) {
    std <- lavaan::standardizedSolution(object, se = FALSE, ...)
    std$id <- seq_len(nrow(std))
    ptable <- lavaan::parameterTable(object)
    std1 <- merge(std, ptable,
                  all.y = FALSE)
    std1 <- std1[order(std1$id), ]
    std1$lavlabel <- lavaan::lav_partable_labels(std1,
                        blocks = c("group", "level"),
                        group.equal = "",
                        group.partial = "",
                        type = "user")
    return(std1$lavlabel)
  }

#' Internal function to form bootstrap CI
#' @noRd

# Internal function for different types
# of bootstrap CI.
# Only work for one statistic.
# Adapted from manymome

boot_ci_internal <- function(t0,
                             t,
                             level = .95,
                             boot_type = c("perc", "bc", "bca.simple")) {
    boot_type <- match.arg(boot_type)
    out <- switch(boot_type,
                  perc = boot_ci_perc(t0 = t0, t = t, level = level),
                  bc = boot_ci_bc(t0 = t0, t = t, level = level),
                  bca.simple = boot_ci_bc(t0 = t0, t = t, level = level))
    out
  }

# Adapted from manymome
#' @noRd

boot_ci_perc <- function(t0,
                         t,
                         level = .95) {
    boot_tmp <- list(t0 = t0,
                     t = matrix(t, ncol = 1),
                     R = length(t))
    ci <- boot::boot.ci(boot_tmp,
                        type = "perc",
                        conf = level)
    ci$perc[4:5]
  }

# Adapted from manymome
#' @noRd

boot_ci_bc <- function(t0,
                       t,
                       level = .95) {
    # Compute the bias correction
    z0_hat <- stats::qnorm(mean(t < t0,
                                na.rm = TRUE))
    z_alpha_l <- stats::qnorm((1 - level) / 2)
    z_alpha_u <- -z_alpha_l
    # BC percentiles
    bc_a_l <- stats::pnorm(2 * z0_hat + z_alpha_l)
    bc_a_u <- stats::pnorm(2 * z0_hat + z_alpha_u)
    # Corresponding confidence level for percentile CIs
    bc_conf_l <- 1 - bc_a_l * 2
    bc_conf_u <- 1 - (1 - bc_a_u) * 2
    boot_ci_bc_l <- boot_ci_perc(t0 = t0,
                                 t = t,
                                 level = bc_conf_l)
    boot_ci_bc_u <- boot_ci_perc(t0 = t0,
                                 t = t,
                                 level = bc_conf_u)
    c(boot_ci_bc_l[1], boot_ci_bc_u[2])
  }