#' @title Fit Measures By Models
#'
#' @description Groups fit measures
#' into a table with models as columns.
#'
#' @return A data-frame-like object of
#' the class `fit_by_model`.
#'
#' @param object_list A named list of
#' [lavaan-class] objects.
#'
#' @param ... Optional arguments to be
#' passed to
#' [lavaan::fitMeasures()].
#'
#' @author Shu Fai Cheung
#' <https://orcid.org/0000-0002-9871-9448>.
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
#' model1 <-
#' '
#' m ~ a*x
#' y ~ b*m
#' ab := a*b
#' '
#' fit1 <- sem(model1, data = dat, fixed.x = FALSE)
#' model2 <-
#' '
#' m ~ a*x
#' y ~ b*m + x
#' ab := a*b
#' '
#' fit2 <- sem(model2, data = dat, fixed.x = FALSE)
#'
#' fitMeasures_by_models(list(no_direct = fit1,
#'                            direct = fit2)))
#'
#' @export

fitMeasures_by_models <- function(object_list,
                                  ...) {
    output_type <- all_type(object_list)
    if (is.na(output_type)) {
        stop("object_list is invalid. Not of the same types or not of the accepted types.")
      }
    if (is.null(names(object_list))) {
        stop("object_list must be a named list.")
      }
    fm_list <- lapply(object_list,
                      fitMeasures,
                      ...)
    out0 <- lapply(fm_list, function(xx) {
                    data.frame(short_name = names(xx),
                               long_name = rep("", length(xx)),
                               value = xx)
                  })
    `%mergefm%` <- function(x, y) {
                      merge(x, y,
                            by = c("short_name", "long_name"),
                            all = TRUE,
                            sort = FALSE)
                    }
    out <- Reduce(`%mergefm%`, out0)
    colnames(out)[-c(1, 2)] <- names(out0)
    long_names <- lavaan_fit_measures_names(long_names = TRUE)
    long_names <- long_names[names(long_names) %in% out$short_name]
    out <- out[match(names(long_names), out$short_name), ]
    out$long_name <- long_names
    # rownames(out) <- out$name
    class(out) <- c("fit_by_models", class(out))
    return(out)
  }

#' @noRd

lavaan_fit_measures_names <- function(x,
                                      long_names = FALSE) {
    to_long_names <-
      c(npar = "Number of parameters",
        fmin = "Discrepancy function value (fmin)",
        chisq = "Test statistic",
        chisq.scaled = "Test statistic (Scaled)",
        df = "Degrees of freedom",
        df.scaled = "Degrees of freedom (Scaled)",
        pvalue = "P-value",
        pvalue.scaled = "P-value (Scaled)",
        chisq.scaling.factor = "Scaling correction factor",
        baseline.chisq = "Test statistic: Baseline model",
        baseline.df = "Degrees of freedom: Baseline model",
        baseline.chisq.scaled = "Test statistic (Scaled): Baseline model",
        baseline.df.scaled = "Degrees of freedom (Scaled): Baseline model",
        baseline.pvalue = "P-value: Baseline model",
        baseline.pvalue.scaled = "P-value (Scaled): Baseline model",
        baseline.chisq.scaling.factor = "Scaling correction factor: Baseline model",
        cfi = "CFI",
        cfi.scaled = "CFI (Scaled)",
        tli = "TLI",
        tli.scaled = "TLI (Scaled)",
        cfi.robust = "CFI (Robust)",
        tli.robust = "TLI (Robust)",
        nfi = "NFI",
        nfi.scaled = "NFI (Scaled)",
        pnfi = "PNFI",
        pnfi.scaled = "PNFI (Scaled)",
        rfi = "RFI",
        rfi.scaled = "RFI (Scaled)",
        ifi = "IFI",
        ifi.scaled = "IFI (Scaled)",
        rni = "RNI",
        rni.robust = "RNI (Scaled)", # To Check
        logl = "Loglikelihood: User model (H0)",
        scaling.factor.h0 = "Scaling correction factor (H0): MLR correction",
        unrestricted.logl = "Loglikelihood: Unrestricted model (H1)",
        scaling.factor.h1 = "Scaling correction factor (H1): MLR correction",
        aic = "AIC",
        bic = "BIC",
        bic2 = "Sample-size adjusted BIC (SABIC)",
        rmsea = "RMSEA",
        rmsea.scaled = "RMSEA (Scaled)",
        rmsea.ci.level = "RMSEA CI confidence level",
        rmsea.ci.lower = "RMSEA CI: Lower bound",
        rmsea.ci.upper = "RMSEA CI: Upper bound",
        rmsea.ci.lower.scaled = "RMSEA (Scaled) CI: Lower bound",
        rmsea.ci.upper.scaled = "RMSEA (Scaled) CI: Upper bound",
        rmsea.close.h0 = "RMSEA P-Close H0 Value", # To Fix
        rmsea.pvalue = "P-Value RMSEA P-Close", # To Fix
        rmsea.pvalue.scaled = "P-Value RMSEA (Scaled) P-Close", # To Fix
        rmsea.notclose.h0 = "RMSEA P-Not-Close H0 Value", # To Fix
        rmsea.notclose.pvalue = "P-Value RMSEA P-Not-Close", # To Fix
        rmsea.notclose.pvalue.scaled = "P-Value RMSEA (Scaled) P-Not-Close", # To Fix
        rmsea.robust = "RMSEA (Robust)",
        rmsea.ci.lower.robust = "RMSEA (Robust) CI: Lower bound",
        rmsea.ci.upper.robust = "RMSEA (Robust) CI: Upper bound",
        rmsea.pvalue.robust = "P-Value RMSEA (Robust) P-Close", # To Fix
        rmsea.notclose.pvalue.robust = "P-Value RMSEA (Robust) P-Not-Close", # To Fix
        rmr = "RMR",
        rmr_nomean = "RMR (No mean)",
        srmr = "SRMR",
        srmr_nomean = "SRMR (No mean)",
        srmr_within = "SRMR (Within covariance matrix)",
        srmr_between = "SRMR (Between covariance matrix)",
        wrmr = "WRMR",
        cn_05 = "Hoelter Critical N (alpha = .05)",
        cn_01 = "Hoelter Critical N (alpha = .01)",
        gfi = "Goodness of Fit Index (GFI)",
        agfi = "Adjusted Goodness of Fit Index (AGFI)",
        pgfi = "Parsimony Goodness of Fit Index (PGFI)",
        mfi = "McDonald Fit Index (MFI)",
        ecvi = "Expected Cross-Validation Index (ECVI)")
    # Some fit measures not included because they are
    # not printed in the summary.
    if (long_names) return(to_long_names)
  }

