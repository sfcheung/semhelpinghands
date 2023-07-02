#' @title Plot Models on a Chi-Squares-vs-Dfs Graph
#'
#' @description Plot models on a graph
#' with model chi-square against model
#' the degrees of freedom, with lines
#' for equal fit measures.
#'
#' @details This function plots models
#' based on their model chi-squares and
#' model degrees of freedoms.It can
#' also add lines for chi-square-df
#' combination with equal values on
#' selected fit measures. Currently
#' supports CFI, TLI, and RMSEA.
#'
#' @return Return a [ggplot2::ggplot()]
#' output that can be further modified.
#'
#' @param ... The [lavaan::lavaan-class]
#' objects to be plotted. Can also be a
#' named list of the
#' [lavaan::lavaan-class] objects. If it
#' is as list, it must be named and the
#' names will be used in the plot.
#'
#' @param fit_measure A length-one
#' character vector of the fit measures
#' to use to plot the lines. Only
#' supports `"cfi"` (the default),
#' `"tli"`, and `"rmsea"`.
#'
#' @param fit_values A numeric vector
#' of the values of the fit measure
#' used to plot the lines. The default
#' values are `c(.90, .95)` for `"cfi"`
#' and `"tli"`, and
#' `c(.00, .02, .05, .08)` for
#' `"rmsea"`.
#'
#' @param line_size The size of the
#' lines. Default is 1.
#'
#' @param label_size The size of the
#' model names. Default is 8.
#'
#' @param point_size The size of the
#' point representing a model. Default
#' is 2.
#'
#' @param position_dodge Offsetting the
#' label of a model from the point.
#' Default is .5. Used by
#' [ggrepel::geom_label_repel()].
#'
#' @param include_model_values If `TRUE`
#' , the values of the models on
#' `fit_measure` will be added to
#' `fit_values`. Default is `FALSE.`
#'
#' @param include_baseline If `TRUE`,
#' the baseline model is included in the
#' plot. Default is `FALSE`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lavaan::fitMeasures()]
#'
#'
#' @examples
#'
#'
#' library(lavaan)
#'
#' # From the help page of modificationIndices
#'
#' HS.model <- '
#'  visual  =~ x1 + x2 + x3
#'  textual =~ x4 + x5 + x6
#'  speed   =~ x7 + x8 + x9
#' '
#'
#' fit <- cfa(HS.model, data = HolzingerSwineford1939)
#' modindices(fit, sort = TRUE, op = "=~")
#'
#' fit2 <- update(fit, add = "visual =~ x9")
#' fit3 <- update(fit, add = "textual =~ x3\nvisual =~ x7")
#'
#' models <- list(Initial = fit,
#'                Model_2 = fit2,
#'                Model_3 = fit3)
#' fit_cfi <- sapply(models, fitMeasures, fit.measures = "cfi")
#' fit_tli <- sapply(models, fitMeasures, fit.measures = "tli")
#' fit_rmsea <- sapply(models, fitMeasures, fit.measures = "rmsea")
#'
#' # Supply the models as arguments
#' plot_models_fm(fit, fit2, fit3)
#'
#' # Plot lines for selected values on a fit measure (CFI by default)
#' plot_models_fm(fit, fit2, fit3, fit_values = c(.90, .925, .95, fit_cfi))
#'
#' # Plot the models' values on the fit measures
#' plot_models_fm(fit, fit2, fit3, include_model_values = TRUE)
#'
#' # Supply the models as a named list
#' plot_models_fm(list(A = fit, B = fit2, C = fit3),
#'                fit_values = c(.90, .925, .95))
#'
#' # Plot the models, fit measure set to TLI
#' plot_models_fm(fit, fit2, fit3, fit_measure = "tli")
#' plot_models_fm(fit, fit2, fit3, fit_measure = "tli",
#'                fit_values = c(.90, .925, .95, fit_tli))
#' plot_models_fm(fit, fit2, fit3, fit_measure = "tli",
#'                include_model_values = TRUE)
#'
#' # Plot the models, fit measure set to RMSEA
#' plot_models_fm(fit, fit2, fit3, fit_measure = "rmsea")
#' plot_models_fm(fit, fit2, fit3, fit_measure = "rmsea",
#'                include_model_values = TRUE)
#'
#' @export
#' @importFrom rlang .data

plot_models_fm <- function(...,
                           fit_measure = c("cfi", "tli", "rmsea"),
                           fit_values,
                           line_size = 1,
                           label_size = 8,
                           point_size = 5,
                           position_dodge = .5,
                           include_model_values = FALSE,
                           include_baseline = FALSE) {
    fit_measure <- match.arg(fit_measure)
    if (missing(fit_values)) {
        if (fit_measure %in% c("cfi", "tli")) {
            fit_values <- c(.90, .95)
          }
        if (fit_measure %in% "rmsea") {
            fit_values <- c(.00, .02, .05, .08)
          }
      }
    fits <- list(...)
    if ((length(fits) == 1) && is.list(fits)) {
        fits <- fits[[1]]
        if (is.null(names(fits))) {
            stop("If a list is supplied, the list must be a named list.")
          }
        fits_names <- names(fits)
      } else {
        fits_names <- sapply(substitute(list(...))[-1], deparse)
        names(fits) <- fits_names
      }
    fits_class <- sapply(fits, inherits, what = "lavaan")
    if (!all(fits_class)) {
        stop("At least one of the object is not of the class 'lavaan'.")
      }
    if (include_model_values) {
        fit_values <- c(fit_values,
                        sapply(fits, lavaan::fitMeasures,
                               fit.measures = fit_measure))
      }
    fit_measures_final <- c("chisq", "df", "baseline.chisq", "baseline.df",
                            fit_measure)
    fms <- as.data.frame(t(sapply(fits, lavaan::fitMeasures,
                                    fit.measures = fit_measures_final)))
    fms$model <- fits_names
    if (!all.equal(max(fms$baseline.chisq), min(fms$baseline.chisq)) ||
        !all.equal(max(fms$baseline.df), min(fms$baseline.df))) {
        stop("The models must have the same baseline model.")
      }
    fms_base <- data.frame(chisq = fms[1, "baseline.chisq"],
                          df = fms[1, "baseline.df"],
                          model = "Baseline")
    if (include_baseline) {
        fms_1 <- rbind(fms_base, fms[c("chisq", "df", "model")])
      } else {
        fms_1 <- fms[c("chisq", "df", "model")]
      }
    p <- ggplot2::ggplot(fms_1, ggplot2::aes(x = .data$df, y = .data$chisq)) +
            ggplot2::geom_point(size = point_size) +
            ggrepel::geom_label_repel(
                      ggplot2::aes(label = .data$model),
                      position = ggplot2::position_dodge(position_dodge)) +
            ggplot2::ylab("Model Chi-square") +
            ggplot2::xlab("Model df") +
            ggplot2::theme_grey()
    # CFI
    if (fit_measure == "cfi") {
        b_cfi <- 1
        a_cfi <- fms_base$chisq - fms_base$df
        a_cfis <- a_cfi * (1 - fit_values)
        k <- length(fit_values)
        dat_cfi <- data.frame(a = a_cfis,
                              b = rep(b_cfi, k),
                              CFI = sprintf("%4.3f", fit_values))
        df_lo <- min(fms_1$df)
        chisq_lo <- range(a_cfis) + df_lo
        df_hi <- max(fms_1$df)
        chisq_hi <- range(a_cfis) + df_hi
        p <- p + ggplot2::geom_abline(data = dat_cfi,
                                 ggplot2::aes(slope = .data$b,
                                     intercept = .data$a,
                                     linetype = .data$CFI,
                                     color = .data$CFI),
                                 size = line_size) +
                 ggplot2::expand_limits(y = c(chisq_lo, chisq_hi))
        return(p)
      }
    # TLI
    if (fit_measure == "tli") {
        k <- length(fit_values)
        b_tli <- (1 - fit_values) * (fms_base$chisq - fms_base$df) +
                    fms_base$df
        b_tli <- b_tli / fms_base$df
        dat_tli <- data.frame(a = rep(0, k),
                              b = b_tli,
                              TLI = sprintf("%4.3f", fit_values))
        df_lo <- min(fms_1$df)
        chisq_lo <- range(b_tli) * df_lo
        df_hi <- max(fms_1$df)
        chisq_hi <- range(b_tli) * df_hi
        p <- p + ggplot2::geom_abline(data = dat_tli,
                                 ggplot2::aes(slope = .data$b,
                                     intercept = .data$a,
                                     linetype = .data$TLI,
                                     color = .data$TLI),
                                  size = line_size) +
                 ggplot2::expand_limits(y = c(chisq_lo, chisq_hi))
        return(p)
      }
    # RMSEA
    if (fit_measure == "rmsea") {
        if (lavaan::lavInspect(fits[[1]], "options")$estimator %in%
              c("ML", "PML", "FML")) {
            n_rmsea <- lavaan::lavInspect(fits[[1]], "ntotal")
          } else {
            n_rmsea <- lavaan::lavInspect(fits[[1]], "ntotal") -
                         lavaan::lavInspect(fits[[1]], "ngroups")
          }
        a_rmsea <- 0
        df_rmseas <- fms_base$df
        b_rmseas <- (fit_values^2 * fms_base$df * n_rmsea + fms_base$df) /
                        df_rmseas
        k <- length(fit_values)
        dat_rmsea <- data.frame(a = rep(a_rmsea, k),
                              b = b_rmseas,
                              RMSEA = sprintf("%5.3f", fit_values))
        df_lo <- min(fms_1$df)
        chisq_lo <- range(b_rmseas) * df_lo
        df_hi <- max(fms_1$df)
        chisq_hi <- range(b_rmseas) * df_hi
        p <- p + ggplot2::geom_abline(data = dat_rmsea,
                                 ggplot2::aes(slope = .data$b,
                                     intercept = .data$a,
                                     linetype = .data$RMSEA,
                                     color = .data$RMSEA),
                                 size = line_size) +
                 ggplot2::expand_limits(y = c(chisq_lo, chisq_hi))
        return(p)
      }
  }
