#' @title Visualize How CFI and TLI Are Computed
#'
#' @description Show how CFI and TLI are
#' computed using a graph of model
#' chi-square vs. model degrees of
#' freedom.
#'
#' @details This function receives an
#' output of [lavaan::lavaan()] or its
#' wrappers (e.g., [lavaan::cfa()]
#' and [lavaan::sem()]) and
#' illustrates how CFI is computed.
#'
#' @return An output of
#' [ggplot2::ggplot()] that can be
#' further modified.
#'
#' @param fit An output of
#' [lavaan::lavaan()] or its wrappers
#' (e.g., [lavaan::cfa()] and
#' [lavaan::sem()]).
#'
#' @param fit_measures The fit measures
#' to be plotted. Acceptable values are
#' `"cfi"` and `"tli"`.
#'
#' @param test The type of model
#' chi-square test. It corresponds
#' to the `test` argument of
#' [lavaan::lavaan()] or its wrappers.
#' Only `"standard"` is supported for now.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#'
#' library(lavaan)
#'
#' # From the help page of lavaan::cfa().
#'
#' HS.model <- '
#' visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9
#' '
#' fit <- cfa(HS.model, data = HolzingerSwineford1939)
#'
#' # By default show how CFI is computed
#' show_ifi(fit)
#'
#' # Show how TLI is computed
#' show_ifi(fit, fit_measures = "tli")
#'
#' # Wrappers
#' show_cfi(fit)
#' show_tli(fit)
#'
#' @export
#'

show_ifi <- function(fit,
                     fit_measures = c("cfi", "tli"),
                     test = c("standard")) {
    fit_measures <- match.arg(fit_measures)
    test <- match.arg(test)
    fit_baseline <- lavaan::lavTech(fit, what = "baseline.test")
    if (is.null(fit_baseline)) {
        stop(paste0("Baseline model is not in 'fit'.",
                    " Was 'baseline' set to 'FALSE'?"))
      }
    if (!(test %in% names(fit_baseline)))  {
        stop(paste0("Test ", dQuote(test),
                    " is not available in 'fit'."))
      }

    df_model <- unname(lavaan::fitMeasures(fit, fit.measures = "df"))
    chisq_model <- unname(lavaan::fitMeasures(fit, fit.measures = "chisq"))

    fit_baseline1 <- fit_baseline[[test]]
    chisq_base <- fit_baseline1$stat
    df_base <- fit_baseline1$df
    chisq_max <- chisq_base * 5 / 4
    df_max <- df_base * 5 / 4

    chisq_df_diff_base <- chisq_base - df_base
    chisq_df_ratio_base <- chisq_base / df_base

    pbase <- ggplot2::ggplot() +
      ggplot2::ylim(0, chisq_max) +
      ggplot2::xlim(0, df_max) +
      ggplot2::labs(
          # title = "Discrepancy (chisq) vs. Simplicity (df)",
          x = "Complex Model <- Degrees of Freedom (df) -> Simple Model",
          y = "Good absolute fit <- Chi-squared (Chisq) -> Bad absolute fit")

    base_point <-
      ggplot2::geom_point(ggplot2::aes(x = df_base,
                                       y = chisq_base),
                          size = 6,
                          colour = "black")
    base_text <-
      ggplot2::annotate("text",
                vjust = 0,
                hjust = .5,
                x = df_base,
                y = chisq_base + chisq_max / 20,
                colour = "red",
                label = paste("Baseline Model\n(df=",
                              df_base,
                              ", Chisq=",
                              formatC(chisq_base, digits = 3, format = "f"),
                              ")",
                              sep = ""))
    saturated_point <-
      ggplot2::geom_point(ggplot2::aes(x = 0,
                                       y = 0),
                          size = 6,
                          colour = "dark green")
    saturated_text <-
      ggplot2::annotate("text",
                vjust = 0,
                hjust = 0,
                x = 0,
                y = 0 + chisq_max / 20,
                colour = "dark green",
                label = "Saturated Model\n(df=0, Chisq=0)")

    df_model <- round(df_base * .75)
    chisq_model <- chisq_base * .20

    p_out <- pbase +
      base_point +
      # base_text +
      saturated_point +
      saturated_text +
      ggplot2::geom_segment(ggplot2::aes(x = 1,
                                         y = 1,
                                         xend = df_base,
                                         yend = df_base),
                            colour = "red",
                            size = 1) +
      ggplot2::geom_point(ggplot2::aes(x = df_base,
                                       y = df_base),
                          size = 4,
                          colour = "red") +
      ggplot2::annotate("text",
                        vjust = 0,
                        hjust = 0.5,
                        x = df_base,
                        y = chisq_base + chisq_max/ 20,
                        label = paste("Baseline Model\n(df=",
                                      df_base,
                                      ", Chisq=",
                                      formatC(chisq_base, digits = 3,
                                              format = "f"),
                                      ")",
                                      sep = ""),
                        colour = "black") +
      ggplot2::annotate("text",
                        vjust = 1,
                        hjust = 0,
                        x = df_base,
                        y = df_base - chisq_max / 40,
                        label = "Red line: Chisq=df",
                        colour = "red") +
      ggplot2::geom_segment(ggplot2::aes(x = df_model,
                                         y = chisq_model,
                                         xend = df_model,
                                         yend = df_model),
                                         size = 1,
                                         colour = "dark green") +
      ggplot2::geom_segment(ggplot2::aes(x = df_model - 2 * df_max / 20,
                                         y = df_model,
                                         xend = df_model + 2 * df_max / 20,
                                         yend = df_model),
                            size = 1,
                            colour = "dark green") +
      ggplot2::geom_point(ggplot2::aes(x = round(df_base * .75),
                                       y = chisq_base * .20),
                                       size = 7,
                                       colour = "blue") +
      ggplot2::annotate("text",
                        vjust = .5,
                        hjust = 1,
                        x = df_model - df_max / 20,
                        y = chisq_model,
                        label = "Fitted model",
                        colour = "blue")

    # CFI
    if (fit_measures == "cfi") {
        p_out <- p_out +
          ggplot2::annotate("text",
                vjust = 0,
                hjust = 0,
                x = 0,
                y = chisq_df_diff_base + chisq_max / 20,
                label = paste0("Blue line:\n",
                               "(Chisq - df) of Fitted Model =\n",
                               "(Chisq - df) of Baseline Model"),
                color = "blue") +
          ggplot2::annotate("text",
                            vjust = .5,
                            hjust = .5,
                            x = df_base * .75 * .5,
                            y = chisq_base * .40,
                            size = 8,
                            label = "CFI=B/A") +
          ggplot2::geom_segment(ggplot2::aes(x = 1,
                                            y = chisq_df_diff_base,
                                            xend = df_base,
                                            yend = chisq_base),
                                colour = "blue",
                                size = 1) +
          ggplot2::geom_segment(ggplot2::aes(x = df_model + df_max / 20,
                                            y = df_model + chisq_df_diff_base,
                                            xend = df_model + df_max / 20,
                                            yend = df_model),
                                size = 2,
                                colour = "black",
                                arrow = ggplot2::arrow(type = "closed")) +
          ggplot2::geom_segment(ggplot2::aes(x = df_model,
                                            y = chisq_base - round(df_base * (1 - .75)),
                                            xend = df_model,
                                            yend = chisq_base * .20),
                                size = 2,
                                colour = "black",
                                arrow = ggplot2::arrow(type = "closed")) +
          ggplot2::annotate("text",
                            vjust = .5,
                            hjust = 0,
                            x = df_model + 2 * df_max / 20,
                            y = chisq_base * .50,
                            label = "A",
                            size = 6) +
          ggplot2::annotate("text",
                            vjust = .5,
                            hjust = 1,
                            x = round(df_base * .75) - df_max / 20,
                            y = chisq_base * .50,
                            label = "B",
                            size = 6) +
          ggplot2::geom_segment(ggplot2::aes(x = df_model - 2 * df_max / 20,
                                            y = df_model + chisq_df_diff_base,
                                            xend = df_model + 2 * df_max / 20,
                                            yend = df_model + chisq_df_diff_base),
                                size = 1,
                                colour = "dark green") +
          ggplot2::labs(title = "The Computation Of CFI")
      }
    # TLI
    if (fit_measures == "tli") {
        p_out <- p_out +
          ggplot2::annotate("text",
                            vjust = 1,
                            hjust = .5,
                            x = df_base / 2,
                            y = chisq_base,
                            label = paste0("Blue line:\n",
                                           "(Chisq/df) of ",
                                           "Fitted Model = \n",
                                           "(Chisq/df) of ",
                                           "Baseline Model"),
                            colour = "blue") +
          ggplot2::geom_segment(ggplot2::aes(x = 1,
                                             y = chisq_df_ratio_base,
                                             xend = df_base,
                                             yend = chisq_base),
                                colour = "blue",
                                size = 1) +
          ggplot2::annotate("text",
                            vjust = .5,
                            hjust = .5,
                            x = df_base * .75 * .30,
                            y = chisq_base * .5,
                            size = 8,
                            label = "TLI=D/C") +
          ggplot2::geom_segment(ggplot2::aes(x = df_model - 2 * df_max / 20,
                                             y = df_model * (chisq_df_ratio_base),
                                             xend = df_model + 2 * df_max / 20,
                                             yend = df_model * (chisq_df_ratio_base)),
                                             size = 1,
                                             colour = "dark green") +
          ggplot2::geom_segment(ggplot2::aes(x = df_model + df_max / 20,
                                             y = df_model * (chisq_df_ratio_base),
                                             xend = df_model + df_max / 20,
                                             yend = df_model),
                                size = 2,
                                colour = "black",
                                arrow = ggplot2::arrow(type="closed")) +
          ggplot2::geom_segment(ggplot2::aes(x = df_model,
                                             y = df_model * (chisq_df_ratio_base),
                                             xend = df_model,
                                             yend = chisq_model),
                                size = 2,
                                colour = "black",
                                arrow = ggplot2::arrow(type = "closed")) +
          ggplot2::annotate("text",
                            vjust = .5,
                            hjust = 0,
                            x = df_model + 2 * df_max / 20,
                            y = chisq_base * .50,
                            label = "C",
                            size = 6) +
          ggplot2::annotate("text",
                            vjust = .5,
                            hjust = 1,
                            x = round(df_base * .75) - df_max / 20,
                            y = chisq_base * .50,
                            label = "D",
                            size = 6) +
          ggplot2::labs(title = "The Computation Of TLI")
      }

    p_out
  }

#' @param ... Arguments to be passed to [show_ifi()].
#'
#' @describeIn show_ifi A wrapper of [show_ifi()] with `fit_measures = "cfi"`.
#' @order 2
#' @export

show_cfi <- function(fit, ...) {
    show_ifi(fit, fit_measures = "cfi", ...)
  }

#' @param ... Arguments to be passed to [show_ifi()].
#'
#' @describeIn show_ifi A wrapper of [show_ifi()] with `fit_measures = "tli"`.
#' @order 3
#' @export

show_tli <- function(fit, ...) {
    show_ifi(fit, fit_measures = "tli", ...)
  }