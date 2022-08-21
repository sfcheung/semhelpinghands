#' @title Group Estimates By Models
#'
#' @description Groups parameter
#' estimates or other information such
#' as *p*-values into a table with
#' models as columns.
#'
#' @return A data-frame-like object of
#' the class `est_table`.
#'
#' @param object_list A named list of
#' [lavaan-class] objects, a named list
#' of the output of
#' [lavaan::parameterEstimates()], or a
#' named list of the output of
#' [lavaan::standardizedSolution()].
#'
#' @param ... Optional arguments to be
#' passed to
#' [lavaan::parameterEstimates()].
#' Ignored if the elements in
#' object_list are the results of
#' [lavaan::parameterEstimates()] or
#' [lavaan::standardizedSolution()].
#'
#' @param col_names A vector of the
#' column names in the parameter
#' estimate tables to be included.
#' Default is `"est"`.
#'
#' @param group_first If `TRUE`, the
#' rows will be grouped by groups first
#' and then by parameters. Ignored if
#' the model has only one group. Default
#' is `FALSE`.
#'
#' @param model_first If `TRUE`, the
#' columns will be grouped by models
#' first and then by columns in the
#' parameter estimates tables. Default
#' is `TRUE`.
#'
#' @param use_standardizedSolution If `TRUE`
#' and `object_list` is not a list of
#' estimates tables,
#' then [lavaan::standardizedSolution()]
#' will be used to generate the table.
#' If `FALSE`, the default, then
#' [lavaan::parameterEstimates()] will
#' be used if necessary.
#'
#' @author Shu Fai Cheung
#' <https://orcid.org/0000-0002-9871-9448>
#' Inspired by the proposal Rönkkö posted in a GitHub
#' <https://github.com/simsem/semTools/issues/24#issue-235172313>
#' for `semTools`. I want something
#' simple for a quick overview and so
#' I wrote this function.
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
#' parameterEstimates(fit1)
#' parameterEstimates(fit2)
#' group_by_models(list(no_direct = fit1,
#'                      direct = fit2),
#'                 col_names = c("est", "pvalue"))
#' # Can also be used with some other functions in
#' # semhelpinghands
#' group_by_models(list(no_direct = fit1,
#'                      direct = fit2),
#'                 col_names = c("est", "pvalue")) |>
#'                 filter_by(op = "~")
#'
#'
#' @export

group_by_models <- function(object_list,
                            ...,
                            col_names = "est",
                            group_first = FALSE,
                            model_first = TRUE,
                            use_standardizedSolution = FALSE) {
    output_type <- all_type(object_list)
    if (is.na(output_type)) {
        stop("object_list is invalid. Not of the same types or not of the accepted types.")
      }
    if (is.null(names(object_list))) {
        stop("object_list must be a named list.")
      }
    grouped <- is_grouped(object_list[[1]])
    if (grouped) {
        if (group_first) {
            m <- c("group", "lhs", "op", "rhs")
          } else {
            m <- c("lhs", "op", "rhs", "group")
          }
      } else {
        m <- c("lhs", "op", "rhs")
      }
    k <- length(object_list)
    model_names <- names(object_list)
    if (output_type == "lavaan") {
        if (use_standardizedSolution) {
            p_est_list <- sapply(object_list,
                                lavaan::standardizedSolution,
                                ...,
                                simplify = FALSE, USE.NAMES = TRUE)
          } else {
            p_est_list <- sapply(object_list,
                                lavaan::parameterEstimates,
                                ...,
                                simplify = FALSE, USE.NAMES = TRUE)
          }
      } else {
        p_est_list <- object_list
      }
    est_list_tmp <- sapply(p_est_list,
                           function(x) {
                              x[, c(m, col_names)]
                            },
                          simplify = FALSE,
                          USE.NAMES = TRUE)
    if (grouped) {
        if (group_first) {
            out <- Reduce(`%mergemodgp1st%`, est_list_tmp)
          } else {
            out <- Reduce(`%mergemod%`, est_list_tmp)
          }
      } else {
        out <- Reduce(`%mergemod2%`, est_list_tmp)
      }
    colnames(out) <- c(m,
                  paste(rep(col_names, k),
                        rep(model_names, each = length(col_names)),
                        sep = "_"))
    class(out) <- c("lavaan.data.frame", class(out))
    mseq <- seq_len(length(m))
    cnlen <- length(col_names)
    if (model_first) {
        tmp <- as.vector(t(matrix(seq_len(k * cnlen), cnlen, k))) + length(m)
        out <- out[, c(mseq, tmp)]
      }
    out
  }

#' @noRd

`%mergemod%` <- function(x, y) {
    suppressWarnings(merge(x,
          y,
          by = c("lhs", "op", "rhs", "group"),
          all = TRUE))
  }

#' @noRd

`%mergemodgp1st%` <- function(x, y) {
    suppressWarnings(merge(x,
          y,
          by = c("group", "lhs", "op", "rhs"),
          all = TRUE))
  }

#' @noRd

`%mergemod2%` <- function(x, y) {
    suppressWarnings(merge(x,
          y,
          by = c("lhs", "op", "rhs"),
          all = TRUE))
  }
