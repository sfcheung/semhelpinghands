#' @title Group Estimates By Models
#'
#' @description Groups parameter estimates or other information such as
#'               p-values into a table with models as columns.
#'
#' @return A data-frame-like object of the class `est_table`.
#'
#' @param output_list A named list of [lavaan-class] objects,
#'                    a named list of the output of
#'                    [lavaan::parameterEstimates()],
#'                    or a named list of the output of
#'                    [lavaan::standardizedSolution()].
#'
#' @param ... Optional arguments to be passed to
#'            [lavaan::parameterEstimates()]. Ignored
#'            if the elements in output_list are the results of
#'            [lavaan::parameterEstimates()] or
#'            [lavaan::standardizedSolution()].
#'
#' @param col_names A vector of the column names in the
#'                  parameter estimate tables to be included.
#'                  Default is `"est"`.
#'
#' @param group_first If `TRUE`, the rows will be grouped
#'                    by groups first and then by
#'                    parameters.
#'                    Ignored if
#'                    the model has only one group.
#'                    Default is `FALSE`.
#'
#' @param model_first If `TRUE`, the columns will be grouped
#'                    by models first and then by
#'                    columns in the parameter estimates
#'                    tables. Default is `TRUE`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' # TODO: Amend
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
#' fit <- sem(model, data = dat, fixed.x = FALSE)
#' parameterEstimates(fit)
#' group_by_dvs(fit)
#' group_by_ivs(fit)
#'
#' @export

group_by_models <- function(output_list,
                            ...,
                            col_names = "est",
                            group_first = FALSE,
                            model_first = TRUE) {
    output_type <- all_type(output_list)
    if (is.na(output_type)) {
        stop("output_list is invalid. Not of the same types or not of the accepted types.")
      }
    if (is.null(names(output_list))) {
        stop("output_list must be a named list.")
      }
    grouped <- is_grouped(output_list[[1]])
    if (grouped) {
        if (group_first) {
            m <- c("group", "lhs", "op", "rhs")
          } else {
            m <- c("lhs", "op", "rhs", "group")
          }
      } else {
        m <- c("lhs", "op", "rhs")
      }
    k <- length(output_list)
    model_names <- names(output_list)
    if (output_type == "lavaan") {
        p_est_list <- sapply(output_list,
                            lavaan::parameterEstimates,
                            ...,
                            simplify = FALSE, USE.NAMES = TRUE)
      } else {
        p_est_list <- output_list
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
