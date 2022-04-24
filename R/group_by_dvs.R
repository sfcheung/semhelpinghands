#' @title Group Estimates By Dependent or Independent Variables
#'
#' @description Groups parameter estimates or other information such as
#'               p-values into a table with dependent variables as columns
#'               and independent variables as rows, or a transpose of
#'               this table.
#'
#' @return A data-frame-like object of the class `est_table`.
#'
#' @param object A [lavaan-class] object.
#'
#' @param ... Optional arguments to be passed to
#'            [lavaan::parameterEstimates()].
#'
#' @param col_name The column name of information to be
#'                 grouped. Default is `"est"`. It accepts
#'                 only one name.
#'
#' @param add_prefix If `TRUE`, the default,
#'                   `colname` will be added as
#'                   prefix to the column names of the
#'                   output.
#'
#' @param group_first If `TRUE`, the rows will be grouped
#'                    by groups first and then by
#'                    independent variables Ignored if
#'                    the model has only one group.
#'                    Default is `FALSE`.
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
#' fit <- sem(model, data = dat, fixed.x = FALSE)
#' parameterEstimates(fit)
#' group_by_dvs(fit)
#' group_by_ivs(fit)
#'
#' @name group_estimates
NULL

#' @rdname group_estimates
#' @export

group_by_dvs <- function(object,
                         ...,
                         col_name = "est",
                         add_prefix = TRUE,
                         group_first = FALSE) {
    if (!inherits(object, "lavaan")) {
        stop("object not a lavaan-class object.")
      }
    p_est <- lavaan::parameterEstimates(object,
                                        ...)
    if (all(is.na(match(col_name, colnames(p_est))))) {
        stop(paste(dQuote(col_name),
              "not in the column names of the parameter estimate table."))
      }
    grouped <- lavaan::lavInspect(object, "ngroups") > 1
    dvs <- unique(p_est[p_est$op == "~", "lhs"])
    p_est_list <- sapply(dvs, function(x, p_est) {
                          out <- p_est[(p_est$lhs == x) &
                                       (p_est$op == "~"), ]
                        }, p_est = p_est, simplify = FALSE)
    colname_list <- get_columns(col_name = col_name,
                                p_est_list = p_est_list,
                                grouped = grouped,
                                group_first = group_first)
    out <- merge_columns(colname_list,
                         grouped = grouped,
                         group_first = group_first)
    if (!add_prefix) {
        colnames(out) <- gsub(paste0(col_name, "_"), "",
                              colnames(out))
      }
    class(out) <- c("est_table", class(out))
    attr(out, "grouped") <- grouped
    attr(out, "group_first") <- group_first
    attr(out, "v_ind") <- 1
    attr(out, "gps") <- NULL
    if (grouped) {
        attr(out, "gp_ind") <- ifelse(group_first, 1, 2)
        attr(out, "v_ind") <- ifelse(group_first, 2, 1)
        attr(out, "gps") <- out[, attr(out, "gp_ind")]
      } else {
        attr(out, "gp_ind") <- NULL
      }
    attr(out, "ivs") <- out[, attr(out, "v_ind")]
    out
  }

#' @rdname group_estimates
#' @export

group_by_ivs <- function(object,
                         ...,
                         col_name = "est",
                         add_prefix = TRUE,
                         group_first = FALSE) {
    out <- group_by_dvs(object, ...,
                        col_name = col_name,
                        add_prefix = add_prefix,
                        group_first = group_first)
    v_ind <- attr(out, "v_ind")
    gp_ind <- attr(out, "gp_ind")
    ivs <- attr(out, "ivs")
    gps <- attr(out, "gps")
    if (group_first) {
        char_col <- c(gp_ind, v_ind)
      } else {
        char_col <- c(v_ind, gp_ind)
      }
    char_names <- colnames(out)[char_col]
    out_numeric <- out[, -char_col]
    out1 <- t(out_numeric)
    out1 <- as.data.frame(out1)
    if (is.null(gps)) {
        colnames(out1) <- ivs
      } else {
        if (group_first) {
            tmp <- paste0(gps,
                         ".",
                         ivs)
            colnames(out1) <- tmp
          } else {
            tmp <- paste0(ivs,
                         ".",
                         gps)
            colnames(out1) <- tmp
          }
      }
    out1 <- cbind(dv = rownames(out1), out1)
    class(out1) <- class(out)
    et_att <- c("v_ind", "gp_ind", "ivs", "gps", "grouped", "group_first")
    for (att_i in et_att) {
        attr(out1, att_i) <- attr(out, att_i)
      }
    attr(out1, "by_ivs") <- TRUE
    out1
  }

#' @noRd
get_columns <- function(col_name,
                        p_est_list,
                        grouped = FALSE,
                        group_first = FALSE) {
    if (!grouped) {
        m <- c("rhs")
        m2 <- c("iv")
      } else {
        if (group_first) {
            m <- c("rhs", "group")
            m2 <- c("iv", "group")
          } else {
            m <- c("group", "rhs")
            m2 <- c("group", "iv")
          }
      }
    out <- mapply(function(x, x_name) {
                      out <- x[, c(m, col_name)]
                      colnames(out) <- c(m2, paste0(col_name, "_", x_name))
                      out
                    },
                  p_est_list, names(p_est_list),
                  SIMPLIFY = FALSE)
    out
  }

#' @noRd
`%merge%` <- function(x, y) {
    merge(x,
          y,
          by = c("iv"),
          all = TRUE)
  }

#' @noRd
`%mergegp%` <- function(x, y) {
    merge(x,
          y,
          by = c("iv", "group"),
          all = TRUE)
  }

#' @noRd
`%mergegp1st%` <- function(x, y) {
    merge(x,
          y,
          by = c("group", "iv"),
          all = TRUE)
  }

#' @noRd
merge_columns <- function(col_list,
                          grouped = FALSE,
                          group_first = FALSE) {
    if (grouped) {
        if (group_first) {
            out <- Reduce(`%mergegp1st%`, col_list)
          } else {
            out <- Reduce(`%mergegp%`, col_list)
          }
      } else {
        out <- Reduce(`%merge%`, col_list)
      }
    #rownames(out) <- out$iv
    #out <- out[, -1]
    out
  }
