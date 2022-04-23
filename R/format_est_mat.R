#' # An internal function
#'
#' @param x The object to be printed.
#' @param rowname_ind A vector of the column numbers that
#'                    should be converted to row names.
#' @param nd The number of decimal places.
#' @param nd_tst The number of digits for test statistics.
#' @param na_print The string to be printed for `NA`.
#'
#' @noRd

format_est_mat <- function(x,
                          v_ind = 1,
                          gp_ind = NULL,
                          group_first = FALSE,
                          nd = 3,
                          nd_tst = 3,
                          na_print = "--") {
    x <- as.data.frame(x)
    vn <- x[, v_ind]
    if (!is.null(gp_ind)) {
        gn <- as.character(x[, gp_ind])
      } else {
        gn <- NULL
      }
    out <- x[-c(v_ind, gp_ind)]
    fct <- function(x) {
        if (is.na(x)) {
            out <- na_print
          } else {
            out <- formatC(x, digits = nd, format = "f")
          }
        return(out)
      }
    out2 <- data.frame(lapply(out,
                              function(x) sapply(x, FUN = fct)))
    if (!is.null(gn)) {
        if (group_first) {
            rownames(out2) <- paste0(gn, ": ", vn)
          } else {
            rownames(out2) <- paste0(vn, "[", gn, "]")
          }
      } else {
        rownames(out2) <- vn
      }
    return(out2)
  }