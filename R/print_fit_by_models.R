#' @title Print a 'fit_by_models' Object
#'
#' @description Print method for a
#' 'fit_by_models' object
#'
#' @param x Object of the class
#' `fit_by_models`.
#'
#' @param ... Optional arguments to be
#' passed to [print()] methods.
#'
#' @param nd The number of digits to be
#' printed. Default is 3. (Scientific
#' notation will never be used.)
#'
#' @param type String. The type of the
#' output. Currently only supports one
#' type, `"compact"`.
#'
#' @param remove_all_na Logical. Whether
#' rows with `NA` in all columns will be
#' removed. Default is `TRUE`.
#'
#' @param measures_compact If output
#' `type` is `"compact"`, the character
#' vector of fit measures to be
#' printed. The names should be the names
#' of the output of [lavaan::fitMeasures()],
#' in vector factor.
#'
#' @return
#'  `x` is returned invisibly. Called for its side effect.
#'
#' @author Shu Fai Cheung
#' <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [fitMeasures_by_models()]
#'
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
#' out <- fitMeasures_by_models(list(no_direct = fit1,
#'                                   direct = fit2))
#' out
#'
#' print(out, nd = 4, measures_compact = c("chisq", "cfi", "rmsea"))
#'
#' @export

print.fit_by_models <- function(x,
                                ...,
                                nd = 3,
                                type = c("compact"),
                                remove_all_na = TRUE,
                                measures_compact = c("npar",
                                                     "chisq",
                                                     "chisq.scaled",
                                                     "df",
                                                     "df.scaled",
                                                     "pvalue",
                                                     "pvalue.scaled",
                                                     "chisq.scaling.factor",
                                                     "cfi",
                                                     "cfi.robust",
                                                     "tli",
                                                     "tli.robust",
                                                     "aic",
                                                     "bic",
                                                     "bic2",
                                                     "rmsea",
                                                     "rmsea.ci.level",
                                                     "rmsea.ci.lower",
                                                     "rmsea.ci.upper",
                                                     "rmsea.close.h0",
                                                     "rmsea.pvalue",
                                                     "rmsea.robust",
                                                     "rmsea.ci.lower.robust",
                                                     "rmsea.ci.upper.robust",
                                                     "rmsea.pvalue.robust",
                                                     "srmr",
                                                     "srmr_nomean")) {
    out <- x
    x_cls <- class(out)
    class(out) <- x_cls[-which(x_cls == "fit_by_models")]
    if (!is.null(measures_compact) && type == "compact") {
        out <- out[out$short_name %in% measures_compact, ]
      }
    if (remove_all_na) {
        tmp <- apply(out[, -c(1, 2)], 1, function(x) !is.na(x))
        tmp <- colSums(tmp)
        out <- out[tmp != 0, ]
      }
    for (i in seq(from = 3, to = ncol(out))) {
        if (is.numeric(out[, i])) {
            out[, i] <- round(out[, i], nd)
          }
      }
    if (type == "compact") {
        rownames(out) <- out$long_name
        out <- out[-which(colnames(out) %in% c("short_name",
                                               "long_name"))]
        print(out)
      }
    invisible(x)
  }
