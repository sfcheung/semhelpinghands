#' @title Annotate the Matrices of a
#' 'lavaan' Model
#'
#' @description Label the elements of
#' the model matrices in a lavaan model.
#'
#' @details This function annotates the
#' model matrices in a [lavaan-class]
#' object. This function is not to be
#' used in analysis. It is a learning
#' tool, for learners to understand
#' the relation between the model
#' matrices and the model parameters.
#'
#' It currently supports a single-level
#' single-group model only.
#'
#' @return [annotate_matrices()]
#' returns an
#' `annotate_matrices`-class object,
#' which is a list of model matrices,
#' with elements annotated:
#'
#' - If a parameter is free, then it is
#'   represented by `"lhs-op-rhs"`
#'   according to the parameter estimate
#'   data frame.
#'
#' - If a parameter is fixed but appears
#'   in the parameter table, it is
#'   represented by `"(lhs-op-rhs = x)"`
#'   , x the value it is fixed to.
#'
#' - If a parameter is fixed to zero but
#'   not in the parameter table, then it
#'   is represented by 0.
#'
#' The print-method return the input,
#' `x`. It was called for its
#' side-effect.
#'
#' @param fit The output of
#' [lavaan::lavaan()] or its wrappers,
#' such as [lavaan::cfa()] and
#' [lavaan::sem()].
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' # Adapted from https://lavaan.ugent.be/tutorial/cfa.html
#'
#' library(lavaan)
#
#' HS.model <- '
#' visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9
#' '
#' fit_cfa <- cfa(HS.model,
#'                data = HolzingerSwineford1939)
#
#' annotate_matrices(fit_cfa)
#'
#' @export
#'

annotate_matrices <- function(fit) {
    if (!inherits(fit, "lavaan")) {
        stop("'fit' is not a lavaan-class object.")
      }
    if (lavaan::lavTech(fit, what = "ngroups") != 1) {
        stop("Multi-sample models are not supported.")
      }
    if (lavaan::lavTech(fit, what = "nlevels") != 1) {
        stop("Multilevel models are not supported.")
      }
    partable <- lavaan::parameterTable(fit)
    partable$pname <- paste0(partable$lhs, partable$op, partable$rhs)
    fcttmp <- function(x, y, z, partable) {
        out <- x
        class(out) <- "matrix"
        out[] <- as.character(x)
        out[x != 0] <- partable$pname[z[x != 0]]
        b <- (x == 0) & (z != 0)
        d <- z[b]
        e <- y[b]
        out[b] <- paste0("(",partable$pname[d], " = ", round(e, 3), ")")
        out
      }
    out <- mapply(fcttmp,
                  lavaan::lavInspect(fit, "free"),
                  lavaan::lavInspect(fit, "start"),
                  lavaan::lavInspect(fit, "partable"),
                  MoreArgs = list(partable = partable),
                  SIMPLIFY = FALSE)
    class(out) <- c("annotate_matrices", class(out))
    out
  }

#' @param x A 'annotate_matrices'-class
#' object. The output of
#' [annotate_matrices()].
#'
#' @param ... Optional arguments. To be
#' passed to the default print method.
#'
#' @export
#' @describeIn annotate_matrices The
#' print method of the output of
#' [annotate_matrices()]

print.annotate_matrices <- function(x, ...) {
    class(x) <- class(x)[-1]
    NextMethod(print, x, quote = FALSE, right = TRUE, ...)
  }