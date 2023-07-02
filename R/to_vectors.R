#' @title Wrapper Functions to Extract
#' Information as a Vector
#'
#' @description A set of wrapper functions
#' to extract information from a
#' `lavaan`-class object and return
#' a named vector.
#'
#' @details This set of wrapper
#' functions are for functions like
#' [lavaan::bootstrapLavaan()] that
#' require users to supply a function
#' that receives
#' a `lavaan`-class object and returns
#' a vector of values.
#'
#' All wrappers functions are designed
#' to have the same form of output:
#' a named numeric vector.
#'
#' Many of the tasks of this set of
#' wrappers can be performed
#' by writing our own functions. The
#' wrapper functions are developed just
#' to save the coding time for some
#' commonly requested information.
#'
#' The wrapper functions are designed
#' to be as simple to use as possible,
#' with as few arguments as possible.
#' If advanced control is needed, users
#' are recommended to write their own
#' wrappers.
#'
#' @return
#' All of them return a named numeric
#' vector.
#'
#' @param object A `lavaan`-class
#' object.
#'
#' @param ... Additional arguments to
#' be passed to the original function.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#'
#' @seealso [lavaan::lavInspect()]
#'
#' @examples
#'
#' # From the help page of lavaan::cfa().
#'
#' library(lavaan)
#' HS.model <- '
#' visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9
#' '
#' fit <- cfa(HS.model, data = HolzingerSwineford1939)
#'
#' vec_rsquare(fit)
#' vec_sample_vcov(fit)
#' vec_sample_var(fit)
#' vec_est_var(fit)
#' vec_est_se(fit)
#'
#' HS.model.sem1 <- '
#' visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9
#' textual ~ a * visual
#' speed ~ b * textual
#' ab := a * b
#' '
#' fit_sem1 <- sem(HS.model.sem1, data = HolzingerSwineford1939)
#'
#' HS.model.sem2 <- '
#' visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9
#' textual ~ a * visual
#' speed ~ b * textual + cp * visual
#' ab := a * b
#' '
#' fit_sem2 <- sem(HS.model.sem2, data = HolzingerSwineford1939)
#'
#' vec_def_var(fit_sem1)
#' vec_def_se(fit_sem1)
#'
#' vec_lavTestLRT(fit_sem1, fit_sem2,
#'                model.names = c("No Direct", "Direct"))
#'
#' vec_lavTestScore(fit_sem1,
#'                  add = "speed ~ visual")
#'
#' vec_lavTestWald(fit_sem2,
#'                 constraints = "cp == 0")
#'
#' vec_compRelSEM(fit)
#'
#'
#' @name vector_from_lavaan
NULL

#' @export
#' @describeIn vector_from_lavaan Get R-squares in a model.
#' @order 1

vec_rsquare <- function(object) {
    out0 <- lavaan::lavInspect(object, what = "r2",
                               add.class = FALSE,
                               list.by.group = TRUE,
                               drop.list.single.group = FALSE)
    ngroup <- lavaan::lavInspect(object, "ngroups")
    if (ngroup > 1) {
        group_labels <- paste0(".", names(out0))
      } else {
        group_labels <- ""
      }
    out1 <- mapply(function(x, xname) {
                names(x) <- paste0(names(x), "_rsq", xname)
                x
              }, x = out0, xname = group_labels,
              SIMPLIFY = FALSE)
    names(out1) <- NULL
    out <- unlist(out1)
    return(out)
  }

#' @export
#' @describeIn vector_from_lavaan Get sample variances and covariances.
#' @order 2

vec_sample_vcov <- function(object) {
    out0 <- lavaan::lavInspect(object, what = "sampstat",
                               add.class = FALSE,
                               list.by.group = TRUE,
                               drop.list.single.group = FALSE)
    ngroup <- lavaan::lavInspect(object, "ngroups")
    if (ngroup > 1) {
        group_labels <- paste0(".", names(out0))
      } else {
        group_labels <- ""
      }
    out1 <- lapply(out0, function(x) x$cov)
    out2 <- mapply(function(x, xname) {
                xx <- to_lower_triangular(x, diag = TRUE)
                names(xx) <- paste0(names(xx), xname)
                xx
              }, x = out1, xname = group_labels,
              SIMPLIFY = FALSE)
    names(out2) <- NULL
    out <- unlist(out2)
    return(out)
  }

#' @export
#' @describeIn vector_from_lavaan Get sample variances.
#' @order 3

vec_sample_var <- function(object) {
    out0 <- lavaan::lavInspect(object, what = "sampstat",
                               add.class = FALSE,
                               list.by.group = TRUE,
                               drop.list.single.group = FALSE)
    ngroup <- lavaan::lavInspect(object, "ngroups")
    if (ngroup > 1) {
        group_labels <- paste0(".", names(out0))
      } else {
        group_labels <- ""
      }
    out1 <- lapply(out0, function(x) diag(x$cov))
    out2 <- mapply(function(x, xname) {
                names(x) <- paste0(names(x), "_var", xname)
                x
              }, x = out1, xname = group_labels,
              SIMPLIFY = FALSE)
    names(out2) <- NULL
    out <- unlist(out2)
    return(out)
  }


#' @export
#' @describeIn vector_from_lavaan Sampling variances of free parameters.
#' @order 4

vec_est_var <- function(object) {
    out0 <- lavaan::lavInspect(object, what = "vcov",
                               add.class = FALSE,
                               list.by.group = TRUE,
                               drop.list.single.group = FALSE)
    out0 <- diag(out0)
    if (length(out0) == 0) {
        stop("No parameters selected")
      }
    return(out0)
  }


#' @export
#' @describeIn vector_from_lavaan Standard errors of free parameters.
#' @order 5

vec_est_se <- function(object) {
    out <- vec_est_var(object = object)
    out <- sqrt(out)
    return(out)
  }


#' @export
#' @describeIn vector_from_lavaan Sampling variances of user-defined parameters.
#' @order 6

vec_def_var <- function(object) {
    out0 <- lavaan::lavInspect(object, what = "vcov.def",
                               add.class = FALSE,
                               list.by.group = TRUE,
                               drop.list.single.group = FALSE)
    out0 <- diag(out0)
    if (length(out0) == 0) {
        stop("No user defined parameters in the model.")
      }
    return(out0)
  }

#' @export
#' @describeIn vector_from_lavaan Standard errors of user-defined parameters.
#' @order 7

vec_def_se <- function(object) {
    out <- vec_def_var(object = object)
    out <- sqrt(out)
    return(out)
  }



#' @param method An argument to be
#' passed to [lavaan::lavTestLRT()].
#' Please refer to the help page of
#' [lavaan::lavTestLRT()].
#'
#' @param A.method An argument to be
#' passed [lavaan::lavTestLRT()].
#' Please refer to the help page of
#' [lavaan::lavTestLRT()].
#'
#' @param scaled.shifted An argument to
#' be
#' passed to [lavaan::lavTestLRT()].
#' Please refer to the help page of
#' [lavaan::lavTestLRT()].
#'
#' @param H1 An argument to be passed
#' to
#' [lavaan::lavTestLRT()].
#' Please refer to the help page of
#' [lavaan::lavTestLRT()].
#'
#' @param model.names An argument to
#' be
#' passed to [lavaan::lavTestLRT()].
#' Please refer to the help page of
#' [lavaan::lavTestLRT()].
#' Unlike
#' [lavaan::lavTestLRT()], this argument
#' is required, for the sake of naming
#' the vector to be returned.
#'
#' @export
#' @describeIn vector_from_lavaan Get sample variances.
#' @order 8

vec_lavTestLRT <- function(object, ...,
                           method = "default",
                           A.method = "delta",
                           scaled.shifted = TRUE,
                           H1 = TRUE,
                           model.names = NULL) {
    if (is.null(model.names)) {
        stop("'model.names' cannot be NULL.")
      }
    out0 <- lavaan::lavTestLRT(object,
                               ...,
                               method = method,
                               A.method = A.method,
                               scaled.shifted = scaled.shifted,
                               H1 = H1,
                               type = "Chisq",
                               model.names = model.names)
    p <- nrow(out0)
    pp <- seq(from = 2, to = p)
    pp2 <- pp - 1
    mnames <- rownames(out0)
    m2names <- paste0(mnames[pp2],
                      "_vs_",
                      mnames[pp])
    out1 <- mapply(function(x, xname) {
                    outi <- out0[x, c("Chisq diff", "Df diff", "Pr(>Chisq)"),
                                 drop = TRUE]
                    outi <- unlist(outi)
                    names(outi) <- paste0(c("chisq_diff.",
                                     "df_diff.",
                                     "chisq_p."), xname)
                    outi
                  }, x = pp, xname = m2names,
                  SIMPLIFY = FALSE)
    out <- unlist(out1)
    return(out)
  }

#' @param add An argument to be passed
#' to [lavaan::lavTestScore()]. Please
#' refer to the help page of
#' [lavaan::lavTestScore()].
#'
#' @param release An argument to be passed
#' to [lavaan::lavTestScore()]. Please
#' refer to the help page of
#' [lavaan::lavTestScore()].
#'
#' @param univariate An argument to be passed
#' to [lavaan::lavTestScore()]. Please
#' refer to the help page of
#' [lavaan::lavTestScore()].
#'
#' @param information An argument to be passed
#' to [lavaan::lavTestScore()]. Please
#' refer to the help page of
#' [lavaan::lavTestScore()].
#'
#' @export
#' @describeIn vector_from_lavaan Do score tests.
#' @order 9

vec_lavTestScore <- function(object,
                             add = NULL,
                             release = NULL,
                             univariate = TRUE,
                             information = "expected") {
    out0 <- lavaan::lavTestScore(object,
                                 add = add,
                                 release = release,
                                 univariate = univariate,
                                 information = information)
    outa0 <- unlist(out0$test[-1])
    names(outa0) <- c("score_chisq", "score_df", "score_p")
    if (univariate) {
        outb0 <- as.data.frame(out0$uni)
        colnames(outb0) <- c("lhs", "op", "rhs",
                             "chisq", "df", "p")
        outb1 <- split(outb0[, c("chisq", "df", "p")],
                        f = outb0$lhs,
                        drop = FALSE,
                        sep = "_")
        outb <- unlist(outb1)
      } else {
        outb <- NULL
      }
    out <- c(outa0, outb)
    return(out)
  }

#' @param constraints An argument to be passed
#' to [lavaan::lavTestWald()]. Please
#' refer to the help page of
#' [lavaan::lavTestWald()].
#'
#' @param prefix Optional. A character
#' string to be added as a prefix to
#' names of the output. Default is
#' `NULL`.
#'
#' @export
#' @describeIn vector_from_lavaan Do a Wald test.
#' @order 10

vec_lavTestWald <- function(object,
                            constraints = NULL,
                            prefix = NULL) {
    out0 <- lavaan::lavTestWald(object,
                                constraints = constraints,
                                verbose = FALSE)
    out1 <- c(wald_stat = out0$stat,
              wald_df = out0$df,
              wald_p = out0$p.value)
    if (!is.null(prefix)) {
        names(out1) <- paste0(prefix, "_", names(out1))
      }
    return(out1)
  }

#' @export
#' @describeIn vector_from_lavaan Composite reliability.
#' @order 11

vec_compRelSEM <- function(object,
                           ...) {
    if (!requireNamespace("semTools")) {
        stop("Please install 'semTools' first.")
      }
    my_call <- match.call()
    if ("return.df" %in% names(my_call)) {
        stop("'return.df' cannot be set. Please remove it.")
      }
    out0 <- semTools::compRelSEM(object,
                                 return.df = FALSE,
                                 ...)
    ngroup <- lavaan::lavInspect(object, "ngroups")
    if (ngroup > 1) {
        group_labels <- paste0(".", names(out0))
        out1 <- mapply(function(x, xname) {
                    names(x) <- paste0(names(x), "_rel", xname)
                    x
                  }, x = out0, xname = group_labels,
              SIMPLIFY = FALSE)
        names(out1) <- NULL
        out <- unlist(out1)
      } else {
        names(out0) <- paste0(names(out0), "_rel")
        out <- out0
      }
    return(out)
  }


#### Helpers (Not Exported)

#' Convert a matrix to a lower triangular matrix
#' @noRd

to_lower_triangular <- function(x, diag = TRUE) {
    out <- x[lower.tri(x, diag = diag)]
    names(out) <- lower_triangular_names(x, diag = diag)
    return(out)
  }

#' Create names for a lower triangular matrix
#' @noRd

lower_triangular_names <- function(x, diag = TRUE) {
    i <- ncol(x)
    out <- character(ifelse(diag,
                            i * (i + 1) / 2,
                            i * (i - 1) / 2))
    vnames <- colnames(x)
    kk <- 0
    for (ii in seq_len(i)) {
        for (jj in seq(from = ifelse(diag, ii, ii + 1),
                       to = i)) {
            kk <- kk + 1
            out[kk] <- paste0(vnames[ii], "~~", vnames[jj])
          }
      }
    out
  }
