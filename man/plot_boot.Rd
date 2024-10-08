% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_boot.R
\name{plot_boot}
\alias{plot_boot}
\title{Diagnostic Plots of Bootstrap Estimates in 'lavaan'}
\usage{
plot_boot(
  object,
  param,
  standardized = NULL,
  nclass = NULL,
  hist_color = "lightgrey",
  hist_linewidth = 1,
  density_line_type = "solid",
  density_line_color = "blue",
  density_line_linewidth = 2,
  est_line_type = "dotted",
  est_line_color = "red",
  est_line_linewidth = 2,
  qq_dot_size = 2,
  qq_dot_color = "black",
  qq_dot_pch = 16,
  qq_line_linewidth = 2,
  qq_line_color = "black",
  qq_line_linetype = "solid"
)
}
\arguments{
\item{object}{Either
a \link[lavaan:lavaan-class]{lavaan::lavaan}
object with bootstrap estimates
stored, or the output of
\code{\link[=standardizedSolution_boot_ci]{standardizedSolution_boot_ci()}}.
For standardized solution
and user-defined parameters, if
the object is a \link[lavaan:lavaan-class]{lavaan::lavaan}
object, the
estimates need to be stored by
\code{\link[=store_boot_est_std]{store_boot_est_std()}} or
\code{\link[=store_boot_def]{store_boot_def()}}.}

\item{param}{String. The name of
the parameter to be plotted, which
should be the name as appeared in
a call to \code{coef()}.}

\item{standardized}{Logical. Whether
the estimates from the standardized
solution are to be plotted. Default
is \code{NULL}. If \code{object} is a
\link[lavaan:lavaan-class]{lavaan::lavaan} object, then
this is a required parameter
and users need to explicitly set it
to \code{TRUE} or \code{FALSE}. If \code{object} is
the output of
\code{\link[=standardizedSolution_boot_ci]{standardizedSolution_boot_ci()}},
then this argument is ignored (
forced to be \code{TRUE} internally).}

\item{nclass}{The number of breaks.
This argument will be passed to
\code{\link[=hist]{hist()}}. Default is \code{NULL}.}

\item{hist_color}{String. The color of the
bars in the histogram. It will be
passed to \code{\link[=hist]{hist()}} for the argument
\code{col}. Default is \code{"lightgrey"}.}

\item{hist_linewidth}{The width of
the borders of the bars in the
histogram. Default is 1.}

\item{density_line_type}{String.
The type of the line of the density
curve in the histogram. It will be
passed to \code{\link[=lines]{lines()}} for the argument
\code{lty}. Default is
\code{"solid"}.}

\item{density_line_color}{String.
The color of the density curve in
the histogram. It will be
passed to \code{\link[=lines]{lines()}} for the argument
\code{col}. Default is \code{"blue"}.}

\item{density_line_linewidth}{The width
of the density curve in the histogram.
It will be
passed to \code{\link[=lines]{lines()}} for the argument
\code{lwd}.
Default is 2.}

\item{est_line_type}{String. The
type of the vertical line in the
histogram showing the point estimate
of the parameter. It will be
passed to \code{\link[=abline]{abline()}} for the argument
\code{lty}. Default is
\code{"dotted"},}

\item{est_line_color}{String. The
color of the vertical line showing
the point estimate in the histogram.
It will be
passed to \code{\link[=abline]{abline()}} for the argument
\code{col}.
Default is \code{"red"}.}

\item{est_line_linewidth}{The width
of the vertical line showing the
point estimate in the histogram.
It will be
passed to \code{\link[=hist]{hist()}} for the argument
\code{lwd}.  Default is 2.}

\item{qq_dot_size}{The size of the
points in the normal QQ-plot.
It will be
passed to \code{\link[=qqnorm]{qqnorm()}} for the argument
\code{cex}. Default is 2.}

\item{qq_dot_color}{String. The color
of the points in the normal QQ-plot.
It will be
passed to \code{\link[=qqnorm]{qqnorm()}} for the argument
\code{col}.
Default is \code{"black"}.}

\item{qq_dot_pch}{Numeric. The shape
of the points in the normal QQ-plot.
It will be
passed to \code{\link[=qqnorm]{qqnorm()}} for the argument
\code{pch}. Default is 16.}

\item{qq_line_linewidth}{The width
of the diagonal line to be drawn in
the normal QQ-plot.
It will be
passed to \code{\link[=qqline]{qqline()}} for the argument
\code{lwd}. Default is 2.}

\item{qq_line_color}{String. The color
of the diagonal line to be drawn in
the normal QQ-plot.
It will be
passed to \code{\link[=qqline]{qqline()}} for the argument
\code{col}.
Default is \code{"black"}.}

\item{qq_line_linetype}{The type of
the diagonal line to be drawn in the
normal QQ-plot. Default is \code{"solid"}.}
}
\value{
Return the original
\link[lavaan:lavaan-class]{lavaan::lavaan} object
invisibly. Called for its side-effect
(plotting the graphs).
}
\description{
Plots for examining the
distribution of bootstrap estimates
in a model fitted by \code{lavaan}.
}
\details{
Rousselet, Pernet, and Wilcox (2021)
argued that when using bootstrapping,
it is necessary to examine the distribution
of bootstrap estimates. This can be
done when \code{\link[boot:boot]{boot::boot()}} is used
because it has a \code{plot} method for
its output. This cannot be easily
done in model fitted by \code{\link[lavaan:lavaan]{lavaan::lavaan()}}.

The function \code{\link[=plot_boot]{plot_boot()}} is used for
plotting the distribution of bootstrap
estimates for a model fitted by
\code{lavaan} in a format similar to that
of the output of \code{\link[boot:boot]{boot::boot()}}, with
a histogram on the left and a normal
QQ-plot on the right.

For free parameters in a model
(unstandardized), it can be called
directly on the output of \code{lavaan}
and retrieves the stored estimates.

For estimates of user-defined parameters,
call \code{\link[=store_boot_def]{store_boot_def()}} first to compute
and store the bootstrap estimates
first.

For estimates in standardized solution,
for both free and user-defined
parameters, call \code{\link[=store_boot_est_std]{store_boot_est_std()}}
first to compute and store the bootstrap
estimates in the standardized solution.

Since Version 0.1.11.2, it can also
plot bootstrap estimates in the output
of \code{\link[=standardizedSolution_boot_ci]{standardizedSolution_boot_ci()}}.
}
\examples{

library(lavaan)

data(simple_mediation)
mod <-
"
m ~ a * x
y ~ b * m + x
ab := a * b
"
fit <- sem(mod, simple_mediation,
           se = "bootstrap",
           bootstrap = 50,
           iseed = 985714)

# Can plot bootstrap estimates for
# free parameters directly
# Note that 'standardized' must be always be set to
# either TRUE or FALSE. No default value.
plot_boot(fit, "a", standardized = FALSE)

# For estimates of user-defined parameters,
# call store_boot_def() first.
fit <- store_boot_def(fit)
plot_boot(fit, "ab", standardized = FALSE)

# For estimates in standardized solution,
# call store_boot_est_std() first.
fit <- store_boot_est_std(fit)
plot_boot(fit, "a", standardized = TRUE)
plot_boot(fit, "ab", standardized = TRUE)

# It can also plot the estimates stored
# in the output of standardizedSolution_boot_ci().
std_boot <- standardizedSolution_boot_ci(fit)
plot_boot(std_boot, "ab")
plot_boot(fit, "ab", standardized = TRUE)


}
\references{
Rousselet, G. A., Pernet, C. R., & Wilcox, R. R. (2021).
The percentile bootstrap: A primer with step-by-step
instructions in R.
\emph{Advances in Methods and Practices in Psychological Science},
\emph{4}(1), 1--10. \doi{10.1177/2515245920911881}
}
\seealso{
\code{\link[lavaan:fitMeasures]{lavaan::fitMeasures()}}, \code{\link[=store_boot_est_std]{store_boot_est_std()}},
and \code{\link[=store_boot_def]{store_boot_def()}}.
}
\author{
Shu Fai Cheung \url{https://orcid.org/0000-0002-9871-9448}
}
