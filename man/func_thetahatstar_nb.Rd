% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nBootstrap-func_thetahatstar_nb.R
\name{func_thetahatstar_nb}
\alias{func_thetahatstar_nb}
\title{Constructor for the thetahatstar_nb Function}
\usage{
func_thetahatstar_nb(x, func, ...)
}
\arguments{
\item{x}{Matrix, data frame, or vector.
Sample data.}

\item{func}{Fit function.
The first argument should be sample data.}

\item{...}{Additional named arguments to pass to \code{func}.}
}
\value{
A function with \code{x} and \code{func} in the environment.
}
\description{
Constructor for the thetahatstar_nb Function
}
\section{Dependencies}{
\itemize{
\item \code{\link[=rmvn_chol]{rmvn_chol()}} (test)
}
}

\examples{
x <- rnorm(n = 100)
func <- median

func_thetahatstar_nb(x, func = func)
}
\seealso{
Other Nonparametric Bootstrap Functions: 
\code{\link{ci_bca_helper}()},
\code{\link{ci_bca}()},
\code{\link{ci_bc}()},
\code{\link{ci_pc}()},
\code{\link{func_jackknife}()},
\code{\link{func_xstar_nb}()},
\code{\link{jackknife}()},
\code{\link{thetahatstar_nb}()},
\code{\link{xstar_nb}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{Nonparametric Bootstrap Functions}
\keyword{nBootstrap}
