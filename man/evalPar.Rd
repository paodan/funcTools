% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evalPar.R
\name{evalPar}
\alias{evalPar}
\title{Evaluate parameters in one goal in the global environment\cr}
\usage{
evalPar(..., .prm = NULL, envir = parent.frame(), returnValue = TRUE)
}
\arguments{
\item{...}{these arguments are of the form tag = value.}

\item{.prm}{named list of arguments to use}

\item{envir}{the environment in where parameters are to be evaluated.}
}
\value{
Returns a list of parameters that have been changed.
}
\description{
Return a list of of parameters that changed, and
change the corresponding value in the global environment.
}
\examples{
{
\dontrun{
evalPar(a = list(x = c(1,2,5), y = 3:5),
c = NULL, e = numeric())

# use .prm argument
tmp = list(a = list(x = c(1,2,5), y = 3:5))
evalPar(.prm = list(prm0 = tmp, a = 1, c = NULL,
e = numeric(), e2 = 1:3))
}}

}
