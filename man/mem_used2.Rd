% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mem_used2.R
\name{mem_used2}
\alias{mem_used2}
\title{How much memory is presently/maximally used by R?}
\usage{
mem_used2(maximum = FALSE)
}
\arguments{
\item{maximum}{locical, whether to show the maximum memory that R used.
Default is FALSE, which means the memory that R currently used.}
}
\description{
This function is inspired by pryr::mem_used function.
}
\examples{
\dontrun{
mem_use2()
mem_use2(TRUE)

x = matrix(1.2, nrow = 10000, ncol = 1000)
mem_used2()
mem_used2(TRUE)

# After removing x, current memory recover but the maximum memory does not.
rm(x)
mem_used2()
mem_used2(TRUE)
}
}
