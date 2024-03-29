% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rQsub_slurm.R
\name{scancelAll}
\alias{scancelAll}
\title{scancelAll: Delete (scancel) of SLURM jobs.}
\usage{
scancelAll(pattern = "*", col = 1)
}
\arguments{
\item{pattern}{regular expression pattern. The default is "*".}

\item{col}{the index of column to match the pattern. The default is 1.}
}
\description{
scancelAll: Delete (scancel) of SLURM jobs.
}
\seealso{
\code{\link{qdel2}}
}
