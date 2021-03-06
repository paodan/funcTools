% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rQsub_slurm.R
\name{qstatAll2}
\alias{qstatAll2}
\title{checking all users' job status on HPC for slurm system}
\usage{
qstatAll2(
  stat = c("run", "all", "wait", "COMPLETED", "TIMEOUT", "FAILED", "OUT_OF_MEMORY",
    "CANCELLED")
)
}
\arguments{
\item{stat}{status, one of "run", "all", "wait", 
"COMPLETED", "TIMEOUT", "FAILED", "OUT_OF_MEMORY", and "CANCELLED".}
}
\description{
checking all users' job status on HPC for slurm system
}
\examples{
\dontrun{
qstatAll2("all")
qstatAll2("run")
qstatAll2("TIMEOUT")
}
}
