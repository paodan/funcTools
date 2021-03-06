% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rQsub.R
\name{rQsub}
\alias{rQsub}
\title{Submit (qsub) R jobs.}
\usage{
rQsub(
  path = getwd(),
  rFile = "testQsub.R",
  jobName = "job",
  threaded = 1,
  memoryG = 10,
  rTimeHour = 2,
  logFile = "logfilename.log",
  email = NULL,
  when2Email = "aes",
  computeNode = "all.q@n00[0-9][0-9]*",
  moreQsubParams = "",
  preCMD = "echo \\"module load R/3.3.0 && Rscript ",
  param1 = 1:10,
  ...
)
}
\arguments{
\item{path}{file path.}

\item{rFile}{the R script to run.}

\item{jobName}{the job name(s).}

\item{threaded}{integer or integer vector to specify
the number of threads of each job.}

\item{memoryG}{numeric or numeric vector to specify the
memory (in Gb) of each job.}

\item{rTimeHour}{numeric or numeric vector. Maximum running
time (in hour) of each job.}

\item{logFile}{the log file(s) for errors and warnings.}

\item{email}{email to receive report from HPC. The default is NULL.}

\item{when2Email}{when will the server send a email. This can be
"b" (start) and/or "e" (end) and/or "a" (abortion) and/or "s" (suspension)
of your job. The default is "aes". This parameter will not be
used if email is NULL.}

\item{computeNode}{which computing CPU node will be used?
Support regular expression. The default is \code{"all.q@n00[0-9][0-9]*"}}

\item{moreQsubParams}{more qsub \href{https://wiki.bioinformatics.umcutrecht.nl/bin/viewauth/HPC/HowToS}{parameters}.}

\item{preCMD}{the command to run R script, the default is
'echo "module load R/3.3.0 && Rscript '.}

\item{param1}{the first parameter passed to R script.}

\item{...}{other parameter passed to R script.}
}
\value{
A list of output of running rFile.
}
\description{
Submit R jobs in parallel (array).
}
\examples{
\dontrun{
# This function can only run on HPC.

## Section 1, the format of R script file (for example myfile.R) to submit:
args = commandArgs(TRUE)
if (FALSE){
 # Input the code to use rQsub, see section
 # And run the code manually in this if`{}` statement , for example:
 rQsub(path = getwd(), rFile = "myfile.R", param1 = 1:5)
}
arg1 = args[1]
# Then input R code to use the arg1 variable. for example:
cat("test code result:", arg1, "\n")

## Section 2, the code to use rQsub
# One parameter in R script
path = "/my/file/path"
rQsub(path = "/my/file/path",
      rFile = paste0(path, "/testQsub.R"),
      jobName = "job", threaded = 1,
      memoryG = 10, rTimeHour = 2,
      logFile = paste0(path, "/logfilename.log"),
      preCMD = 'echo "module load R/3.3.0 && Rscript ',
      param1 = 1:10)
# Multiple parameters in R script and using 2 threads
rQsub(path = "/my/file/path",
      rFile = paste0(path, "/testQsub.R"),
      jobName = "job", threaded = 2,
      param1 = 1:10, param2 = 2:11, param3 = 3:12)

# Specify job names, threads, memories and running time.
rQsub(path = "/my/file/path",
      rFile = paste0(path, "/testQsub.R"),
      jobName = paste0("job_", formatN(1:10, 2)),
      logFile = paste0(path, "/logfilename_", formatN(1:10, 2), ".log"),
      threaded = rep(c(1, 2), each = 5),
      memoryG = rep(c(10, 20), each = 5),
      rTimeHour = rep(c(24, 48), each = 5),
      param1 = 1:10)
}
}
