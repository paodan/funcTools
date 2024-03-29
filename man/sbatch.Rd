% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rQsub_slurm.R
\name{sbatch}
\alias{sbatch}
\title{sbatch: Submit (sbatch, like qsub) R jobs.}
\usage{
sbatch(
  path = getwd(),
  rFile = "testSbatch.R",
  jobName = "job",
  threaded = 1,
  memoryG = 10,
  rTimeHour = 2,
  logFile = "logfilename.log",
  email = NULL,
  when2Email = "END,FAIL,STAGE_OUT,TIME_LIMIT",
  computeNode = NULL,
  moreQsubParams = "",
  array = 1:10,
  dependency = FALSE,
  preCMD = "module load R/3.5.1 && Rscript ",
  param1 = NULL,
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

\item{when2Email}{when will the server send a email. 
Valid values are NONE, BEGIN, END, FAIL, REQUEUE, ALL (equivalent 
to BEGIN, END, FAIL, REQUEUE, and STAGE_OUT), STAGE_OUT  (burst buffer
stage  out  and  teardown completed), TIME_LIMIT, TIME_LIMIT_90 (reached 90 
percent of time limit), TIME_LIMIT_80 (reached 80 percent of time limit), 
TIME_LIMIT_50 (reached 50  percent  of  time  limit) and ARRAY_TASKS (send 
emails for each array task). Multiple values may be specified in a 
comma separated list. The default is "END,FAIL,STAGE_OUT,TIME_LIMIT". For
compatibility, when2Email="aes" is allowed, 
meaning when2Email="END,FAIL,STAGE_OUT,TIME_LIMIT".}

\item{computeNode}{which computing CPU node will be used?
Support regular expression. The default is NULL, meaning the node is not 
specified. For compatibility, computeNode = \code{"all.q@n00[0-9][0-9]*"} is 
equivalent to computeNode = NULL.}

\item{moreQsubParams}{more sbatch \href{https://slurm.schedmd.com/sbatch.html}{parameters}.}

\item{array}{either integers or character indicating array IDs, for example 1:10, "1-10", "1-10,100,1000".}

\item{dependency}{logical, If true, this job can only be run after the previous job 
named by jobName is finished.}

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
\details{
SLURM_ARRAY_TASK_ID is the variable in the systeim environment that can be used in R scripts.
}
\examples{
\dontrun{
# This function can only run on HPC with slurm.

#######################################################
# For the compatiblity of rQsub2 function
## Section 1, the format of R script file (for example myfile.R) to submit:
args = commandArgs(TRUE)
if (FALSE){
 # Input the code to use sbatch/rQsub, see section 2
 # And run the code manually in this if`{}` statement , for example:
 sbatch(path = getwd(), rFile = "myfile.R", param1 = 1:5)
}
arg1 = args[1]
# Then input R code to use the arg1 variable. for example:
cat("test code result:", arg1, "\n")

## Section 2, the code to use rQsub
# One parameter in R script
path = "/my/file/path"
sbatch(path = path,
      rFile = paste0(path, "/testQsub.R"),
      jobName = "job", threaded = 1,
      memoryG = 10, rTimeHour = 2,
      logFile = paste0(path, "/logfilename.log"),
      preCMD = 'module load R/3.3.0 && Rscript ',
      param1 = 1:10)
# Multiple parameters in R script and using 2 threads
sbatch(path = path,
      rFile = paste0(path, "/testQsub.R"),
      jobName = "job", threaded = 2,
      param1 = 1:10, param2 = 2:11, param3 = 3:12)

# Specify job names, threads, memories and running time.
sbatch(path = path,
      rFile = paste0(path, "/testQsub.R"),
      jobName = paste0("job_", formatN(1:10, 2)),
      logFile = paste0(path, "/logfilename_", formatN(1:10, 2), ".log"),
      threaded = rep(c(1, 2), each = 5),
      memoryG = rep(c(10, 20), each = 5),
      rTimeHour = rep(c(24, 48), each = 5),
      param1 = 1:10)



##############################
#' ## Section 1, the format of R script file to submit:
if (FALSE){
# Run the code manually in this if`{}` statement , for example:
pathFile = rstudioapi::getActiveDocumentContext()
path = dirname(pathFile$path)
Rfile = pathFile$path
sbatch(path = path, rFile = Rfile, array = 1:5)
# See section 2 for mor examples.
}

arg1 = Sys.getenv("SLURM_TASK_ID")
arg1 = as.numeric(arg1)
# Then input R code to use the arg1 variable. for example:
cat("test code result:", arg1, "\n")
Sys.sleep(200)

## Section 2, the code to use rQsub
sbatch(path = path, rFile = Rfile,
      jobName = "job", threaded = 2,
      memoryG = 10, rTimeHour = 1.3,
      logFile = paste0(path, "/logfilename.log"),
      array = 1:3,
      preCMD = 'module load R/3.5.1 && Rscript ',
      param1 = NULL)

# Run slurm job1 when previous job (also named "job1") is finished
sbatch(path = path, rFile = Rfile,
      jobName = "job1", threaded = 2,
      memoryG = 10, rTimeHour = 2,
      logFile = paste0(path, "/logfilename.log"),
      array = 1,
      dependency = TRUE,
      preCMD = 'module load R/3.5.1 && Rscript ',
      param1 = NULL)
}
}
