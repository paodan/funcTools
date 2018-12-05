#' Submit (qsub) R jobs.
#' @description Submit R jobs in parallel (array).
#' @param path file path.
#' @param rFile the R script to run.
#' @param preCMD the command to run R script, the default is
#' 'echo "module load R/3.3.0 && Rscript '.
#' @param jobName the job name(s).
#' @param threaded integer or integer vector to specify
#' the number of threads of each job.
#' @param memoryG numeric or numeric vector to specify the
#' memory (in Gb) of each job.
#' @param rTimeHour numeric or numeric vector. Maximum running
#' time (in hour) of each job.
#' @param logFile the log file(s) for errors and warnings.
#' @param email email to receive report from HPC.
#' @param param1 the first parameter passed to R script.
#' @param ... other parameter passed to R script.
#' @return A list of output of running rFile.
#' @examples {
#' \dontrun{
#' # This function can only run on HPC
#' # One parameter in R script
#' rQsub(path = "/my/file/path",
#'       rFile = paste0(path, "/testQsub.R"),
#'       jobName = "job", threaded = 1,
#'       memoryG = 10, rTimeHour = 2,
#'       logFile = paste0(path, "/logfilename.log"),
#'       preCMD = 'echo "module load R/3.3.0 && Rscript ',
#'       param1 = 1:10)
#' # Multiple parameters in R script and using 2 threads
#' rQsub(path = "/my/file/path",
#'       rFile = paste0(path, "/testQsub.R"),
#'       jobName = "job", threaded = 2,
#'       param1 = 1:10, param2 = 2:11, param3 = 3:12)
#'
#' # Specify job names, threads, memories and running time.
#' rQsub(path = "/my/file/path",
#'       rFile = paste0(path, "/testQsub.R"),
#'       jobName = paste0("job_", formatN(1:10, 2)),
#'       logFile = paste0(path, "/logfilename_", formatN(1:10, 2), ".log"),
#'       threaded = rep(c(1, 2), each = 5),
#'       memoryG = rep(c(10, 20), each = 5),
#'       rTimeHour = rep(c(24, 48), each = 5),
#'       param1 = 1:10)
#' }
#' }
#' @export
rQsub = function(path = getwd(), rFile = paste0(path, "/testQsub.R"),
                 jobName = "job",
                 threaded = 1, memoryG = 10, rTimeHour = 2,
                 logFile = paste0(path, "/logfilename.log"),
                 email = "wtao@umcutrecht.nl",
                 preCMD = 'echo "module load R/3.3.0 && Rscript ',
                 param1 = 1:10,
                 ...){
  params = list(param1, ...) # parameters passed to R
  num = unique(sapply(params, length))
  if (length(unique(sapply(params, length))) != 1){
    stop("The length of arguments are not the same!")
  }

  checkNum = function(x, num = length(param1)){
    if (length(x) == 1){
      x = rep(x, num)
    } else if (length(x) < 1 || length(x) != num){
      stop("The length of ", deparse(substitute(x)),
           " must be 1 or the same as 'param1': ", num)
    }
    x
  }

  # memory
  memoryG = checkNum(memoryG, num)
  memory = paste0("h_vmem=", memoryG, "G")
  # time
  rTimeHour = checkNum(rTimeHour, num)
  rTime = paste0("h_rt=", rTimeHour, ":00:00")
  # jobName
  jobName = checkNum(jobName, num)
  # logFile
  logFile = checkNum(logFile, num)
  # threaded
  threaded = checkNum(threaded, num)

  qparam = paste("qsub -N", jobName, "-pe threaded", threaded,
                 "-l", memory, "-l", rTime, "-e", logFile,
                 "-M", email, "-m aes -cwd")
  qsub_all_parameters = paste0(logFile[1], "_qsub_all_parameters.log")
  fileConn = file(qsub_all_parameters)
  on.exit(close(fileConn))
  writeLines(qparam, fileConn)

  paramInR = unlist(lapply(1:num, function(x){
    paste(lapply(params, "[", x), collapse = " ")
  }))
  ids = list()
  for(i in 1:num){
    cat (rFile, "\n", paramInR[i], "\n")
    cmd = paste0(preCMD, rFile, ' ', paramInR[i], '" | ', qparam[i])
    cat(i, ":", cmd, "\n")
    ids = c(ids, system(cmd, intern = TRUE, wait = TRUE))
  }
  return(ids)
}



#' process the results form qstat command
#' @param statRes the results from `system("qstat -r", TRUE)`.
#' @return a data.frame of tidy results
#' @import chron
#' @export
#' @examples
#' \dontrun{
#' x = system("qstat -r", T)
#' qstatProcess(x)
#' }
qstatProcess = function(statRes){
  if (length(statRes) == 0){
    message("No queue job was found!")
    return(character(0))
  }
  f = function(x, pat, fill = ""){
    y = grep(pat, x, value = TRUE)
    if (length(y) == 0) {
      y = fill
    }
    gsub(pat, "", y)
  }
  statRes = removeSpace(statRes)

  len = length(statRes)
  id1 = grep("^[0-9]* [0-9]\\.[0-9]*", statRes)
  id2 = c(id1[-1]-1, len)
  res0 = lapply(seq_along(id1), function(mi){
    statResSub = statRes[id1[mi] : id2[mi]]
    return(c(grep("^[0-9]* [0-9]\\.[0-9]*", statResSub, value = TRUE),
             f(statResSub, "^Full jobname: "),
             f(statResSub, "^h_rt=", fill = 24*3600),
             f(statResSub, "^Hard Resources: "),
             f(statResSub, "^Requested PE: ", fill = "threaded 1"),
             f(statResSub, "^Binding: "),
             f(statResSub, "^Soft Resources:")))
  })
  if (length(res0) < 1){
    cat("No jobs are on HPC based on your request!\n")
    return(invisible(NULL))
  }
  res0 = res0[sapply(res0, length) == 7]
  res0 = as.data.frame(t(as.data.frame(res0)))
  colnames(res0) = c("base", "fullName", "maxSeconds", "hardResources",
                     "requestedPE", "binding", "softResources")

  res = data.frame(strSplit(res0$base, " "), stringsAsFactors = FALSE)
  colnames(res) = c("job.ID", "prior", "name", "user", "state", "submit.start.at",
                    "at", "queue", "slots", "ja.task.ID")[1:ncol(res)]
  res$slots = as.numeric(res$slots)
  res$submit.start.at = as.Date(res$submit.start.at, "%m/%d/%Y")
  res$at = chron::chron(times. = res$at, format = c(dataes = "m/d/y", time = "h:m:s"))
  res$submit.start.at = as.POSIXlt(paste(res$submit.start.at, res$at))

  if (ncol(res) == 9){
    res$ja.task.ID = ""
  }
  res$at = NULL

  # additional information
  res$name = res0$fullName
  res$maxSeconds = as.numeric(strSplit(res0$maxSeconds, " ")[,1])
  res$will.stop.at = res$submit.start.at + res$maxSeconds
  res$hardResources = res0$hardResources
  res$requestedPE = res0$requestedPE
  res$binding = res0$binding
  res$softResources = res0$softResources
  return(res)
}


#' Status (qstat) of running jobs of all users.
#' @param stat the job status, including "run" (the default),
#' "all", and "wait".
#' @return a data frame of job details, including job ID, prior, name, user,
#' submit.start, at, queue, slots, and ja.task.ID.
#' @seealso \code{\link{qstat}}, \code{\link{qstatGroupAll}}
#' @export
qstatAll = function(stat = c("run", "all", "wait")){
  stat = match.arg(stat)
  if (stat == "all"){
    cmd2 = "qstat -u \\* -s rp -r"
  } else if (stat == "run"){
    cmd2 = "qstat -u \\* -s r -r"
  } else if (stat == "wait"){
    cmd2 = "qstat -u \\* -s p -r"
  } else {
    stop("Unknown stat!")
  }

  return(qstatProcess(statRes = system(cmd2, intern = TRUE)))
}


#' Status (qstat) of running jobs of group members.
#' @param stat the job status, including "run" (the default),
#' "all", and "wait".
#' @param groupMembers the group member name.
#' @seealso \code{\link{qstat}}, \code{\link{qstatAll}}
#' @export
qstatGroupAll = function(stat = c("run", "all", "wait"),
                         groupMembers = system("ls /hpc/dla_lti", intern = TRUE)){
  stat = match.arg(stat)
  res0 = qstatAll(stat)
  res = subset(res0, user %in% groupMembers)
  if (nrow(res) > 0) rownames(res) = 1:nrow(res)
  return(res)
}

#' remove multiple spaces to one space
#' @param x a vector of string.
#' @export
removeSpace = function(x){
  return(gsub("^ *|(?<= ) | *$", "", x, perl = TRUE))
}

#' Add leading zeros
#' @param x a vector of string or number.
#' @param n final number of charactors.
#' @import stringr
#' @export
formatN = function(x, n = 3){
  stringr::str_pad(string = x, width = n, pad = "0")
}


#' Status (qstat) of my running jobs.
#' @param stat the job status, including "all" (the default),
#' "run", and "wait".
#' @seealso \code{\link{qstatAll}}, \code{\link{qstatGroupAll}}
#' @export
qstat = function(stat = c("all", "run", "wait")){
  stat = match.arg(stat)

  if (stat == "run"){
    # cmd = "qstat -s r"
    cmd2 = "qstat -s r -r"
  } else if (stat == "wait"){
    # cmd = "qstat -s p"
    cmd2 = "qstat -s p -r"
  } else if (stat == "all"){
    # cmd = "qstat"
    cmd2 = "qstat -s rp -r"
  } else {
    stop("Unknown stat!")
  }
  return(qstatProcess(statRes = system(cmd2, intern = TRUE)))
}


#' Delete (qdel) of queue jobs.
#' @param id The job IDs.
#' @seealso \code{\link{qdelAll}}
#' @export
qdel = function(id){
  cmd = paste("qdel", paste(id, collapse = " "))
  res = system(cmd, intern = TRUE)
  return(res)
}

#' Delete (qdel) of job by pattern
#' @param pattern The pattern to match.
#' @param col The column index to match pattern.
#' @return Deleted job IDs.
#' @seealso \code{\link{qdel}}
#' @export
qdelAll = function(pattern = "*", col = 1){
  q = qstat("all")
  id = as.character(q[,col])
  indx = grep(pattern, id)
  if (length(indx) > 0){
    qUser = q[indx,]
    id2Del = as.character(qUser[,1])
    qdel(id2Del)
  } else {
    cat("No jobs matched!")
    id2Del = NULL
  }
  return(id2Del)
}
