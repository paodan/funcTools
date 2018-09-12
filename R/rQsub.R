#' Submit (qsub) R jobs.
#' @description Submit R jobs in parallel (array).
#' @param path file path.
#' @param rFile the R script to run.
#' @param preCMD the command to run R script, the default is
#' 'echo "module load R/3.3.0 && Rscript '.
#' @param jobName the job name.
#' @param threaded the number of threads.
#' @param memoryG memory in Gb.
#' @param rTimeHour maximum running time in hour.
#' @param logFile the log file for errors and warnings.
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
#' }
#' }
#' @export
rQsub = function(path = getwd(),
                 rFile = paste0(path, "/testQsub.R"),
                 jobName = "job",
                 threaded = 1,
                 memoryG = 10,
                 rTimeHour = 2,
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

  # memory and time
  memory = paste0("h_vmem=", memoryG, "G")
  rTime = paste0("h_rt=", rTimeHour, ":00:00")

  qparam = paste("qsub -N", jobName, "-pe threaded", threaded,
                 "-l", memory, "-l", rTime, "-e", logFile,
                 "-M", email, "-m aes -cwd")
  paramInR = unlist(lapply(1:num, function(x){
    paste(lapply(params, "[", x), collapse = " ")
  }))
  ids = list()
  for(i in 1:num){
    cat (rFile, "\n", paramInR[i], "\n")
    cmd = paste0(preCMD, rFile, ' ', paramInR[i], '" | ', qparam)
    cat(i, ":", cmd, "\n")
    ids = c(ids, system(cmd, intern = TRUE, wait = TRUE))
  }

  return(ids)
}


#' Status (qstat) of running jobs of all users.
#' @param stat the job status, including "all", "run", and "wait".
#' @return a data frame of job details, including job ID, prior, name, user,
#' submit.start, at, queue, slots, and ja.task.ID.
#' @seealso \code{\link{qstat}}, \code{\link{qstatGroupAll}}
#' @export
qstatAll = function(stat = c("all", "run", "wait")){
  stat = match.arg(stat)
  if (stat != "all"){
    if (stat == "run"){
      cmd = "qstat -u \\* -s r"
    } else if (stat == "wait"){
      cmd = "qstat -u \\* -s p"
    } else {
      stop("Unknown stat!")
    }
    res0 = gsub("^ *|(?<= ) | *$", "", system(cmd, intern = TRUE), perl = TRUE)
    res = strSplit(res0[-2], " ")
    colnames(res) = res[1,]
    res = res[-1,,drop = FALSE]
  } else {
    res = rbind(qstatAll("run"), qstatAll("wait"))
  }
  return(as.data.frame(res))
}

#' Status (qstat) of running jobs of group members.
#' @param stat the job status, including "all", "run", and "wait".
#' @param groupMembers the group member name.
#' @seealso \code{\link{qstat}}, \code{\link{qstatAll}}
#' @export
qstatGroupAll = function(stat = c("all", "run", "wait"),
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
#' @param stat the job status, including "all", "run", and "wait".
#' @seealso \code{\link{qstatAll}}, \code{\link{qstatGroupAll}}
#' @export
qstat = function(stat = c("all", "run", "wait")){
  stat = match.arg(stat)
  if (stat == "run"){
    cmd = "qstat -s r"
  } else if (stat == "wait"){
    cmd = "qstat -s p"
  } else if (stat == "all"){
    cmd = "qstat"
  }
  # res0 = gsub("^ *|(?<= ) | *$", "", system(cmd, intern = TRUE), perl = TRUE)
  res0 = removeSpace(system(cmd, intern = TRUE))
  res = strSplit(res0[-2], " ")
  colnames(res) = res[1,]
  res = res[-1,,drop = FALSE]
  return(data.frame(res))
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
