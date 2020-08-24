####### To be finished *******

#' Submit (sbatch, like qsub) R jobs.
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
#' @param email email to receive report from HPC. The default is NULL.
#' @param when2Email when will the server send a email. 
#' Valid values are NONE, BEGIN, END, FAIL, REQUEUE, ALL (equivalent 
#' to BEGIN, END, FAIL, REQUEUE, and STAGE_OUT), STAGE_OUT  (burst buffer
#' stage  out  and  teardown completed), TIME_LIMIT, TIME_LIMIT_90 (reached 90 
#' percent of time limit), TIME_LIMIT_80 (reached 80 percent of time limit), 
#' TIME_LIMIT_50 (reached 50  percent  of  time  limit) and ARRAY_TASKS (send 
#' emails for each array task). Multiple values may be specified in a 
#' comma separated list. The default is "END,FAIL,STAGE_OUT,TIME_LIMIT". For
#' compatibility, when2Email="aes" is allowed, 
#' meaning when2Email="END,FAIL,STAGE_OUT,TIME_LIMIT".
#' 
#' 
#' @param computeNode which computing CPU node will be used?
#' Support regular expression. The default is NULL, meaning the node is not 
#' specified. For compatibility, computeNode = \code{"all.q@@n00[0-9][0-9]*"} is 
#' equivalent to computeNode = NULL.
#' @param moreQsubParams more sbatch \href{https://slurm.schedmd.com/sbatch.html}{parameters}.
#' @param param1 the first parameter passed to R script.
#' @param ... other parameter passed to R script.
#' @return A list of output of running rFile.
#' @export
#' @examples
#' \dontrun{
#' # This function can only run on HPC with slurm.
#'
#' ## Section 1, the format of R script file (for example myfile.R) to submit:
#' args = commandArgs(TRUE)
#' if (FALSE){
#'  # Input the code to use rQsub, see section
#'  # And run the code manually in this if`{}` statement , for example:
#'  rQsub2(path = getwd(), rFile = "myfile.R", param1 = 1:5)
#' }
#' arg1 = args[1]
#' # Then input R code to use the arg1 variable. for example:
#' cat("test code result:", arg1, "\n")
#'
#' ## Section 2, the code to use rQsub
#' # One parameter in R script
#' path = "/my/file/path"
#' rQsub2(path = "/my/file/path",
#'       rFile = paste0(path, "/testQsub.R"),
#'       jobName = "job", threaded = 1,
#'       memoryG = 10, rTimeHour = 2,
#'       logFile = paste0(path, "/logfilename.log"),
#'       preCMD = 'module load R/3.3.0 && Rscript ',
#'       param1 = 1:10)
#' # Multiple parameters in R script and using 2 threads
#' rQsub2(path = "/my/file/path",
#'       rFile = paste0(path, "/testQsub.R"),
#'       jobName = "job", threaded = 2,
#'       param1 = 1:10, param2 = 2:11, param3 = 3:12)
#'
#' # Specify job names, threads, memories and running time.
#' rQsub2(path = "/my/file/path",
#'       rFile = paste0(path, "/testQsub.R"),
#'       jobName = paste0("job_", formatN(1:10, 2)),
#'       logFile = paste0(path, "/logfilename_", formatN(1:10, 2), ".log"),
#'       threaded = rep(c(1, 2), each = 5),
#'       memoryG = rep(c(10, 20), each = 5),
#'       rTimeHour = rep(c(24, 48), each = 5),
#'       param1 = 1:10)
#' }

rQsub2 = function(path = getwd(), rFile = "testQsub.R",
                 jobName = "job", threaded = 1, memoryG = 10, rTimeHour = 2, 
                 logFile = "logfilename.log", email = NULL,
                 when2Email = "END,FAIL,STAGE_OUT,TIME_LIMIT",
                 computeNode = NULL,
                 moreQsubParams = "",
                 preCMD = 'module load R/3.3.0 && Rscript ',
                 param1 = 1:10,
                 ...){
  params = list(param1, ...) # parameters passed to R
  num = unique(sapply(params, length))
  if (length(unique(sapply(params, length))) != 1){
    stop("The length of arguments are not the same!")
  }
  
  if(!is.null(computeNode)){
    stopifnot(length(computeNode) == 1 && is.character(computeNode))
    if(computeNode == "all.q@n00[0-9][0-9]*"){
      computeNode = NULL
    }
    computeNode = paste("-w", computeNode)
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
  memory = paste0("--mem=", memoryG, "G")
  # time
  rTimeHour = checkNum(rTimeHour, num)
  rTime = paste0("--time=", rTimeHour, ":00:00")
  # jobName
  jobName = checkNum(jobName, num)
  # logFile
  logFile = checkNum(logFile, num)
  # threaded
  threaded = checkNum(threaded, num)
  
  emailTypes = c('NONE', 'BEGIN', 'END', 'FAIL', 'REQUEUE', 'ALL', 
                 'STAGE_OUT', 'TIME_LIMIT', 'TIME_LIMIT_90', 
                 'TIME_LIMIT_80', 'TIME_LIMIT_50', 'ARRAY_TASKS', "aes")
  if (is.null(email)){
    emailAndWhen = ""
  } else {
    stopifnot(is.character(when2Email) && 
                length(when2Email) ==1 &&
                all(strsplit(when2Email, ",")[[1]] %in% c(emailTypes))
    )
    if (when2Email == "aes"){
      when2Email = "END,FAIL,STAGE_OUT,TIME_LIMIT"
    }
    emailAndWhen = paste("--mail-user=", email, "--mail-type=", when2Email)
  }
  
  qparam = paste("sbatch", computeNode, "-J", jobName, "-c", threaded,
                 memory, rTime, 
                 "-e", logFile, "-o", paste0(logFile, ".out"),
                 emailAndWhen, moreQsubParams)
  qsub_all_parameters = paste0(logFile[1], "_sbatch_all_parameters.log")
  fileConn = file(qsub_all_parameters)
  on.exit(close(fileConn))
  writeLines(qparam, fileConn)
  
  paramInR = unlist(lapply(1:num, function(x){
    paste(lapply(params, "[", x), collapse = " ")
  }))
  ids = list()
  for(i in 1:num){
    cat (rFile, "\n", paramInR[i], "\n")
    cmd = paste('echo -e "#!/bin/bash\n', preCMD, rFile, 
                paramInR[i], '" |', qparam[i])
    cat(i, ":", cmd, "\n")
    ids = c(ids, system(cmd, intern = TRUE, wait = TRUE))
  }
  return(ids)
}




#' Status (sacct) of SLURM. jobs.
#' @param stat the job status, including "running", "completed", "failed", 
#' "timeout", "resizing", "deadline", "node_fail".
#' @export
#' @seealso \code{\link{qstat}}
qstat2 = function(stat = c("running", "completed", "failed", "timeout", 
                           "resizing", "deadline", "node_fail"), 
                  groups = NULL, users = NULL){
  
  stopifnot(all(stat %in% c("running", "completed", "failed", "timeout", 
                            "resizing", "deadline", "node_fail")))
  # running (r),
  # completed (cd), failed (f), timeout (to), resizing (rs),
  # deadline (dl) and node_fail (nf)
  stats = c(running = "r", completed = "cd", failed ="f", timeout = "to", 
            resizing = "rs", deadline = "dl", node_fail = "nf")
  s = stats[stat]
  
  cmd2 = paste0("sacct -s ", paste0(s, collapse  = " -s "))
  
  if(!is.null(groups)){
    cmd2 = paste0(cmd2, " -A ", paste0(groups, collapse = ","))
  }
  if(is.null(users)){
    cmd2 = paste0(cmd2, " -a ")
  } else {
    cmd2 = paste0(cmd2, " -u ", paste0(users, collapse = ","))
  }
  
  cmd2 = paste0(cmd2, " -o JobID,JobName,User,Account,UID,Timelimit,", 
                "Submit,Start,Elapsed,NCPUS,NNodes,AllocTRES,NodeList,", 
                # "NTasks,", 
                "Partition,ReqMem,State,WorkDir")
  
  res0 = system(cmd2, intern = TRUE)
  res1 = grep("^[0-9]+ ", res0, value = TRUE)
  res2 = removeSpace(res1)
  
  res = as.data.frame(strSplit(res2, " "), stringsAsFactors = FALSE)
  colnames(res) = as.vector(strSplit(removeSpace(res0[1]), " "))
  
  numID = c("JobID", "UID", "NCPUS", "NNodes")
  for(mi in numID){
    res[[mi]] = as.numeric(res[[mi]])
  }
  res$ReqMem = as.numeric(sub("Gn", "", res$ReqMem))
  return(res)
}


####### To be finished **********
qstat3 = function(stat = c("all", "run", "wait"), 
                  groups = NULL, users = NULL){
  res0 = system("squeue -o %all", intern = T)
  res1= as.data.frame(strSplit(tail(res0, -1), "\\|"), stringsAsFactors = FALSE)
  colnames(res1) = as.vector(strSplit(head(res0, 1), "\\|"))
  return(res1)
}

#' Delete (scancel) of SLURM jobs.
#' @param id The job ID.
#' @seealso \code{\link{qdelAll2}}
#' @export
qdel2 = function(id){
  # cmd = paste("scancel", paste(id, collapse = " "))
  cmd = paste("scancel", id)
  res = system(cmd, intern = TRUE)
  return(res)
}


# To be finished ****
hpcInfo2 = function(){
  res0 = system("sinfo -o %all", TRUE)
  res1 = as.data.frame(strSplit(res0[-1], "\\|"), stringsAsFactors = FALSE)
  colnames(res1) = as.vector(strSplit(gsub(" ", "", res0[1]), "\\|"))
  numIDs = c("CPUS", "TMP_DISK", "FREE_MEM", "MEMORY", "PRIO_TIER", 
             "NODES", "SOCKETS", "CORES", "THREADS")
  for(mi in numIDs){
    res1[[mi]] = as.numeric(res1[[mi]])
    if(mi %in% c("FREE_MEM", "MEMORY")){
      res1[[mi]] = res1[[mi]]/10^3
    }
  }
  return(res1)
}
