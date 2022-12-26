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
    emailAndWhen = paste0("--mail-user=", email, " --mail-type=", when2Email)
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
#' 
#' @param array either integers or character indicating array IDs, for example 1:10, "1-10", "1-10,100,1000".
#' @param dependency logical, If true, this job can only be run after the previous job 
#' named by jobName is finished.
#' @param moreQsubParams more sbatch \href{https://slurm.schedmd.com/sbatch.html}{parameters}.
#' @param param1 the first parameter passed to R script.
#' @param ... other parameter passed to R script.
#' @return A list of output of running rFile.  
#' @details SLURM_ARRAY_TASK_ID is the variable in the systeim environment that can be used in R scripts.
#' @export
#' @examples
#' \dontrun{
#' # This function can only run on HPC with slurm.

#'
#' #######################################################
#' # For the compatiblity of rQsub2 function
#' ## Section 1, the format of R script file (for example myfile.R) to submit:
#' args = commandArgs(TRUE)
#' if (FALSE){
#'  # Input the code to use sbatch/rQsub, see section 2
#'  # And run the code manually in this if`{}` statement , for example:
#'  sbatch(path = getwd(), rFile = "myfile.R", param1 = 1:5)
#' }
#' arg1 = args[1]
#' # Then input R code to use the arg1 variable. for example:
#' cat("test code result:", arg1, "\n")
#'
#' ## Section 2, the code to use rQsub
#' # One parameter in R script
#' path = "/my/file/path"
#' sbatch(path = path,
#'       rFile = paste0(path, "/testQsub.R"),
#'       jobName = "job", threaded = 1,
#'       memoryG = 10, rTimeHour = 2,
#'       logFile = paste0(path, "/logfilename.log"),
#'       preCMD = 'module load R/3.3.0 && Rscript ',
#'       param1 = 1:10)
#' # Multiple parameters in R script and using 2 threads
#' sbatch(path = path,
#'       rFile = paste0(path, "/testQsub.R"),
#'       jobName = "job", threaded = 2,
#'       param1 = 1:10, param2 = 2:11, param3 = 3:12)
#'
#' # Specify job names, threads, memories and running time.
#' sbatch(path = path,
#'       rFile = paste0(path, "/testQsub.R"),
#'       jobName = paste0("job_", formatN(1:10, 2)),
#'       logFile = paste0(path, "/logfilename_", formatN(1:10, 2), ".log"),
#'       threaded = rep(c(1, 2), each = 5),
#'       memoryG = rep(c(10, 20), each = 5),
#'       rTimeHour = rep(c(24, 48), each = 5),
#'       param1 = 1:10)
#' 
#' 
#' 
#' ##############################
#' #' ## Section 1, the format of R script file to submit:
#' if (FALSE){
#' # Run the code manually in this if`{}` statement , for example:
#' pathFile = rstudioapi::getActiveDocumentContext()
#' path = dirname(pathFile$path)
#' Rfile = pathFile$path
#' sbatch(path = path, rFile = Rfile, array = 1:5)
#' # See section 2 for mor examples.
#' }
#' 
#' arg1 = Sys.getenv("SLURM_TASK_ID")
#' arg1 = as.numeric(arg1)
#' # Then input R code to use the arg1 variable. for example:
#' cat("test code result:", arg1, "\n")
#' Sys.sleep(200)
#'
#' ## Section 2, the code to use rQsub
#' sbatch(path = path, rFile = Rfile,
#'       jobName = "job", threaded = 2,
#'       memoryG = 10, rTimeHour = 1.3,
#'       logFile = paste0(path, "/logfilename.log"),
#'       array = 1:3,
#'       preCMD = 'module load R/3.5.1 && Rscript ',
#'       param1 = NULL)
#' 
#' # Run slurm job1 when previous job (also named "job1") is finished
#' sbatch(path = path, rFile = Rfile,
#'       jobName = "job1", threaded = 2,
#'       memoryG = 10, rTimeHour = 2,
#'       logFile = paste0(path, "/logfilename.log"),
#'       array = 1,
#'       dependency = TRUE,
#'       preCMD = 'module load R/3.5.1 && Rscript ',
#'       param1 = NULL)
#' }
sbatch = function(path = getwd(), rFile = "testSbatch.R",
                  jobName = "job", threaded = 1, memoryG = 10, rTimeHour = 2, 
                  logFile = "logfilename.log", email = NULL,
                  when2Email = "END,FAIL,STAGE_OUT,TIME_LIMIT",
                  computeNode = NULL,
                  moreQsubParams = "",
                  array = 1:10,
                  dependency = FALSE,
                  preCMD = 'module load R/3.5.1 && Rscript ',
                  param1 = NULL,
                  ...){
  
  if(!is.null(param1)){
    if(!is.null(array)){
      stop("Either array or param1 must be NULL")
    } else {
      if(dependency){
        message("'dependency' will not be used")
      }
      rTimeHour =  rTimeHour = paste0(floor(rTimeHour), ":", formatN(60*(rTimeHour - floor(rTimeHour)), 2), ":00")
      return(rQsub2(path, rFile, jobName, threaded, memoryG, rTimeHour, logFile,
                    email, when2Email, computeNode, moreQsubParams, preCMD, param1, ...))
    }
  }
  
  params0 = ""
  if(!is.null(array)){
    stopifnot(is.null(param1))
    if(!is.null(param1) || length(list(...))!=0){
      stop("When array is not NULL, param1 ... must be NULL")
    }
    if(is.integer(array)){
      stopifnot(all(array>0))
      array = paste0(array, collapse = ",")
    } else if(is.character(array)){
      stopifnot(length(array) == 1)
    } else {
      stop("If not NULL, array must be integers or characters of length of 1")
    }
    params0 = paste(params0, "-a", array)
  }
  
  stopifnot(is.logical(dependency))
  if(dependency){
    params0 = paste(params0, "-d singleton")
  }
  # 
  # 
  # params = list(param1, ...) # parameters passed to R
  # num = unique(sapply(params, length))
  # if (length(unique(sapply(params, length))) != 1){
  #   stop("The length of arguments are not the same!")
  # }
  
  if(!is.null(computeNode)){
    stopifnot(length(computeNode) == 1 && is.character(computeNode))
    if(computeNode == "all.q@n00[0-9][0-9]*"){
      computeNode = NULL
    }
    params0 = paste(params0, "-w", computeNode)
  }
  
  # memory
  memory = paste0("--mem=", memoryG, "G")
  params0 = paste(params0, memory)
  
  # time
  rTimeHour = paste0(floor(rTimeHour), ":", formatN(60*(rTimeHour - floor(rTimeHour)), 2), ":00")
  params0 = paste(params0, paste0("--time=", rTimeHour))
  
  # jobName
  params0 = paste(params0, "-J", jobName)
  
  # logFile
  params0 = paste(params0, "-e", logFile, "-o", paste0(logFile, ".out"))
  
  # threaded
  params0 = paste(params0, "-c", threaded)
  
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
    emailAndWhen = paste0("--mail-user=", email, " --mail-type=", when2Email)
  }
  qparam = paste("sbatch", params0, emailAndWhen, moreQsubParams)
  
  cmd = paste('echo -e "#!/bin/bash\n', preCMD, rFile, 
              '" |', qparam)
  system(cmd, intern = TRUE, wait = TRUE)
  
}


qstatProcess2 = function(statRes){
  res = strSplit(statRes, "\\|") # split columns.
  res = res[,-ncol(res)]   # remove the last (empty) column.
  colnames(res) = res[1,]  # the first row is the column names.
  res = res[-1,]           # remove the first row after naming the columns.
  res = as.data.frame(res[res[,"User"] != "",,drop = FALSE]) # convert to data frame.
  return(res)
}

qstatSummary2 = function(statRes){
  if(nrow(statRes) == 0){
    message("0 Gb memory and 0 CPUs have been included in the table\n\n")
    return(invisible(statRes))
  }
  
  fmt = function(item){
    tapply(item, statRes$State, sum, na.rm = TRUE)
  }
  memory = as.numeric(sub("Mn|Mc", "E-3", sub("Gn|Gc", "", statRes$ReqMem)))
  memorySummary = fmt(memory)
  
  node = as.numeric(statRes$ReqNodes)
  nodeSummary = fmt(node)
  
  cpu = as.numeric(statRes$ReqCPUS)
  cpuSummary = fmt(cpu)
  
  
  attr(statRes, "usedMemoryInTotal") = mT = memorySummary
  attr(statRes, "usedNodesInTotal") = nT = nodeSummary
  attr(statRes, "usedCPUsInTotal") = cT = cpuSummary
  
  pst = function(itemSummary){
    paste(paste0(itemSummary, "(", names(itemSummary), ")"),
          collapse = ", ")
  }
  
  message(pst(mT), " Gb memory ", "and \n", 
          pst(cT), " CPUs have been included in the table\n\n")
  
  
  itemPerU = function(item){
    y = tapply(item, INDEX = list(statRes$User, statRes$State), 
               sum, na.rm = TRUE)
    y[is.na(y)] = 0
    y = data.frame(y)
    if(nrow(y) > 0 && !"RUNNING" %in% colnames(y)){
      y$RUNNING = 0
    }
    y
  }
  
  mU = itemPerU(memory)
  
  attr(statRes, "memoryPerUser") = mU
  
  mUSort = sort(setNames(mU$RUNNING, rownames(mU)), decreasing = TRUE)
  message("The following top (upto) 5 users are using most of the memory: ")
  message(sprintf("  %s(%s G)", names(head(mUSort, 5)), 
                  head(mUSort, 5)), "\n\n")
  
  cU = itemPerU(cpu)
  
  attr(statRes, "CPUsPerUser") = cU 
  
  cUSort = sort(setNames(cU$RUNNING, rownames(cU)), decreasing = TRUE)
  message("The following top (upto) 5 users are using the most of CPUs: ")
  message(sprintf("  %s(%s)", names(head(cUSort, 5)), head(cUSort, 5)), "\n\n")
  return(invisible(statRes))
}


#' Print statRes2 object
#' @param statRes the result of sacct/sacctAll/sacctGroupAll functions.
#' @examples 
#' \dontrun{
#' tmp = qstatAll2()
#' print(tmp)
#' }
#' @export
print.statRes2 = function(statRes){
  print.data.frame(statRes)
  res = qstatSummary2(statRes)
  return(invisible(res))
}

#' checking all users' job status on HPC for slurm system
#' @param stat status, one of "run", "all", "wait", 
#' "COMPLETED", "TIMEOUT", "FAILED", "OUT_OF_MEMORY", and "CANCELLED".
#' @examples 
#' \dontrun{
#' qstatAll2("all")
#' qstatAll2("run")
#' qstatAll2("TIMEOUT")
#' }
#' @export
qstatAll2 = function(stat = c("run", "all", "wait",
                              "COMPLETED", "TIMEOUT", "FAILED", 
                              "OUT_OF_MEMORY", "CANCELLED")){
  .Deprecated("sacctAll")
  stat = match.arg(stat)
  
  variables = unique(c('JobID', 'JobName', 'Account', 'State',
                       'Group', 'User', 'WorkDir',
                       'AllocNodes', 'NodeList', 'AllocCPUS', 
                       'Submit', 'Start', 'Eligible','Elapsed', 
                       unlist(strsplit(removeSpace(system("sacct -e", 
                                                          intern = TRUE)), " "))))
  cmd2 = paste("sacct --alluser -p -o ", paste0(variables, collapse = ","))
  
  res0 = system(cmd2, intern = TRUE)
  res = qstatProcess2(res0)
  
  if (stat == "all"){
    res = res
  } else if (stat == "run"){
    res = subset(res, State == "RUNNING")
  } else if (stat == "wait"){
    res = subset(res, State == "PENDING")
  } else if (stat == "COMPLETED"){
    res = subset(res, State == "COMPLETED")
  } else if (stat == "TIMEOUT"){
    res = subset(res, State == "TIMEOUT")
  } else if (stat == "FAILED"){
    res = subset(res, State == "FAILED")
  } else if (stat == "OUT_OF_MEMORY"){
    res = subset(res, State == "OUT_OF_MEMORY")
  } else if (stat == "CANCELLED"){
    res = subset(res, substr(State, 1, 9) == "CANCELLED")
  } else {
    stop("Unknown stat!")
  }
  
  # res = qstatProcess(statRes = system(cmd2, intern = TRUE))
  class(res) = c("statRes2", "data.frame")
  # return(invisible(res))
  return(res)
}


#' checking all users' job status on HPC for slurm system
#' @param stat status, one of "run", "all", "wait", 
#' "COMPLETED", "TIMEOUT", "FAILED", "OUT_OF_MEMORY", and "CANCELLED".
#' @examples 
#' \dontrun{
#' sacctAll("all")
#' sacctAll("run")
#' sacctAll("TIMEOUT")
#' }
#' @export
sacctAll = function(stat = c("run", "all", "wait",
                              "COMPLETED", "TIMEOUT", "FAILED", 
                              "OUT_OF_MEMORY", "CANCELLED")){
  stat = match.arg(stat)
  
  variables = unique(c('JobID', 'JobName', 'Account', 'State',
                       'Group', 'User', 'WorkDir',
                       'AllocNodes', 'NodeList', 'AllocCPUS', 
                       'Submit', 'Start', 'Eligible','Elapsed', 
                       unlist(strsplit(removeSpace(system("sacct -e", 
                                                          intern = TRUE)), " "))))
  cmd2 = paste("sacct --alluser -p -o ", paste0(variables, collapse = ","))
  
  res0 = system(cmd2, intern = TRUE)
  res = qstatProcess2(res0)
  
  if (stat == "all"){
    res = res
  } else if (stat == "run"){
    res = subset(res, State == "RUNNING")
  } else if (stat == "wait"){
    res = subset(res, State == "PENDING")
  } else if (stat == "COMPLETED"){
    res = subset(res, State == "COMPLETED")
  } else if (stat == "TIMEOUT"){
    res = subset(res, State == "TIMEOUT")
  } else if (stat == "FAILED"){
    res = subset(res, State == "FAILED")
  } else if (stat == "OUT_OF_MEMORY"){
    res = subset(res, State == "OUT_OF_MEMORY")
  } else if (stat == "CANCELLED"){
    res = subset(res, substr(State, 1, 9) == "CANCELLED")
  } else {
    stop("Unknown stat!")
  }
  
  # res = qstatProcess(statRes = system(cmd2, intern = TRUE))
  class(res) = c("statRes2", "data.frame")
  # return(invisible(res))
  return(res)
}

#' checking your own job status on HPC for slurm system
#' @param stat status, one of "run", "all", "wait", 
#' "COMPLETED", "TIMEOUT", "FAILED", "OUT_OF_MEMORY", and "CANCELLED".
#' @examples 
#' \dontrun{
#' qstat2(stat = "run")
#' qstat2("all")
#' }
#' @export
qstat2 = function(stat = c("run", "all", "wait",
                           "COMPLETED", "TIMEOUT", "FAILED", 
                           "OUT_OF_MEMORY", "CANCELLED")){
  .Deprecated("sacct")
  stat = match.arg(stat)
  
  variables = unique(c('JobID', 'JobName', 'Account', 'State',
                       'Group', 'User', 'WorkDir',
                       'AllocNodes', 'NodeList', 'AllocCPUS', 
                       'Submit', 'Start', 'Eligible','Elapsed', 
                       unlist(strsplit(removeSpace(system("sacct -e", 
                                                          intern = TRUE)), " "))))
  cmd2 = paste("sacct -p -o ", paste0(variables, collapse = ","))
  
  res0 = system(cmd2, intern = TRUE)
  res = qstatProcess2(res0)
  
  if (stat == "all"){
    res = res
  } else if (stat == "run"){
    res = subset(res, State == "RUNNING")
  } else if (stat == "wait"){
    res = subset(res, State == "PENDING")
  } else if (stat == "COMPLETED"){
    res = subset(res, State == "COMPLETED")
  } else if (stat == "TIMEOUT"){
    res = subset(res, State == "TIMEOUT")
  } else if (stat == "FAILED"){
    res = subset(res, State == "FAILED")
  } else if (stat == "OUT_OF_MEMORY"){
    res = subset(res, State == "OUT_OF_MEMORY")
  } else if (stat == "CANCELLED"){
    res = subset(res, substr(State, 1, 9) == "CANCELLED")
  } else {
    stop("Unknown stat!")
  }
  
  # res = qstatProcess(statRes = system(cmd2, intern = TRUE))
  class(res) = c("statRes2", "data.frame")
  # return(invisible(res))
  return(res)
}



#' checking your own job status on HPC for slurm system
#' @param stat status, one of "run", "all", "wait", 
#' "COMPLETED", "TIMEOUT", "FAILED", "OUT_OF_MEMORY", and "CANCELLED".
#' @examples 
#' \dontrun{
#' sacct(stat = "run")
#' sacct("all")
#' }
#' @export
sacct = function(stat = c("run", "all", "wait",
                           "COMPLETED", "TIMEOUT", "FAILED", 
                           "OUT_OF_MEMORY", "CANCELLED")){
  stat = match.arg(stat)
  
  variables = unique(c('JobID', 'JobName', 'Account', 'State',
                       'Group', 'User', 'WorkDir',
                       'AllocNodes', 'NodeList', 'AllocCPUS', 
                       'Submit', 'Start', 'Eligible','Elapsed', 
                       unlist(strsplit(removeSpace(system("sacct -e", 
                                                          intern = TRUE)), " "))))
  cmd2 = paste("sacct -p -o ", paste0(variables, collapse = ","))
  
  res0 = system(cmd2, intern = TRUE)
  res = qstatProcess2(res0)
  
  if (stat == "all"){
    res = res
  } else if (stat == "run"){
    res = subset(res, State == "RUNNING")
  } else if (stat == "wait"){
    res = subset(res, State == "PENDING")
  } else if (stat == "COMPLETED"){
    res = subset(res, State == "COMPLETED")
  } else if (stat == "TIMEOUT"){
    res = subset(res, State == "TIMEOUT")
  } else if (stat == "FAILED"){
    res = subset(res, State == "FAILED")
  } else if (stat == "OUT_OF_MEMORY"){
    res = subset(res, State == "OUT_OF_MEMORY")
  } else if (stat == "CANCELLED"){
    res = subset(res, substr(State, 1, 9) == "CANCELLED")
  } else {
    stop("Unknown stat!")
  }
  
  # res = qstatProcess(statRes = system(cmd2, intern = TRUE))
  class(res) = c("statRes2", "data.frame")
  # return(invisible(res))
  return(res)
}

#' Status (sacct, equivalent to qstat) of running jobs of group members.
#' @param stat the job status, including "run" (the default),
#' "all", "wait", "COMPLETED", "TIMEOUT", "FAILED", "OUT_OF_MEMORY", 
#' and "CANCELLED".
#' @param groupMembers the group member names. The default is the group members 
#' of the user who run this code.
#' @seealso \code{\link{sacct}}, \code{\link{sacctAll}}
#' @export
qstatGroupAll2 = function(stat = c("run", "all", "wait",
                                   "COMPLETED", "TIMEOUT", "FAILED", 
                                   "OUT_OF_MEMORY", "CANCELLED"),
                          groupMembers = dir(paste0("/hpc/", strSplit(system("groups", intern = TRUE), ' ')[1,1]))){
  
  res0 = qstatAll2(stat)
  
  res = subset(res0, User %in% groupMembers)
  return(res)
}


#' Delete (scancel) of SLURM jobs.
#' @param id The job ID.
#' @seealso \code{\link{scancelAll}}
#' @export
qdel2 = function(id){
  .Deprecated("scancel")
  # cmd = paste("scancel", paste(id, collapse = " "))
  cmds = paste("scancel", id)
  res = c()
  for(cmd in cmds)
    res[cmd] = system(cmd, intern = TRUE)
  return(res)
}

#' Delete (scancel) of SLURM jobs.
#' @param id The job ID.
#' @seealso \code{\link{scancelAll}}
#' @export
scancel = function(id){
  # cmd = paste("scancel", paste(id, collapse = " "))
  cmds = paste("scancel", id)
  res = c()
  for(cmd in cmds)
    res[cmd] = system(cmd, intern = TRUE)
  return(res)
}

#' Delete (scancel) of SLURM jobs.
#' @param pattern regular expression pattern. The default is "*".
#' @param col the index of column to match the pattern. The default is 1.
#' @seealso \code{\link{qdel2}}
#' @export
qdelAll2 = function(pattern = "*", col = 1){
  .Deprecated("scancelAll")
  q = sacct("all")
  id = as.character(q[,col])
  indx = grep(pattern, id)
  if (length(indx) > 0){
    qUser = q[indx,]
    id2Del = as.character(qUser[,1])
    qdel2(rev(id2Del))
  } else {
    cat("No jobs matched!")
    id2Del = NULL
  }
  return(id2Del)
}


#' Delete (scancel) of SLURM jobs.
#' @param pattern regular expression pattern. The default is "*".
#' @param col the index of column to match the pattern. The default is 1.
#' @seealso \code{\link{qdel2}}
#' @export
scancelAll = function(pattern = "*", col = 1){
  q = sacct("all")
  id = as.character(q[,col])
  indx = grep(pattern, id)
  if (length(indx) > 0){
    qUser = q[indx,]
    id2Del = as.character(qUser[,1])
    qdel2(rev(id2Del))
  } else {
    cat("No jobs matched!")
    id2Del = NULL
  }
  return(id2Del)
}


#' The Memory and CPU usage of each node on HPC
#' 
#' @export
hpcInfo2 = function(){
  res0 = system("sinfo -o %all", TRUE)
  res1 = as.data.frame(strSplit(res0[-1], "\\|"), stringsAsFactors = FALSE)
  colnames(res1) = as.vector(strSplit(gsub(" ", "", res0[1]), "\\|"))
  numIDs = c("CPUS", "TMP_DISK", "FREE_MEM", "MEMORY", "PRIO_TIER", 
             "NODES", "SOCKETS", "CORES", "THREADS")
  for(mi in numIDs){
    res1[[mi]] = suppressWarnings(as.numeric(res1[[mi]]))
    if(mi %in% c("FREE_MEM", "MEMORY")){
      res1[[mi]] = res1[[mi]]/10^3
    }
  }
  infoShow = res1[, c("HOSTNAMES", "FREE_MEM", "CPUS(A/I/O/T)")]
  infoShow = sortDataframe(infoShow, "FREE_MEM", decreasing = TRUE)
  infoShow$index = 1:nrow(infoShow)
  print(infoShow)
  return(invisible(res1))
}