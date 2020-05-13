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
#' @param email email to receive report from HPC. The default is NULL.
#' @param when2Email when will the server send a email. This can be
#' "b" (start) and/or "e" (end) and/or "a" (abortion) and/or "s" (suspension)
#' of your job. The default is "aes". This parameter will not be
#' used if email is NULL.
#' @param computeNode which computing CPU node will be used?
#' Support regular expression. The default is \code{"all.q@@n00[0-9][0-9]*"}
#' @param moreQsubParams more qsub \href{https://wiki.bioinformatics.umcutrecht.nl/bin/viewauth/HPC/HowToS}{parameters}.
#' @param param1 the first parameter passed to R script.
#' @param ... other parameter passed to R script.
#' @return A list of output of running rFile.
#' @export
#' @examples
#' \dontrun{
#' # This function can only run on HPC.
#'
#' ## Section 1, the format of R script file (for example myfile.R) to submit:
#' args = commandArgs(TRUE)
#' if (FALSE){
#'  # Input the code to use rQsub, see section
#'  # And run the code manually in this if`{}` statement , for example:
#'  rQsub(path = getwd(), rFile = "myfile.R", param1 = 1:5)
#' }
#' arg1 = args[1]
#' # Then input R code to use the arg1 variable. for example:
#' cat("test code result:", arg1, "\n")
#'
#' ## Section 2, the code to use rQsub
#' # One parameter in R script
#' path = "/my/file/path"
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

rQsub = function(path = getwd(), rFile = "testQsub.R",
                 jobName = "job",
                 threaded = 1, memoryG = 10, rTimeHour = 2,
                 logFile = "logfilename.log",
                 email = NULL,
                 when2Email = "aes",
                 computeNode = "all.q@n00[0-9][0-9]*",
                 moreQsubParams = "",
                 preCMD = 'echo "module load R/3.3.0 && Rscript ',
                 param1 = 1:10,
                 ...){
  params = list(param1, ...) # parameters passed to R
  num = unique(sapply(params, length))
  if (length(unique(sapply(params, length))) != 1){
    stop("The length of arguments are not the same!")
  }

  stopifnot(length(computeNode) == 1 && is.character(computeNode))

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

  if (is.null(email)){
    emailAndWhen = ""
  } else {
    if (grep("[aes]", when2Email)){
      emailAndWhen = paste("-M", email, "-m", when2Email)
    } else{
      message("No email will be sent!")
      emailAndWhen = ""
    }
  }

  qparam = paste("qsub", "-q", computeNode,  "-N", jobName, "-pe threaded", threaded,
                 "-l", memory, "-l", rTime, "-e", logFile,
                 emailAndWhen, "-cwd", moreQsubParams)
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
# qstatProcess = function(statRes){
#   if (length(statRes) == 0){
#     message("No queue job was found!")
#     return(character(0))
#   }
#   f = function(x, pat, fill = ""){
#     y = grep(pat, x, value = TRUE)
#     if (length(y) == 0) {
#       y = fill
#     }
#     gsub(pat, "", y)
#   }
#   statRes = removeSpace(statRes)
#
#   len = length(statRes)
#   id1 = grep("^[0-9]* [0-9]\\.[0-9]*", statRes)
#   id2 = c(id1[-1]-1, len)
#   res0 = lapply(seq_along(id1), function(mi){
#     statResSub = statRes[id1[mi] : id2[mi]]
#     return(c(grep("^[0-9]* [0-9]\\.[0-9]*", statResSub, value = TRUE),
#              f(statResSub, "^Full jobname: "),
#              f(statResSub, "^h_rt=", fill = 24*3600),
#              f(statResSub, "^Hard Resources: "),
#              f(statResSub, "^Requested PE: ", fill = "threaded 1"),
#              f(statResSub, "^Binding: "),
#              f(statResSub, "^Soft Resources:")))
#   })
#   if (length(res0) < 1){
#     cat("No jobs are on HPC based on your request!\n")
#     return(invisible(NULL))
#   }
#   res0 = res0[sapply(res0, length) == 7]
#   res0 = as.data.frame(t(as.data.frame(res0)))
#   colnames(res0) = c("base", "fullName", "maxSeconds", "hardResources",
#                      "requestedPE", "binding", "softResources")
#
#   res = data.frame(strSplit(res0$base, " "), stringsAsFactors = FALSE)
#   colnames(res) = c("job.ID", "prior", "name", "user", "state", "submit.start.at",
#                     "at", "queue", "slots", "ja.task.ID")[1:ncol(res)]
#   res$slots = if (is.null(res$slots)) {
#     0
#   } else {
#     slot_tmp = res$slots
#     slot_tmp[slot_tmp == ""] = 0
#     as.numeric(slot_tmp)
#   }
#   res$submit.start.at = as.Date(res$submit.start.at, "%m/%d/%Y")
#   res$at = chron::chron(times. = res$at, format = c(dataes = "m/d/y", time = "h:m:s"))
#   res$submit.start.at = as.POSIXlt(paste(res$submit.start.at, res$at))
#
#   if (ncol(res) == 9){
#     res$ja.task.ID = ""
#   }
#   res$at = NULL
#
#   # additional information
#   res$name = res0$fullName
#   res$maxSeconds = as.numeric(strSplit(res0$maxSeconds, " ")[,1])
#   res$will.stop.at = res$submit.start.at + res$maxSeconds
#   res$hardResources = res0$hardResources
#   res$requestedPE = res0$requestedPE
#   res$binding = res0$binding
#   res$softResources = res0$softResources
#   return(res)
# }

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
    return(c(base = grep("^[0-9]* [0-9]\\.[0-9]*", statResSub, value = TRUE),
             fullName = f(statResSub, "^Full jobname: "),
             maxSeconds = f(statResSub, "^h_rt=|^Hard Resources: h_rt=", fill = 24*3600),
             memory = f(statResSub, "^h_vmem=|^Hard Resources: h_vmem="),
             tmpspace = f(statResSub, "^tmpspace=|^Hard Resources: tmpspace="),
             requestedPE = f(statResSub, "^Requested PE: ", fill = "threaded 1"),
             binding = f(statResSub, "^Binding: "),
             softResources = f(statResSub, "^Soft Resources:")))
  })
  if (length(res0) < 1){
    cat("No jobs are on HPC based on your request!\n")
    return(invisible(NULL))
  }
  res0 = res0[sapply(res0, length) == 8]
  res0 = as.data.frame(t(as.data.frame(res0)))
  # colnames(res0) = c("base", "fullName", "maxSeconds", "hardResources",
  #                    "requestedPE", "binding", "softResources")

  res = data.frame(strSplit(res0$base, " "), stringsAsFactors = FALSE)
  colnames(res) = c("job.ID", "prior", "name", "user", "state", "submit.start.at",
                    "at", "queue", "slots", "ja.task.ID")[1:ncol(res)]
  res$slots = if (is.null(res$slots)) {
    0
  } else {
    slot_tmp = res$slots
    slot_tmp[slot_tmp == ""] = 0
    as.numeric(slot_tmp)
  }
  res$submit.start.at = as.Date(res$submit.start.at, "%m/%d/%Y")
  res$at = chron::chron(times. = res$at, format = c(dataes = "m/d/y", time = "h:m:s"))
  res$submit.start.at = as.POSIXlt(paste(res$submit.start.at, res$at))

  if (ncol(res) == 9){
    res$ja.task.ID = ""
  }
  res$at = NULL
  queue = res$queue
  res$queue = NULL
  taskID = res$ja.task.ID
  res$ja.task.ID = NULL

  # additional information
  res$name = res0$fullName
  res$maxSeconds = as.numeric(strSplit(res0$maxSeconds, " ")[,1])
  res$will.stop.at = res$submit.start.at + res$maxSeconds
  res$memory = strSplit(res0$memory, " ")[,1]
  res$memory[res$memory == ""] = "10G"
  res$tmpspace = strSplit(res0$tmpspace, " ")[,1]
  # res$requestedPE = res0$requestedPE
  # res$binding = res0$binding
  # res$softResources = res0$softResources
  res$queue = queue
  res$ja.task.ID = taskID
  
  class(res) = c("statRes", "data.frame")
  return(res)
}

#' Summarize qstat results
#' @param statRes the result of qstat/qstatAll/qstatGroupAll functions.
#' @examples 
#' \dontrun{
#' tmp = qstatAll()
#' qstatSummary(tmp)
#' }
#' @export
qstatSummary = function(statRes){
  memory = as.numeric(sub("G", "", statRes$memory))
  
  attr(statRes, "usedMemoryInTotal") = mT = sum(memory, na.rm = TRUE)
  attr(statRes, "usedSlotsInTotal") = sT = sum(statRes$slots, na.rm = TRUE)
  
  message(mT, " Gb memory ", "and ", sT, " cores have been inlcuded in the table\n\n")
  
  attr(statRes, "memoryPerUser") = mU = sort(tapply(memory, INDEX = statRes$user, 
                                                sum, na.rm = TRUE),
                                         decreasing = TRUE)
  message("The following top (upto) 5 users are using or going to use most memory: \n")
  message(sprintf("  %s(%s G)", names(head(mU, 5)), head(mU, 5)), "\n\n")
  
  attr(statRes, "slotsPerUser") = sU = sort(tapply(statRes$slots, INDEX = statRes$user, 
                                               sum, na.rm = TRUE),
                                        decreasing = TRUE)
  message("The following top (upto) 5 users are using or going to use most cores: \n")
  message(sprintf("  %s(%s)", names(head(sU, 5)), head(sU, 5)), "\n\n")
  return(statRes)
}

#' Print statRes object
#' @param statRes the result of qstat/qstatAll/qstatGroupAll functions.
#' @examples 
#' \dontrun{
#' tmp = qstatAll()
#' print(tmp)
#' }
#' @export
print.statRes = function(statRes){
  print.data.frame(statRes)
  res = qstatSummary(statRes)
  return(invisible(res))
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

  res = qstatProcess(statRes = system(cmd2, intern = TRUE))
  # res = qstatSummary(res)
  return(res)
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
  # res = qstatSummary(res)
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
  res = qstatProcess(statRes = system(cmd2, intern = TRUE))
  # res = qstatSummary(res)
  return(res)
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


#' Information of HPC computing nodes
#' @return a data frame, including 
#' · queuename, the queue name;
#' · qtype, the queue type - one of B(atch), I(nteractive), C(heckpointing), P(arallel);
#' · resv.used.tot., the number of used and available job slots;
#' · load_avg, the load average of the queue host or another load value;
#' · arch, the architecture of the queue host;
#' · stats, the state of the queue - one of u(nknown), a(larm), A(larm), 
#'   C(alendar suspended), s(uspended), S(ubordinate), d(isabled), D(isabled), 
#'   E(rror), c(configuration ambiguous), o(rphaned), P(reempted).
#' · etc.
#' 
#' @export
#' @examples 
#' \dontrun{
#' hpcInfo()
#' 
#' View(hpcInfo())
#' }
hpcInfo = function(){
  qstatF0 = system("qstat -F", intern = TRUE)
  qstatF0 = grep("^[q|a|h|t|\\-]", removeSpace(sub("^\t", "", qstatF0)), 
                 value = TRUE)
  
  id_head = 1
  id_jobs_start = grep("^---", qstatF0) + 1
  id_jobs_end = c(grep("^---", qstatF0)[-1] - 1, length(qstatF0))
  
  len = unique(id_jobs_end - id_jobs_start)
  if(length(len) != 1){
    stop("Not every node has the same length of information.")
  }
  
  info1 = matrix(strSplit(qstatF0[id_jobs_start], " "), 
                 nrow = length(id_jobs_start),
                 dimnames = list(seq_along(id_jobs_start), 
                                 strSplit(qstatF0[id_head], " ")))
  info2 = matrix(as.numeric(strSplit(info1[,"resv/used/tot."], "/")), 
                 nrow = nrow(info1),
                 dimnames = list(seq(nrow(info1)), 
                                 strSplit("resvJobs/usedJobs/totalJobs", "/")))
  
  colNM = strSplit(qstatF0[(id_jobs_start[1] + 1):id_jobs_end[1]], "=")[,1]
  # value = strSplit(qstatF0[-c(id_head, id_jobs_start-1)], "=")[,2]
  value = strSplit(qstatF0[-c(id_head, id_jobs_start-1, id_jobs_start)], 
                   "=")[,2]
  info3 = matrix(value, ncol = len, byrow = TRUE, 
                 dimnames = list(seq_along(id_jobs_start), colNM))
  
  info = data.frame(info1, info2, info3, stringsAsFactors = FALSE)
  
  numericVar1 = c("load_avg", "hl.mem_total", "hl.swap_total", 
                  "hl.virtual_total", 
                  "hl.mem_free", 
                  "hl.swap_free", "hl.virtual_free", "hl.mem_used", 
                  "hl.swap_used", "hl.virtual_used", 
                  "hc.h_vmem", "hc.tmpspace")
  
  numericVar2 = c("hl.num_proc", "hl.m_socket", "hl.m_core", "hl.m_thread", 
                  "hl.load_avg", "hl.load_short", 
                  "hl.load_medium", "hl.load_long", 
                  "hl.cpu", "hl.np_load_avg", 
                  "hl.np_load_short", "hl.np_load_medium", "hl.np_load_long", 
                  "hc.slots", 
                  # "qf.h_rt", 
                  "qf.seq_no", "qf.rerun")
  
  GT2N = function(x) {
    xG = grep("*G$", x)
    xT = grep("*T$", x)
    y = rep(0, length(x))
    y[xG] = as.numeric(sub("G", "", x[xG]))
    y[xT] = as.numeric(sub("T", "", x[xT])) * 1024
    y
  }
  
  for(mi in numericVar1){
    info[[mi]] = GT2N(info[[mi]])
  }
  for(mi in numericVar2){
    info[[mi]] = as.numeric(info[[mi]])
  }
  
  info = sortDataframe(info, "queuename")
  rownames(info) = seq(nrow(info))
  class(info) = c("HpcInfo", "data.frame")
  return(info)
}

#' print HpcInfo object
#' @param x HpcInfo object.
#' @param ... other parameters in print.data.frame function.
#' @return print.HpcInfo returns an invisible data frame, which shows the 
#' available jobs, slots and memory on HPC nodes.
#' @export
print.HpcInfo = function(x, ...){
  y = x[c('queuename', 'usedJobs', 'totalJobs', 'hl.num_proc', 
          'hl.mem_total', 'hl.mem_free')]
  
  qA = qstatAll()
  slotsBooked = data.frame(slots = tapply(qA$slots, qA$queue, sum, na.rm = TRUE))
  y$slotsBooked = slotsBooked[y$queuename, 1]
  y$slotsBooked[is.na(y$slotsBooked)] = 0
  
  y$jobsAvail = y$totalJobs - y$usedJobs
  y$slotsAvail = y$hl.num_proc - y$slotsBooked
  
  memoryBooked = data.frame(memory = tapply(qA$memory, qA$queue, function(a, b) 
    sum(as.numeric(sub("G", "", a)), na.rm = TRUE)))[y$queuename, 1]
  
  memoryBooked[is.na(memoryBooked)] = 0
  
  y$memoryAvail = y$hl.mem_total - memoryBooked
  
  z = y[, c("queuename", "jobsAvail", "slotsAvail", "memoryAvail")]
  print.data.frame(z, ...)
  return(invisible(z))
}