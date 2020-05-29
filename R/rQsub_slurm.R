####### To be finished *******




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
  colnames(res1) = as.vector(strSplit(res0[1], "\\|"))
  numIDs = c("CPUs", "TMP_DISK", "FREE_MEM", "MEMORY", "PRIO_TIER", 
             "NODES", "SOCKETS", "CORES", "THREADS")
  for(mi in numIDs){
    res1[[mi]] = as.numeric(res1[[mi]])
    if(mi %in% c("FREE_MEM", "MEMORY")){
      res1[[mi]] = res1[[mi]]/10^3
    }
  }
  return(res1)
}
