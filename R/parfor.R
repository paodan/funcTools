#' parallel computing with a progress bar
#' @param x a vector/list that function FUN needs
#' @param FUN function
#' @param ... other parameters that function FUN needs
#' @param .core number of cores
#' @param .printConsole logical, if messages from FUN is printed
#' @param .progressBar logical, if a progressBar is added
#' @import doParallel
#' @import parallel
#' @examples 
#' \dontrun{
#' parfor(1:10, c, 1)
#' parfor(1:10, Sys.sleep, .core = 10)
#' y = parfor(1:10, print, .printConsole = TRUE, .progressBar = FALSE)
#' parfor(1:10, print, .printConsole = TRUE, .progressBar = TRUE)
#' }
#' @export
parfor = function(x, FUN, ..., .core = 2, .printConsole = TRUE, .progressBar = TRUE){
  if(.progressBar){
    .printConsole = TRUE
    pb <- txtProgressBar(min = 1, max = length(x), style = 3)
    on.exit(close(pb))
  }
  
  if(.printConsole){
    cl <- makeCluster(.core, outfile="") # number of cores. Notice 'outfile'
  } else {
    cl <- makeCluster(.core) # number of cores. Notice 'outfile'
  }
  on.exit(stopCluster(cl))
  registerDoParallel(cl)
  
  if(.progressBar){
    res <- foreach(i = seq_along(x)) %dopar% 
      {
        setTxtProgressBar(pb, i) 
        FUN(x[[i]], ...)
      }
  } else {
    res <- foreach(i = seq_along(x)) %dopar% FUN(x[[i]], ...)
  }
  
  return(res)
}

