
#' Get the information of loaded C functions/routines in a package or shared object (so or dll)
#' @param package package name or shared object name
#' @return DLLRegisteredRoutines or NULL if no C function is found
#' @examples 
#' \dontrun{
#' tmp = getLoadedCFun("base")
#' tmp
#' class(tmp)
#' 
#' library(Matrix)
#' getLoadedCFun("Matrix")
#' 
#' library(devtools)
#' getLoadedCFun("devtools")
#' }
#' @export
getLoadedCFun = function(package){
  dlls = getLoadedDLLs()
  nm = names(dlls)
  if(package %in% nm){
    res = getDLLRegisteredRoutines(dlls[[package]])
  } else {
    res = NULL
  }
  return(res)
}
