
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

#' list all R frameworks
#' @param frameworkPath the R.framework path, the default is 
#' "/Library/Frameworks/R.framework/" on Mac.
#' @export
#' @examples 
#' \dontrun{
#' R_frameworks()
#' }
R_frameworks = function(frameworkPath = "/Library/Frameworks/R.framework/"){
  stopifnot(get_os() == "osx")
  versionPath = paste0(frameworkPath, "/Versions/")
  versions = dir(versionPath)
  
  versions = versions[versions != "Current"]
  res = setNames(versions, paste0(versionPath, versions))
  return(res)
}


#' The current version of R framework
#' @param frameworkPath the R.framework path, the default is 
#' "/Library/Frameworks/R.framework/" on Mac.
#' @export
#' @examples 
#' \dontrun{
#' R_currentFramework()
#' }
R_currentFramework = function(frameworkPath = "/Library/Frameworks/R.framework/"){
  stopifnot(get_os() == "osx")
  cu = system(paste0("readlink ", frameworkPath, 
                     "/Versions/Current"), intern = TRUE)
  res = head(tail(as.vector(strSplit(cu, "/")), 2), 1)
  res = setNames(res, cu)
  return(res)
}

#' R version string
#' @export
#' @examples
#' \dontrun{
#' R_version()
#' }
#' 
R_version = function(){
  paste0(R.version$major, ".", R.version$minor)
}
#' 
#' #' Switch R version 
#' #' @param Version 
#' #' 
#' R_switch = function(Version, path){
#'   
#'   system("ls -al /Library/Frameworks/R.framework/Versions", intern = TRUE)
#'   
#' }