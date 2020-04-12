
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

#' Switch R framework version on Mac
#' @param Version R framework version to switch to
#' @param reset if TRUE, the framework is changed back to the
#' original framework version. The default is FALSE.
#' @param openRStudio if TRUE, open an RStudio session with the new R framework
#' version. The default is FALSE.
#' @param frameworkPath the R.framework path, the default is 
#' "/Library/Frameworks/R.framework/" on Mac.
#' @param RStudioPath RStudio path, the default is 
#' "/Applications/RStudio.app/Contents/MacOS/RStudio".
#' @export
#' @examples 
#' \dontrun{
#' # List all frameworks
#' fw = R_frameworks()
#' fw
#' 
#' # Current framework
#' cfw = R_currentFramework()
#' cfw
#' 
#' # Switch
#' R_switch(setdiff(fw, cfw)[1], reset = FALSE, openRStudio = TRUE)
#' R_switch(cfw, reset = FALSE, openRStudio = TRUE)
#' 
#' R_switch(setdiff(fw, cfw)[1], reset = TRUE, openRStudio = FALSE)
#' 
#' # Reset the initial framework
#' R_switch(cfw, reset = FALSE)
#' }
R_switch = function(version = R_frameworks(),
                    reset = FALSE, 
                    openRStudio = FALSE,
                    frameworkPath = "/Library/Frameworks/R.framework/",
                    RStudioPath = "/Applications/RStudio.app/Contents/MacOS/RStudio"){

  version = match.arg(version)
  fw = R_frameworks()
  stopifnot(version %in% fw)
  
  if(reset & !openRStudio){
    warning("When reset = TRUE and openRStudio = FALSE, R framework is ", 
            "changed for only 1 second. No obvious effect can be seen.")
  }
  
  current = R_currentFramework()
  
  if(version == current){
    stop("The current framework (", current, ") is the same framework to ", 
         "switch, following framework(s) is/are available:\n\t", 
         paste0(setdiff(R_frameworks(), current), collapse = ", "))
  }
  
  # alias r4="unlink /Library/Frameworks/R.framework/Versions/Current && ln -s /Library/Frameworks/R.framework/Versions/4.0/ /Library/Frameworks/R.framework/Versions/Current && /Applications/RStudio.app/Contents/MacOS/RStudio "
  vPath = normalizePath(paste0(frameworkPath, "/Versions/"))
  cPath = paste0(vPath, "/Current")
  
  cmdSwitch = paste0('unlink ', cPath, 
                     ' && ln -s ', vPath, '/', version, '/ ', cPath)
  if (openRStudio){
    cmdSwitch = paste0(cmdSwitch, ' && ', RStudioPath)
  }
  
  cmdReset = paste0('unlink ', cPath, 
                      ' && ln -s ', vPath, '/', current, '/ ', cPath)
  
  if(reset) {
    on.exit({
      Sys.sleep(1)
      system(cmdReset)
      message("R framework is reset to ", R_currentFramework())
      })
  }
  message("Current R framework is ", current, 
          " (working R version is ", R_version(), ")")
  message("R framework is switched to ", version)
  res = system(cmdSwitch)
  return(invisible(res))
}
