#' Set working directory to where the file that put this
#' function located.
#' @return the working directory before the change.
#' @import rstudioapi
#' @examples {
#' \dontrun{
#' # Put the following codes in one file and save.
#' getwd()
#' setwd2thisFile()
#' getwd()
#' }
#' }
setwd2thisFile = function(){
  preDir = getwd()
  this.dir = dirname(getActiveDocumentContext()$path)
  if (this.dir == ""){
    message("Cannot find where this file located! The current working ",
            "directory is not changed!")
  } else {
    cat("Set working directory to ")
    setwd(this.dir)
  }
  invisible(preDir)
}
