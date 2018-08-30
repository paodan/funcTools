#' @title Set working directory according to the file including this function\cr
#' @description Set working directory to where the file that put this
#' function located.
#' @return the working directory before the change.
#' @import rstudioapi
#' @export
#' @examples {
#' \dontrun{
#' # Put the following codes in one file and save.
#' getwd()
#' setwd2thisFile()
#' getwd()
#' }
#' }
#' @export
setwd2thisFile = function(){
  preDir = getwd()
  this.dir = dirname(getActiveDocumentContext()$path)
  if (this.dir == ""){
    message("Cannot find where this file located! The current working ",
            "directory is not changed!")
  } else {
    setwd(this.dir)
    cat("Set working directory to", this.dir, "\n")
  }
  invisible(preDir)
}
