#' Using $ sign to access the functions within a functionInfo object
#' 
#' @param name character, the element to extract
#' @export
#' @examples
#' \dontrun{
#' library(Matrix)
#' library(ggplot2)
#' x = funCode(print)
#' 
#' x$default # access a function for ANY (default S3) objects
#' x$ggplot # access a function for S3 objects
#' x$diagonalMatrix # access a function for S4 objects
#' x$asdfasdfasdf # show a warning
#' }
`$.functionInfo` <- function(x, name) {
  S3 = names(x@fS3@fcn)
  S4 = names(x@fS4@fcn)
  
  if(name %in% S3){
    message("This is a function for an S3 object")
    x@fS3@fcn[[name]]
  } else if (name %in% S4){
    message("This is a function for an S4 object")
    x@fS4@fcn[[name]]
  } else {
    warning(paste("No function for class:", name))
    x@fcn
  }
}

#' @importFrom utils .DollarNames
#' @export
.DollarNames.functionInfo <- function(x, pattern = "") {
  # This is a function to auto-complete the command with a dollar ($) sign followed by a variable name.
  methods <- c(names(x@fS3@fcn), names(x@fS4@fcn))
  if (is.atomic(x) || is.symbol(x)) {
    return(character())
  } 
  if (pattern == ""){
    methods
  }  else {
    findMatches(pattern, methods)
  }
}

