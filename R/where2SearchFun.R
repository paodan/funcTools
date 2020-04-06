#' Which environment/package(s) is/are a function located in the search() path
#' 
#' @param x function or function name to search.
#' @param env environment to search, if the function x is not found in this 
#' environment, the parent environment is further searched until the top 
#' environment, i.e. empty environment (\code{emptyenv()}) 
#' or \code{R_EmptyEnv}.
#' @param mode either "function" or "any", if mode = "function" (default),
#' considering x is only a function object.
#' @return environment where the function/object x is found.
#' If x is not found, \code{NULL} is returned. If multiple 
#' environments are found, the first environment is the default place to 
#' get the value of x.
#' @export
#' @examples 
#' \dontrun{
#' detachPackages("Matrix")
#' whereis("print")
#' whereis(print)
#' 
#' # Matrix::print is the default print
#' library(Matrix)
#' whereis("print")
#' identical(print, Matrix::print)
#' identical(print, base::print)
#' 
#' print = function() print()
#' whereis("print")
#' 
#' # Search any object
#' whereis(.Options, mode = "any")
#' }
whereis = function(x, env = parent.frame(), mode = c("function", "any")) {
  mode = match.arg(mode)
  if (tryCatch(!is.character(x), error = function(e) TRUE)) 
    x = as.character(substitute(x))
  
  res = c()
  while(!identical(env, emptyenv())){
    if (exists(x, envir = env, mode = mode, inherits = FALSE)) {
      res = c(res, env)
    }
    env = parent.env(env)
  }
  return(res)
}
