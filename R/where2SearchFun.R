#' Which environment/package(s) is/are a function located in the search() path
#' 
#' @param x function or function name to search.
#' @param env environment to search, if the function x is not found in this 
#' environment, the parent environment is further searched until the top 
#' environment, i.e. empty environment (\code{emptyenv()}) 
#' or \code{R_EmptyEnv}.
#' @return environment/package name(s) where the function x is found.
#' R_GlobalEnv is the global environment (\code{globalenv()}). If 
#' function x is not found, \code{NULL} is returned. If multiple 
#' environments are found, the first environment is the default place to 
#' call the function x.
#' @export
#' @examples 
#' \dontrun{
#' detachPackages("Matrix")
#' where2SearchFun("print")
#' where2SearchFun(print)
#' 
#' # Matrix::print is the default print
#' library(Matrix)
#' where2SearchFun("print")
#' identical(print, Matrix::print)
#' identical(print, base::print)
#' 
#' print = function() print()
#' where2SearchFun("print")
#' }
where2SearchFun = function(x, env = parent.frame()) {
  if (tryCatch(!is.character(x), error = function(e) TRUE)) 
    x = as.character(substitute(x))
  
  res = c()
  while(!identical(env, emptyenv())){
    if (exists(x, envir = env, mode = "function", inherits = FALSE)) {
      res = c(res, .rmpkg(environmentName(env)))
    }
    env = parent.env(env)
  }
  return(res)
}