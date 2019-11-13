#' list the variables that a function can access
#' @param fun function object.
#' @param printValue logical, if TRUE, print the value of each variable that
#' the \code{fun} can access. The default is FALSE.
#' @param variable2print character or vector of charactor or NULL. If it's NULL and
#' \code{printValue} is TRUE, the value of all variables will be printed. If this
#' is a (vector of) character and \code{printValue} is TRUE, then the value(s) of
#' variable(s) indicated by this (vector of) character will be printed. When there
#' is no such variable indicated by variable2print, a warning will be raised.
#' @return the name(s) of variable(s) which can be accessed by \code{'fun'} function.
#' @examples
#' \dontrun{
#' a = 1
#' f = function(b) {
#'   ab = 2
#'   innerF = function(c) ab * b * c
#'   return(innerF)
#' }
#' funObj = f(2)
#'
#' lsAccessibleVariable(f) # global environment.
#' lsAccessibleVariable(funObj) # environment within function f.
#' lsAccessibleVariable(funObj, T) # all variables in environment within function f.
#' lsAccessibleVariable(funObj, T, "ab") # variables 'ab' in environment within function f.
#' }

lsAccessibleVariable = function(fun, printValue = FALSE, variable2print = NULL){
  stopifnot(is.function(fun))
  stopifnot(is.logical(printValue))
  stopifnot(is.character(variable2print) || is.null(variable2print))

  env = environment(fun)
  res = ls(env)
  if (printValue){
    if (is.null(variable2print)){
      variable2print = res
    }
    res0 = intersect(res, variable2print)
    nosuchVar = setdiff(variable2print, res)
    if(length(res0) > 0){
      for (mi in 1:length(res0)){
        cat(mi, "/", length(res0), ":\n", rep("-", 10),
            " ", res0[mi], " ", rep("-", 10), "\n", sep = "")
        print(get(res0[mi], env))
        cat(rep("-", 20), "\n\n", sep = "")
      }
      if (length(nosuchVar) > 0){
        warning("No these variables:\n", nosuchVar)
      }
    } else {
      warning("No variable to print.\n")
    }
  }
  cat("The environment of the function (", as.character(substitute(fun)), ") is: ", sep = "")
  print(env)
  cat("\n")
  return(res)
}
