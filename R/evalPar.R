#' @title Evaluate parameters in one goal in the global environment\cr
#' @description Return a list of of parameters that changed, and
#' change the corresponding value in the global environment.
#' @param ... these arguments are of the form tag = value.
#' @param .prm named list of arguments to use
#' @param envir the environment in where parameters are to be evaluated.
#' @return Returns a list of parameters that have been changed.
#' @export
#' @examples {
#' \dontrun{
#' evalPar(a = list(x = c(1,2,5), y = 3:5),
#' c = NULL, e = numeric())
#'
#' # use .prm argument
#' tmp = list(a = list(x = c(1,2,5), y = 3:5))
#' evalPar(.prm = list(prm0 = tmp, a = 1, c = NULL,
#' e = numeric(), e2 = 1:3))
#' }}
#'
evalPar = function(..., .prm = NULL, envir = parent.frame(),
                   returnValue = TRUE){
  if (is.list(.prm)) {
    if(!is.null(names(.prm)) && !any(names(.prm) == "")){
      prm = .prm
    } else {
      stop(".prm must be a named list")
    }
  } else {
    # important to this function
    prm = as.list(substitute(list(...)))[-1L]
  }

  prmName = setNames(names(prm), names(prm))
  for(mi in prmName){
    # print(mi)
    if(is.language(prm[[mi]])){
      # print(eval(prm[[mi]]))
      assign(mi, eval(prm[[mi]]), envir = envir)
    } else {
      assign(mi, prm[[mi]], envir = envir)
    }
  }

  # return value
  if (returnValue){
    res = lapply(prmName, get, envir = envir)
  } else {
    res = NULL
  }
  cat("Assigned variables are:\n", paste0(names(prmName), collapse = ", "))
  invisible(res)
}

# # old version
# evalPar = function(..., .prm = NULL, envir = parent.frame()){
#   if (is.list(.prm)) {
#     prm = .prm
#   } else {
#     prm = list(...)
#   }
#   if (length(prm) == 0) return(prm)
#   prmName = names(prm)
#   stopifnot(all(!is.null(prmName)))
#   stopifnot(all(prmName != ""))
#   for(mi in names(prm)) {
#     assign(mi, prm[[mi]], envir = envir)
#   }
#   return(prm)
# }

