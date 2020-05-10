#' non-generic functions that can generate generic using "getGeneric"
#' @param package package name.
#' @param envir environment, which can transformed using an attached package name.
#' default is as.environment("package:base")
#' @return the name of non-generic functions that can generate generic functions 
#' using \code{\link{getGeneric}} function.
#' @export
#' @examples
#' \dontrun{
#' functionsCanGetGeneric()
#' functionsCanGetGeneric(as.environment("package:methods"))
#' 
#' # In all attached packages
#' lapply(setNames(rev(search()), rev(search())), function (x){
#'        functionsCanGetGeneric(as.environment(x))})
#'        
#' c("cov") %in% functionsCanGetGeneric(env = as.environment("package:stats"))
#' c("which.min", "which") %in% functionsCanGetGeneric()
#' }
functionsCanGetGeneric = function(package, 
                                  env = as.environment("package:base")){
  if (!missing(package)){
    env = as.environment(paste0("package:", package))
  }
  envName = environmentName(env)
  envName_rm = sub("^.+:", "", .rmpkg(envName))
  res = unlist(lapply(ls(envir = env, all = TRUE), function(x) {
    fun = get(x, envir = env)
    
    nullGeneric = if (envName_rm == "R_GlobalEnv"){
      is.null(getGeneric(x, where = .GlobalEnv))
    } else {
      is.null(getGeneric(x, package = envName_rm))
    }
    if((!is.primitive(fun) && isGeneric(x, where = env)) || 
       nullGeneric) {
      NULL 
    } else {x}
  }))
  return(res)
}


