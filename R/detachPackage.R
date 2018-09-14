#' detach packages
#' @param packages package names
#' @export
#' @return removed package name(s)
#' @seealso \code{\link{detachPackagesAll}}
#' @examples
#' \dontrun{
#' # detach one package
#' library(funcTools)
#' detachPackages("funcTools")
#'
#' # detach multiple packages
#' library(funcTools)
#' library(ggplot2)
#' library(stringr)
#' detachPackages(c("funcTools", "ggplot2", "stringr"))
#' }
detachPackages = function(packages, prefix = "package:"){
  rmPackages = c()
  for(i in packages){
    p = paste0(prefix, i)
    if (p %in% search()){
      rmPackages = c(rmPackages, i)
      detach(p, unload = TRUE, character.only = TRUE)
    }
  }
  return(rmPackages)
}

#' detach all packages except basic packages
#' @description detach all packages except "base", "methods", "datasets",
#' "utils", "graphics", "stats", and "grDevices".
#' @return removed package name(s)
#' @export
#' @seealso \code{\link{detachPackages}}
#' @examples
#' \dontrun{
#' library(funcTools)
#' library(ggplot2)
#' library(stringr)
#' detachPackagesAll()
#' }
detachPackagesAll = function(){
  all = grep("^package:", search(), value = T)
  notRM = c("base", "methods", "datasets", "utils", "graphics", "stats", "grDevices")
  toRemove = all[! all %in% paste0("package:", notRM)]
  rmPackages = detachPackages(toRemove, prefix = "")
  return(rmPackages)
}
