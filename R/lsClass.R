#' List all classes in packages
#' 
#' @param packages character or NULL, in which packages to list all classes.
#' Default is NULL, which means in all attached packages and the global 
#' environment.
#' @param pattern character, only the classes with the patter are listed. 
#' Default is NULL, meaning return all classes if exist.
#' @param asList logical, whether return a list object of all classes in 
#' each package, or by default (FALSE), return a data frame of three columns,
#' namely, index, package, and class.
#' @export
#' @examples 
#' \dontrun{
#' lsClass()
#' lsClass(pattern = "*functionInfo")
#' lsClass(asList = TRUE)
#' 
#' lsClass("funcTools")
#' lsClass("package:funcTools")
#' lsClass("methods")
#' 
#' lsClass("DESeq2")
#' lsClass(c("funcTools", "DESeq2"))
#' lsClass(packages = c("funcTools", "DESeq2", "asdf"))
#' lsClass(packages = c("funcTools", "DESeq2", "asdf", "asdf32"))
#' }
lsClass = function(packages = NULL, pattern = NULL, asList = FALSE){
  stopifnot(is.null(packages) | is.character(packages))
  stopifnot(is.null(pattern) | is.character(pattern))
  if (is.null(packages)){
    packages = search()
  }
  
  p0 = intersect(packages, c(".GlobalEnv", "Autoloads", "tools:rstudio"))
  packages0 = packages[!packages %in% p0]
  
  packages1 = packages0[(substr(packages0, start = 1, 8) != "package:")]
  if(length(packages1) > 0){
    packages = unique(c(p0, paste0("package:", packages1)))
  } else {
    packages = unique(c(p0, packages0))
  }
  
  inInstalled = packages %in% paste0("package:", installed.packages()[,1])
  notInstalledPackages = setdiff(packages[!inInstalled], 
                                 c(".GlobalEnv", "Autoloads", "tools:rstudio"))
  
  if (length(notInstalledPackages) > 0){
    mes = if (length(notInstalledPackages) == 1){
      "package is not installed, check if it is spelled right, or install it first:\n"
    } else {
      "packages are not installed, check if they are spelled right, or install them first:\n"
    }
    warning(length(notInstalledPackages), " ", mes, "  ",
            paste0("'", gsub("package:", "", notInstalledPackages), 
                   "'", collapse = ", "))
  }
  
  inSearch = packages %in% search()
  if (!all(inSearch)){
    notInSearchPackages = substr(packages[!inSearch], 9, 1000)
    mes = if (length(notInSearchPackages) == 1){
      "package is not loaded, load it by:\n"
    } else {
      "packages are not loaded, load them by:\n"
    }
    warning(length(notInSearchPackages), " ", mes, "  ",
            paste0("library(", notInSearchPackages, ")", collapse = "; "))
  }
  
  packages = setNames(packages, packages)
  cls = lapply(packages, function(x){
    if (x %in% search()){
      res <- ls(pos = x, pattern = "^\\.__C__", all.names = T)
      if(length(res) > 0){
        res = gsub("^\\.__C__", "",res)
        if(!is.null(pattern))
          res = grep(pattern, res, ignore.case = TRUE, value = TRUE)
      }
      res = data.frame(index = seq_along(res),
                       package = rep(gsub("package:", "", x), length(res)), 
                       class = res)
    } else {
      res <- NULL
    }
    return(res)
  })
  
  if(!asList){
    cls = as.data.frame(do.call("rbind", cls))
    rownames(cls) = seq_len(nrow(cls))
  } else {
    cls = lapply(cls, "[[", "class")
  }
  return(cls)
}
