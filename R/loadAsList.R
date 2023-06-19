#' Load data and assign it/them into a list
#' @param file the .Rdata file
#' @examples 
#' \dontrun{
#' x = "a"
#' y = 1
#' save(x,y, file = "~/Downloads/tmp_xy.Rdata")
#' rm(list = c("x", "y"))
#' loadAsList("~/Downloads/tmp_xy.Rdata")
#' }
#' @export
loadAsList = function(file){
  cat("Loading", normalizePath(file), "\n")
  `__res__` = environment()
  load(file, `__res__`)
  res = as.list(`__res__`)
  id = names(res) != "__res__"
  on.exit(cat("----- Successfully loaded -----\n\n"))
  return(res[id])
}