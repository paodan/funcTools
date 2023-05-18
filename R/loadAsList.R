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
loadAsList = function(file){
  res = load(file)
  load(file)
  txt = paste0("list(", paste0(paste0(res, " = ", res), collapse = ", "), ")")
  e = parse(text = txt)
  return(eval(e))
}