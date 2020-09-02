#' How much memory is presently/maximally used by R?
#' 
#' This function is inspired by pryr::mem_used function.
#' 
#' @param maximum locical, whether to show the maximun memory that R used.
#' Default is FALSE, which means the memory that R currently used.
#' @import pryr
#' @examples 
#' \dontrun{
#' mem_use2()
#' mem_use2(TRUE)
#' 
#' x = matrix(1.2, nrow = 10000, ncol = 1000)
#' mem_used2()
#' mem_used2(TRUE)
#' 
#' # After removing x, current memory recover but the maximun memory does not.
#' rm(x)
#' mem_used2()
#' mem_used2(TRUE)
#' }
mem_used2 = function(maximum = FALSE){
  show_bytes = utils::getFromNamespace("show_bytes", ns = "pryr")
  node_size = utils::getFromNamespace("node_size", ns = "pryr")
  
  if (maximum){
    mem = show_bytes(sum(gc()[, "max used"] * c(node_size(), 8)))
  } else {
    mem = show_bytes(sum(gc()[, 1] * c(node_size(), 8)))
  }
  return(mem)
}
