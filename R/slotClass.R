
#' Obtain class names of slots in an S4 object
#' 
#' @description get class names of slots in an S4 object, which is similar 
#' to \link{\code{getSlots}} function, but \code{getSlots} get class names of 
#' slots based on a class name not an object instance.
#' @param object an S4 object
#' @param slot_names character, the class of which slot to obtain. Default is NULL, 
#' meaning obtain the class name of all slots that object has.
#' @return a list of class names for either all slots (if slot_names = NULL) or for
#' the slots that `slot_names` specifys.
#' @export
#' @examples 
#' \dontrun{
#' ## Suppose SE is an object of SummarizedExperiment class
#' slotClass(SE)
#' slotClass(SE, "colData")
#' }
slotClass = function(object, slot_names = NULL){
  slot_names0 = setNames(slotNames(object), slotNames(object))
  if(is.null(slot_names)){
    slot_namesRes = slot_names0
  } else {
    if(!is.character(slot_names)) 
      stop("slot_names must be either NULL or characters.")
    slot_names0 = intersect(slot_names, slot_names0)
    slot_names0 = setNames(slot_names0, slot_names0)
    if(length(slot_names0) == 0){
      stop("Slot(s) not found:\n  ", slot_names)
    } else {
      slot_namesRes = slot_names0
    }
  }
  res = lapply(slot_namesRes, function(x){
    y = slot(object, x)
    class(y)
  })[slot_namesRes]
  names(res) = slot_namesRes
  return(res)
}