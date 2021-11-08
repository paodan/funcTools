#' Judge if the slots in two S4 objects are identical
#' @param obj1 First S4 object
#' @param obj2 Second S4 object
#' @param slots The slot names, the default is NULL, which means all of the slot
#' names in the first object.
#' @param isList logical, is the obj1 or obj2 a list, the default is FALSE.
#' @return A list of two elements (FALSE and TRUE) if both exists. 
#' The slot names in the FALSE element are not identical between two objects. 
#' In contrast, the slot names in the TRUE element are identical between two 
#' objects.
#' @export
#' @examples 
#' \dontrun{
#' library(Matrix)
#' i = c(1,3:8)
#' j = c(2,9,6:10)
#' x = 7 * (1:7)
#' (A = sparseMatrix(i, j, x = x))
#' identicalSlots(A, A)
#' 
#' y = x + 1
#' (B = sparseMatrix(i, j, x = y))
#' identicalSlots(A, B)
#' 
#' x = list(a = 1:3, b = letters)
#' y = list(a = 1:3, b = LETTERS)
#' identicalSlots(x, y, isList = T)
#' 
#' x = list(a = 1:3, b = letters, c = TRUE)
#' y = list(a = 1:3, b = LETTERS, c = TRUE)
#' identicalSlots(x, y, isList = T)
#' }
identicalSlots = function(obj1, obj2, slots = NULL, isList = FALSE){
  gSlots = function(o, s){
    if(is.list(o)){
      y = o[[s]]
    } else {
      y = slot(o, s)
    }
    y
  }
  gSlotName = function(o){
    if(is.list(o)){
      y = names(o)
    } else {
      y = slotNames(o)
    }
    y
  }
  if(is.null(slots)){
    slots = gSlotName(obj1)
  }
  
  stopifnot(any(slots %in% gSlotName(obj1)) && any(slots %in% gSlotName(obj2)))
  slots = setNames(slots, slots)
  FT = unlist(lapply(slots, function(x) {
    identical(gSlots(obj1, x), gSlots(obj2, x))
  }))
  return(tapply(names(FT), FT, list))
}
