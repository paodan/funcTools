#' Judge if the slots in two S4 objects are identical
#' @param obj1 First S4 object
#' @param obj2 Second S4 object
#' @param slots The slot names, the default is NULL, which means all of the slot
#' names in the first object.
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
#' }
identicalSlots = function(obj1, obj2, slots = NULL){
  
  if(is.null(slots)){
    slots = slotNames(obj1)
  }
  
  stopifnot(any(slots %in% slotNames(obj1)) && any(slots %in% slotNames(obj2)))
  slots = setNames(slots, slots)
  FT = unlist(lapply(slots, function(x) {
    identical(slot(obj1, x), slot(obj2, x))
  }))
  return(tapply(names(FT), FT, c))
}
