#' @title Make Unique Names\cr
#' @description make an character vector suitable for names by adding
#' an integer to the end of the characters.
#' @param x character vector for names (usually with duplicates).
#' @param sep a character to seperate the original character and added
#' an integer.
#' @return A character vector without duplicates.
#' @export
#' @examples {
#' \dontrun{
#' makeUniqueNames(c("a", "b" ,"a", "a"))
#' makeUniqueNames(c("a", "b" ,"a", "a", "b), ".")
#' }
#' }
#'
makeUniqueNames = function(x, sep = "_"){
  y = unique(x[duplicated(x)])
  for (mi in y){
    id1 = x == mi
    x[id1] = c(mi, paste0(mi, sep, 1:(sum(id1)-1)))
  }
  return(x)
}
