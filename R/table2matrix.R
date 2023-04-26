
#' Convert a table object to a matrix
#' @param tbl a table object
#' @examples
#' \dontrun{
#' 
#' a = sample(letters[1:5], 20, replace = T) 
#' b = sample(letters[1:5], 20, replace = T) 
#' tbl = table(a, b)
#' View(tbl)
#' mat = table2matrix(tbl)
#' View(mat)
#' }
#' @export
table2matrix = function(tbl){
  matrix(tbl, nrow = dim(tbl)[1], dimnames = dimnames(tbl))
}

