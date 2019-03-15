#' Sort data.frame according to multiple columns.
#' @param x a data.frame to sort.
#' @param by either a data.frame, or a vector of characters, or a vector
#' of integers. The data.frame x is sorted according to this parameter.
#' If it is a data.frame, then the column names should be a
#' part of column names of x. A vector of characters must not exceed the
#' column names of x. The integer vector is the index of columns.
#' @param returnID logic. If FALSE (default), the sorted index is not
#' returned.
#' @param decreasing logic. Should the sort order be increasing (default)
#' or decreasing?
#' @return A sorted data.frame according to 'by'.
#' @export
#' @examples
#' \dontrun{
#' x = data.frame(a = c(3,3,6,4,4), b = c(4,2,2,1,1), c = 1:5)
#' sortDataframe(x)
#' sortDataframe(x, by = c("a", "b"))
#' sortDataframe(x, by = 1:2)
#' sortDataframe(x, by = x[, 1:2])
#' sortDataframe(x, returnID = TRUE)
#' }
sortDataframe = function(x, by = x, decreasing = FALSE, returnID = FALSE){
  stopifnot(is.data.frame(x))
  if (is.character(by)){
    nm = by
  } else if (is.data.frame(by)){
    nm = colnames(by)
  } else if (is.integer(by)){
    nm = colnames(x)[by]
  } else {
    stop("Unknown class of 'by'")
  }
  stopifnot(all(nm %in% colnames(x)))
  cmd = paste0("with(x, order(", paste0(nm, collapse = ","), ", decreasing = decreasing))")
  id = eval(parse(text = cmd))
  y = x[id,]
  if (returnID){
    y = list(res = y, id = id)
  }
  return(y)
}
