#' @title Make a named list inside out\cr
#' @description Make a named list inside out. Then the elements of the list
#' becomes the names of list, and the names of list becomes its element.
#' @param namedList A named list.
#' @return A list fliped inside out.
#' @import reshape2
#' @export
#' @examples {
#' \dontrun{
#' a = list(a = c("a1", "a2", "a3"),
#'          b = c("a2", "b1", "b3"),
#'          d = c("d3", "b1", "a2"))
#' insideOutList(a)
#' }
#' }
insideOutList = function (namedList) {
  stopifnot(!is.null(names(namedList)))
  DF = melt(namedList)
  eleInList = as.character(unique(DF$value))
  names(eleInList) = eleInList
  res = lapply(eleInList, function(x) {
    DF[DF$value %in% x, 2]
  })
  return(res)
}
