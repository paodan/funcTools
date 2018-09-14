#' Convert network edges to adjacency matrix
#' @param edge a data.frame of three columns, the first two columns
#' represent nodes in a network, the third column is the weight.
#' @param rownm row names in the final adjacency matrix. All node names
#' must present in either rownm or colnm.
#' @param colnm column names in the final adjacency matrix. All node names
#' must present in either rownm or colnm.
#' @param restValue the value to fill in the blank edges.
#' @return An adjacency matrix.
#' @export
#' @examples
#' \dontrun{
#' edge = data.frame(c("a", "b", "c"), c("a", "c", "d"), c(1,1,1))
#' edge2Mat(edge, edge[,1], edge[,2])
#'
#' nodes = unique(c(as.character(edge[,1]), as.character(edge[,2])))
#' edge2Mat(edge, nodes, nodes)
#'
#' edge = data.frame(c(1:3), c(3:5), c(1,1,1))
#' edge2Mat(edge,  edge[,1],  edge[,2] )
#' }
edge2Mat = function(edge, rownm, colnm, restValue = 0){
  stopifnot(ncol(edge) == 3)

  if(!is.character(edge[,1])) edge[,1] = as.character(edge[,1])
  if(!is.character(edge[,2])) edge[,2] = as.character(edge[,2])

  stopifnot((edge[,1] %in% rownm) && (edge[,2] %in% colnm))

  m = matrix(restValue, length(rownm), length(colnm),
             dimnames = list(rownm, colnm))
  m[as.matrix(edge[,1:2])] = edge[,3]
  return(m)
}
