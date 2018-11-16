#' Convert network edges to adjacency matrix
#' @param edge a data.frame of three columns, the first two columns
#' represent nodes in a network, the third column is the weight.
#' @param rownm row names in the final adjacency matrix. All node names
#' must present in either rownm or colnm.
#' @param colnm column names in the final adjacency matrix. All node names
#' must present in either rownm or colnm.
#' @param direction logic, whether this network is directed,
#' the default is TRUE.
#' @param rmName logic, whether to remove row names and column names in the
#' adjacency matrix.
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
#' edge2Mat(edge, nodes, nodes,direction = FALSE)
#' edge2Mat(edge, nodes, nodes,direction = FALSE, rmName = TRUE)
#'
#' edge = data.frame(c(1:3), c(3:5), c(1,1,1))
#' edge2Mat(edge,  edge[,1],  edge[,2] )
#' }
edge2Mat = function(edge, rownm, colnm, direction = TRUE,
                    rmName = FALSE, restValue = 0){
  stopifnot(ncol(edge) == 3)

  if(!is.character(edge[,1])) edge[,1] = as.character(edge[,1])
  if(!is.character(edge[,2])) edge[,2] = as.character(edge[,2])

  stopifnot((edge[,1] %in% rownm) && (edge[,2] %in% colnm))

  m = matrix(restValue, length(rownm), length(colnm),
             dimnames = list(rownm, colnm))
  m[as.matrix(edge[,1:2])] = edge[,3]

  if (!direction){
    if (!all(rownm == colnm)){
      stop("If direction is FALSE, rownm must be identical to colnm!")
    } else {
      m[as.matrix(edge[,2:1])] = edge[,3]
    }
  }
  if (rmName){
    dimnames(m) = NULL
  }
  return(m)
}


#' Adjacency matrix to a data.frame of edges.
#' @param mat adjacency matrix.
#' @param mode Character, to specify the class of graph and which part of
#' the matrix will be used. Possible values are: "directed" (default),
#' "undirected", "upper", "lower".
#' @param diag logic, whether to include the diagonal of the matrix.
#' @return a data.frame of edge information. The first column is from node,
#' the second column is to node, and the third is weight.
#' @import igraph
#' @examples {
#' \dontrun{
#' mat = matrix(rnorm(4*4), nrow = 4,
#'              dimnames = list(letters[1:4], LETTERS[1:4]))
#' mat2Edge(mat, mode = "undirected", diag = TRUE)
#' mat2Edge(mat, mode = "undirected", diag = FALSE)
#' mat2Edge(mat, mode = "directed", diag = TRUE)
#' mat2Edge(mat, mode = "upper", diag = TRUE)
#' mat2Edge(mat, mode = "upper", diag = FALSE)
#' }
#' }
#' @seealso edge2Mat
#' @export
mat2Edge = function(mat, mode = c("directed", "undirected", "upper", "lower"),
                    diag = FALSE){
  stopifnot(is.matrix(mat))
  mode = match.arg(mode)
  g = graph.adjacency(adjmatrix = mat, weighted = TRUE,
                      diag = diag, mode = mode)
  edge = get.data.frame(g)
  return(edge)
}
