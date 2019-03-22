#' Find the shortest path
#' @param Adjacency matrix
#' @param start node
#' @param end node
#' @export
#' @examples  {
#' \donrun{
#' G = matrix(c(c(0, 6, 0, 1, 0),
#' c(6, 0, 5, 2, 2),
#' c(0, 5, 0, 0, 5),
#' c(1, 2, 0, 0, 1),
#' c(0, 2, 5, 1, 0))/10, nrow = 5, byrow = T,
#' dimnames = list(letters[1:5], letters[1:5]))
#'   # debugonce(findShortestPath)
#'   findShortestPath(G, start = "a", end = "c")
#'   findShortestPath(G, start = "b", end = "c") #
#'   findShortestPath(G, start = "d", end = "c")
#'   findShortestPath(G, start = "e", end = "c")
#'   findShortestPath(G, start = "a", end = "d")
#'
#'   # longest path (only work unweighted network)
#'   findShortestPath(-G, start = "e", end = "a")
#'
#'
#'   chengyu = list(a = c("ning que wu lan"),
#'                  b = c("ji quan bu ning"),
#'                  c = c("lan yu chong shu"),
#'                  d = c("zu chuang mi fang"),
#'                  e = c("shu dian wang zu"),
#'                  f = c("ji fei gou tiao"),
#'                  g = c("ning si bu qu"),
#'                  h = c("lan zui ru ni"),
#'                  i = c("niu dao ge ji"))
#'
#'   idiomDist = function(idiom, sep = " "){
#'     idiom = t(sapply(idiom, strSplit, sep))
#'     G1 = matrix(0, nrow = nrow(idiom), ncol = nrow(idiom),
#'                 dimnames = list(rownames(idiom), rownames(idiom)))
#'     for(mi in rownames(idiom)){
#'       G1[mi,] = idiom[, 1] == idiom[mi, 4]
#'     }
#'     return(G1)
#'   }
#'
#'   x = findShortestPath(-idiomDist(chengyu), start = "b", end = "d")
#'   unlist(chengyu[x$shortestPath], use.names = F)
#'
#' }
#' }
findShortestPath = function(G, start, end){
  # set the start node first and end node last
  nodes = colnames(G)
  nodes = c(start, nodes[!nodes %in% c(start, end)], end)

  # initiate the distance table
  dists = data.frame(dist = rep(Inf, ncol(G)),
                     previousNode = "",
                     row.names = nodes)
  dists[1,1] = 0
  # initiate the nodes to search
  IN = rownames(dists)
  while (length(IN) > 1){
    # neighbor = na.omit(G[IN[1], IN])
    neighbor = (G[IN[1], IN])
    neighbor = neighbor[!is.na(neighbor) & neighbor != 0]

    neighborCurrentDist = neighbor + dists[IN[1],1]
    neighborPreviousDist = setNames(dists[names(neighbor),1], names(neighbor))

    # check which neighbor nodes should be updated
    updateDist = pmin(neighborPreviousDist, neighborCurrentDist, na.rm = T)
    isupdate = neighborPreviousDist != updateDist

    # update distance table when the current distance of neighbor is smaller than previous distance
    dists[names(isupdate)[isupdate],2] = IN[1]
    dists[names(isupdate)[isupdate],1] = updateDist[isupdate]

    # move the current node out
    IN = IN[-1]
    # reorder the updated node accoding to the distance to the start node
    IN = unique(c(names(sort(updateDist[isupdate])), IN))
  }

  # dists = dists[colnames(G),]

  pathOrder = c(end)
  while(all(pathOrder != start)){
    # pathOrder = c(pathOrder, paste0(dists[pathOrder[length(pathOrder)],2], collapse = ","))
    pathOrder = c(pathOrder, dists[pathOrder[length(pathOrder)],2])
  }
  pathOrder = rev(pathOrder)
  shortestPath = paste0(pathOrder, collapse = " -> ")
  # dists = dists[pathOrder,]
  list(shortestLength = dists[nrow(dists),1],
       shortestPath = pathOrder,
       distanceTable = dists)
}
