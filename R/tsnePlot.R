#' make a tSNE plot for both the new clusters and original clusters
#' @param object Seurat object which have done with \code{\link{run_tsne}()}.
#' @param newCluster Character, new cluster type, one of 'DBclust', 'FindClusters' or 'Original'.
#' @param ... parameters in \code{\link{geom_point}()}, for example alpha = 0.5.
#' @return an ggplot object plotting a tSNE with two-cluster information,
#' new clusters and original clusters.
#' @import ggplot2
#' @import Seurat
#' @importClassesFrom Seurat seurat
#' @examples
#' SeuratObject = DBclust_dimension(SeuratObject, 1, 2, reduction.use = "tsne",
#' MinPts = 1, G.use = 3, set.ident = TRUE)
#' tsnePlot(SeuratObject, newCluster = "DBclust")
#'
#' SeuratObject = FindClusters(dcdat, pc.use = 1:5, k.param = 30, k.scale = 6,
#' do.modularity = T, resolution = 1.4)
#' tsnePlot(SeuratObject, newCluster = "FindClusters")
#'
#' tsnePlot(SeuratObject, newCluster = "Original")
#'
#' @seealso \code{\link{tsne.plot}()}, \code{\link{tsne}()}, \code{\link{Rtsne}()}
#' @export
setGeneric(name = "tsnePlot", def = function(object, readCount = NULL, newCluster = "DBclust", ...) {
  standardGeneric("tsnePlot")
})
setMethod(f = "tsnePlot", signature = "seurat",
          definition = function(object, readCount = NULL, newCluster = "DBclust", ...){
            data.info = object@data.info
            newClusterName = paste0(newCluster, ".ident")
            if (newCluster == "DBclust"){
              data.info$DBclust.ident = as.factor(data.info$DBclust.ident-1)
            } else if (newCluster == "FindClusters"){
              data.info$FindClusters.ident = as.factor(as.integer(object@ident))
            } else if (newCluster == "Original"){
              data.info$Original.ident = object@data.info$orig.ident
            } else stop("Unknown newCluster: newCluster can be 'DBclust', 'FindClusters' or 'Original'.")

            tsne12 = data.frame(object@tsne.rot, data.info)
            if (newClusterName %in% colnames(tsne12)){
              tsne12$new.ident = tsne12[[newClusterName]]
            } else stop(newCluster, " not found in 'object'.")

            if (!is.null(readCount)){
              tsne12$readCount = readCount[rownames(object@tsne.rot)]
              p = ggplot(tsne12, aes(tSNE_1, tSNE_2, size = readCount, color = new.ident,
                                     shape = orig.ident))+
                geom_point(...)
            } else {
              p = ggplot(tsne12, aes(tSNE_1, tSNE_2, color = new.ident,
                                     shape = orig.ident))+
                geom_point(...)
            }
            return(p)
          })
