#' Random forest gene regulatory network construction
#'
#' `grNetwork()` is a function for making gene regulatory network. Using
#' expression of transcription factor (or other genes of interest) and target
#' genes (each row represents a sample, each column denotes a gene),
#' it outputs a list of two data frames, i.e., weight and performance.
#' Data frame "weight" consists of out-node (from.gene), in-node (to.gene)
#' and edge weight (imp). Data frame "performance" consists of the statistics
#' of each random forest model, including mse, r, p, pVarExplaned.
#'
#' @param inputMatrix The expression matrix (sample x gene) of TFs or other
#' genes of interest, rownames and colnames must be provided.
#' @param outputMatrix The expression matrix (sample x gene) of target genes,
#' rownames and colnames must be provided.
#' @param K parameter to decide the number of genes to build a tree. Should be either
#' `sqrt` or `all` or an integer.
#' @param nbTrees Number of trees. The default is 1000.
#' @param importanceMeasure Importance type in \code{\link{randomForest}()},
#' either `IncNodePurity` or `%IncMSE`.
#' @param seed Random seeds. It is useful for reproducibility.
#' if It is not required, `NULL` should be used.
#' @param trace If `TRUE` (the default), indicative process will be shown
#' in console. This parameter will be not used if `fast` is `TRUE`.
#' @param fast When `FALSE` (the default), no parallel computing will be
#' performed, and the `cores` parameter will not be used. When `TRUE`,
#' parallel computing, and `cores` cores will be used.
#' @param cores The number of cores  (the default).
#' @param ... Other arguments passed on to \code{\link{randomForest}()}.
#' @export
#' @examples
#' \dontrun{
#' grNetwork(inputMatrix = input.matrix, outputMatrix = output.matrix0,
#' K="sqrt", nbTrees = 2500, importanceMeasure = impMeas,
#' seed = 1234, trace = FALSE, fast = TRUE, cores = 12)
#'
#' grNetwork(inputMatrix = input.matrix, outputMatrix = output.matrix0,
#' K = "sqrt", nbTrees = 1000, importanceMeasure = impMeas,
#' seed = 1234, trace = TRUE, fast = FALSE)
#'}
grNetwork <- function(inputMatrix, outputMatrix, K="sqrt", nbTrees=1000,
                      importanceMeasure="IncNodePurity",
                      seed = 1234, trace=TRUE, fast = FALSE,
                      cores = detectCores(), ...){
  ## All of the matrixs are sample * gene
  # inputMatrix is Gene Expression matrix of TFs
  # outputMatrix is Gene Expression matrix of All genes (or TFs)

  # Set random number generator seed if seed is provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  # Report when parameter importanceMeasure is not correctly spelled
  if (importanceMeasure != "IncNodePurity" && importanceMeasure != "%IncMSE") {
    stop("Parameter importanceMeasure must be \"IncNodePurity\" or \"%IncMSE\"")
  }
  # Check if nodesize parameter is in the input arguments
  args = list(...)
  nInArgs = "nodesize" %in% names(args)
  # In and out gene names
  outputGeneNames = colnames(outputMatrix)
  inputGeneNames = colnames(inputMatrix)
  # Local function
  .local = function(targetGeneIdx, inputGeneNames, outputGeneNames,
                    inputMatrix, outputMatrix, K, nbTrees,
                    importanceMeasure, seed, trace,
                    nodesizeInArgs = nInArgs, ...){
    # The number of input genes, names of input genes
    num.samples = nrow(outputMatrix)
    num.genes = ncol(outputMatrix)
    if (trace) {
      cat(paste("Computing gene ", targetGeneIdx, "/", num.genes, "\n", sep=""))
      flush.console()
    }
    targetGeneName = outputGeneNames[targetGeneIdx]
    # Remove target gene from input genes
    theseInputGeneNames = setdiff(inputGeneNames, targetGeneName)
    # x and y
    x = inputMatrix[, theseInputGeneNames]
    numInputGenes <- length(theseInputGeneNames)
    y = outputMatrix[, targetGeneName]
    # Set mtry
    if (class(K) == "numeric") {mtry = K
    } else if (K == "sqrt") {mtry = round(sqrt(numInputGenes))
    } else if (K == "all") {mtry = numInputGenes
    } else {stop("Parameter K must be \"sqrt\", or \"all\", or an integer")}

    if (trace) {
      cat(paste("K = ", mtry,", ", nbTrees, " trees\n\n", sep=""))
      flush.console()
    }
    # Importance type
    if (importanceMeasure == "%IncMSE") {
      y = y
      importance0 = TRUE
      type0 = 1
    } else {
      y = y / sd(y) # Normalize output
      importance0 = FALSE
      type0 = 2
    }
    # Run randomForest
    if (nodesizeInArgs) {
      rf = randomForest(x, y, mtry=mtry, ntree=nbTrees, importance=importance0, ...)
    } else {
      # By default, grow fully developed trees
      rf = randomForest(x, y, mtry=mtry, ntree=nbTrees, importance=importance0, nodesize=1, ...)
    }
    # Extract the importance
    im = importance(rf, type = type0)
    weight = data.frame(from.gene = rownames(im),
                        to.gene = rep(targetGeneName, nrow(im)),
                        imp = im/num.samples)
    # Evaluate the performance of RF model
    y_hat = predict(rf)
    return(list(weight = weight,
                mse = sum((y_hat - y)^2)/length(y),
                r = as.numeric(cor(y_hat, y)),
                p = cor.test(y_hat, y)$p.value,
                pVarExplaned = rf$rsq[length(rf$rsq)]*100))
  }

  # Run .local()
  idx = setNames(1:num.genes, nm = outputGeneNames)
  tic = system.time({
    if (fast){
      # Parallel computing
      cl = makeCluster(cores)
      registerDoParallel(cl)
      on.exit(stopCluster(cl))
      res = foreach(targetGeneIdx = idx, .packages = "randomForest") %dopar% {
        .local(targetGeneIdx, inputGeneNames,
               outputGeneNames, inputMatrix,
               outputMatrix, K, nbTrees,
               importanceMeasure, seed, trace = FALSE, ...)
      }
      names(res) = outputGeneNames
    } else{
      res = lapply(idx, function(targetGeneIdx){
        .local(targetGeneIdx, inputGeneNames,
               outputGeneNames, inputMatrix,
               outputMatrix, K, nbTrees,
               importanceMeasure, seed, trace, ...)
      })
    }
  })
  # Elapsed time
  print(tic)
  # Combine data
  weight = do.call("rbind", lapply(res, "[[", "weight"))
  colnames(weight) = c("from.gene", "to.gene", "imp")
  performance = data.frame(
    mse = do.call("c", lapply(res, "[[", "mse")),
    r = do.call("c", lapply(res, "[[", "r")),
    p = do.call("c", lapply(res, "[[", "p")),
    pVarExplaned = do.call("c", lapply(res, "[[", "pVarExplaned"))
  )
  return(list(weight = weight, performance = performance))
}
