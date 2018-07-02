#### sample reads based on some probability
#' @title sampleReads
#' @description Provide gene reads of bulk sequencing data, do downsampling
#' to get a new set of gene reads to mimic the single cell sequencing data.
#' @param reads Vector of gene names (factor or character) or a data frame
#' (or matrix) of gene names and corresponding frequencies.
#' @param size Integer, the number of reads of downsampling.
#' @param prob Either "dunif" or the numeric vector of gene reads representing
#' the prio probability to do sampling.
#' @return A data frame, the row names of which are gene names, and each column
#' of which is the frequencies of genes in a single cell.
#' @examples
#' \dontrun{
#' read0 = sample(paste0("gene", 1:10^7), size =3*10^6, replace = TRUE)
#' readExp1 = sampleReads(reads = read0, size = 10^4, prob = "dunif")
#' readExp2 = sampleReads(reads = read0, size = 10^4,
#'                        prob = rep(1/length(read0), length(read0)))
#' }
#' @export
sampleReads = function(reads, size = 10^5){
  cl = class(reads)
  stopifnot(cl %in% c("factor", "character", "data.frame", "matrix"))
  if(cl %in% c("data.frame", "matrix")){
    # make sure that count of a transcript = 0 is possible
    reads[,1] = factor(reads[,1], levels = reads[,1])

    stopifnot(ncol(reads) == 2 && is.integer(reads[,2]))
    reads = rep(reads[, 1], reads[, 2])
  } else if(cl %in% c("character")){
    reads = factor(reads, unique(reads))
  }

  # sampling
  samp = sample(x = reads, size = size, replace = TRUE)

  yy = data.frame(table(samp))
  rownames(yy) = yy[,1]
  yy = yy[, -1, drop = FALSE]

  return(yy)
}
