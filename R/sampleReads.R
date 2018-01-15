#### sample reads based on some probability
#' @title sampleReads
#' @description Provide gene reads of bulk sequencing data, do downsampling
#' to get a new set of gene reads to mimic the single cell sequencing data.
#' @param reads Vector of gene names (factor or character) or a data frame
#' (or matrix) of gene names and corresponding frequencies.
#' @param size Integer, the number of reads of downsampling.
#' @param rep Integer, the number of sampling to mimic the number of sigle
#' cells.
#' @param prob Either "dunif" or the numeric vector of gene reads representing
#' the prio probability to do sampling.
#' @param sd ***
#' @param addNoise Logical. If TRUE, add extra noise to the sampled data
#' @return A data frame, the row names of which are gene names, and each column
#' of which is the frequencies of genes in a single cell.
#' @export
sampleReads = function(reads, size = 10^5, rep = 10, prob = "dunif",
                       sd = 0, addNoise = FALSE){
  cl = class(reads)
  stopifnot(cl %in% c("factor", "character", "data.frame", "matrix"))
  if(cl %in% c("data.frame", "matrix")){
    stopifnot(ncol(reads) == 2 && is.integer(reads[,2]))
    reads = rep(reads[, 1], reads[, 2])
  }
  len = length(reads)

  # probability of each gene  ****** to be done
  if (is.character(prob) && prob == "dunif") {
    prob = dunif(x = 1:len, min = 1, max = len)
  } else {
    prob = prob[reads]/sum(prob[reads])
  }

  # sampling
  samp = lapply(1:rep, function(n) {
    sample(x = reads, size = size, replace = TRUE, prob = prob)})

  y = lapply(samp, function(x) {data.frame(table(x))})
  # yy = joinAllFrame(listFrame = y, by = "x")
  yy = cbindList(List = y, ID = "x", replace = 0, includeColId = FALSE)

  # add noise  ****** to be done
  if (addNoise){
    ad = function(x, mean, sig){
      y = x * rnorm(n = length(x), mean, sig)
      y[y<0] = 0
      return(y)
    }
    yy = yy + ad(yy, mean = 0, sig = 1)
  }
  # ad = function(x, total) {
  #   sig = x*22000/total
  #   rnorm(1, 0, sig)
  # }
  #
  # ad = function(x, total) {
  #   sig = 10^(log(x*22000, base = 10)/log(total, base = 10))
  #   rnorm(1, 0, sig)
  # }
  return(yy)
}
