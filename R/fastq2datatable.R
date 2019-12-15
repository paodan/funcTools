#' Read fastq file and convert to a data frame
#' @param fastqFile a fastq file name
#' @rawNamespace import(data.table, except = c(melt, dcast))
#' @export
#' @seealso \code{\link{fastq2dataframe_rev}}
fastq2dataframe = function(fastqFile){
  fastq = fread(file = fastqFile, sep = "\t", header = F)
  n = nrow(fastq)
  if (n %% 4 != 0){
    stop("The row number of fastqFile is not multiple of 4, please check if the file is correct.")
  }

  fastq_table = data.frame(label = fastq$V1[seq(from = 1, to = n, by = 4)],
                           sequence = fastq$V1[seq(from = 2, to = n, by = 4)],
                           mark = fastq$V1[seq(from = 3, to = n, by = 4)],
                           score = fastq$V1[seq(from = 4, to = n, by = 4)],
                           stringsAsFactors = F)
  return(fastq_table)
}

#' reverse a fastq corresponding data frame to a data.table of fastq format, and save a fastq file
#' @param fastqDataframe a data frame of four columns, and the column names must be
#' "label", "sequence", "mark", and "score".
#' @param fileName the fastq file to be written.
#' @import data.table
#' @export
#' @seealso \code{\link{fastq2dataframe}}
#' @examples {
#' \dontrun{
#' # Generating information for 3 reads, each of which is length of 50.
#' len = 50
#' n = 3
#' sequences = sapply(1:n, function(x) paste0(sample(c("A", "T", "C", "G"), len, replace = T), collapse = ""))
#' base = 33:(33+42) # ASCII_BASE=33
#' mode(base) = "raw"
#' scores = sapply(1:n, function(x) paste0(sapply(sample(base[2:40], len, replace = T), rawToChar), collapse = ""))
#' labels = paste0("@sequence", 1:n) # "label" must start with `@`.
#'
#' fastq_test = data.frame(label = labels, sequence = sequences,
#'                         mark = "+", score = scores)
#'
#' # Saving file
#' fastq = fastq2dataframe_rev(fastq_test, "./fastq_test.fastq")
#'
#' # Reading fastq file to a data frame
#' fastqTable = fastq2dataframe("./fastq_test.fastq")
#' }
#' }
fastq2dataframe_rev = function(fastqDataframe, fileName = NULL){
  stopifnot(is.data.frame(fastqDataframe))
  stopifnot(all(colnames(fastqDataframe) == c("label", "sequence", "mark", "score")))
  stopifnot(nrow(fastqDataframe) > 1)

  fastq = data.table(as.vector(t(fastqDataframe)))
  if (!is.null(fileName)){
    if(length(grep(".+\\.fastq$", fileName, ignore.case = T)) == 0){
      fileName = paste0(fileName, ".fastq")
    }

    fwrite(fastq, file = fileName, col.names = F, quote = F, showProgress = T)
  }
  return(fastq)
}

