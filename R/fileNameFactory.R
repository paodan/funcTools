#' A function factory to create a function that is used to create a file name with a certain extention.
#' @param path the path that is concatenated to a file name.
#' @param extension the file extension that is concatenated to a file name.
#' @param createFolder logical, whether to create the folder if the path doesn't exist.
#' @return fileNameFactory returns a function that create a file name. That function
#' has two attributs, i.e., `path` and `extension`
#' @examples
#' \dontrun{
#' Rdata = fileNameFactory(path = "Rdata/", ".Rdata")
#' Rdata
#' Rdata("asdf")
#'
#' x = 10
#' pngs = fileNameFactory("figs/", ".png")
#' pngs
#' pngs("pngFig", x)
#'
#' pdfs = fileNameFactory("pdfs/", ".png")
#' pdfs
#' pdfs("first/", "pngFile_", x)
#'
#' # Once the function is created, the path/extension will not be changed by changing the attributes
#' attr(pngs, "path") = "figs_2"
#' pngs
#' pngs("asdf")
#' # You need to recreate the function with the new path
#' pngs2 = fileNameFactory("figs_2/", ".png")
#' pngs2("asdf")
#' }
#' @export
fileNameFactory = function(path = "Rdata", extension = ".Rdata", createFolder = TRUE){
  res = function(...){
    file = paste0(path, "/", ..., extension, collapse = "")
    if(createFolder){
      dir.create(path = dirname(file), showWarnings = FALSE, recursive = TRUE)
    }
    return(file)
  }
  attr(res, "path") = path
  attr(res, "extension") = extension
  return(res)
}