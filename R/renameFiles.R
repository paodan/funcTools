#' rename files
#' 
#' @param old the original file names. The path information IS allowed, 
#' @param new the new file names. Unless on purpose, the path information 
#' should NOT be included, because "/" in the new file names will be replaced 
#' by characters according to \code{replacement} parameter, and 
#' the files are saved in the same folder(s) as the \code{old} files.
#' @param replacement character(s) to replace the illegal characters in the
#' new names.
#' @param checkExtention logical, if TRUE (default) check if new names 
#' contains a file extension.
#' @importFrom fs path_sanitize
#' @importFrom tools file_ext
#' @examples
#' # old file names
#' tmpFolder = "./tmp"
#' dir.create(tmpFolder)
#' file1 = tempfile(pattern = "oldFile", tmpdir = tmpFolder, fileext = ".csv")
#' file2 = tempfile(pattern = "oldFile", tmpdir = tmpFolder, fileext = ".csv")
#' write.csv(cars, file = file1)
#' write.csv(cars, file = file2)
#' dir(tmpFolder)
#' 
#' # new file names (must not contain the path, i.e. "./tmp/"!)
#' newNames = sub("./tmp/oldFile", "newFile", c(file1, file2))
#' renamed = renameFiles(old = c(file1, file2), new = newNames)
#' renamed
#' 
#' dir(tmpFolder)
renameFiles = function(old, new,
                       replacement = "_",
                       checkExtention = TRUE){
  if(!all(file.exists(old))){
    stop("The following files are not found according to 'fileNames':\n",
         paste0(old[!file.exists(old)], collapse = ","))
  }
  stopifnot(length(old) == length(new))
  
  paths = dirname(old)
  new2 = path_sanitize(new, replacement = replacement)
  
  if(checkExtention){
    if("" %in% file_ext(new2)){
      stop("Following new names do not have file extentions:\n", 
           paste0(new2[file_ext(new2) == ""], collapse = ", "), 
           "\n If you do it on purpose, ",
           "then set checkExtention = FALSE")
    }
  }
  
  beforeReplacement = setdiff(new, new2)
  replace = data.frame(
    new = character(),
    new2 = character()
  )
  if (length(beforeReplacement)){
    replace = data.frame(
      new = beforeReplacement,
      new2 = setdiff(new2, new)
    )
    message("Some new names are not allowed. ", 
            "They have been replaced as following:\n")
    print(replace)
  }
  
  file.rename(from = old, to = paste0(paths, "/", new2))
  return(list(replace = replace, 
              names = data.frame(path = paths, 
                                 old = basename(old), 
                                 new = new, 
                                 new2 = new2)))
}
