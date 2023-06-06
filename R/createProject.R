#' Create a project folder, where it contains "0_RawData", "Rdata", "Rscript",
#' "figs", "csv", and "ppt" folders. And create two R script files
#' ("00_01_functions.R" and "01_01_readData.R") in Rscript folder. If \code{.git = TRUE},
#' then a git repository will be initialized.
#' @param proj project name
#' @param path the path where the project is created
#' @param .git logical, whether to create a git repository, default is FALSE
#' @param showWarnings logical, whether to show warnings when create the folders, default is FALSE
#' @return a list of two elements, i.e., `dirs` and `files`. `dirs` contains all
#' the folders that have been created. `files` contains all the files that have
#' been created.
#' @examples
#' \dontrun{
#' # Create 'tempProj' project in the current working directory
#' createProject("tempProj")
#' dir.exists("tempProj")
#' unlink("tempProj",recursive = TRUE) # remove this folder
#' # Create 'tempProj2' project in the home directory
#' createProject("tempProj2", path = "~/")
#' dir.exists("~/tempProj2")
#' unlink("~/tempProj2",recursive = TRUE) # remove this folder
#' # Create 'tempProj3' project in the home directory and initialize it as a git repository.
#' # But you need to install git in the system in advance.
#' createProject("tempProj3", path = "~/", .git = TRUE)
#' dir.exists("~/tempProj3/.git")
#' unlink("~/tempProj3",recursive = TRUE) # remove this folder
#' }
#' @export
createProject = function(proj, path = "./", .git = FALSE, showWarnings = FALSE){
  path = file.path(path, proj)
  if(dir.exists(path)){
    stop(path, " (", normalizePath(path), ") exists. Change another proj name.")
  }
  dirs = paste0(path, c("", "/0_RawData", "/Rdata", "/Rscript", "/figs", "/csv", "/ppt"))
  files = paste0(path, c("/Rscript/00_01_functions.R", "/Rscript/01_01_readData.R"))
  for(mi in dirs){
    dir.create(mi, showWarnings = showWarnings, recursive = T)
  }
  if(file.exists(paste0(path, "/Rscript"))){
    file.create(files)
  }
  x = system("which git", intern = T)
  if(length(x) == 1 && .git){
    cmd = paste("cd", path, "; git init")
    system(cmd)
  }
  return(list(dirs = dirs, files = files))
}