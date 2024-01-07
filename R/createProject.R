#' Create a project folder, where it contains "0_RawData", "Rdata", "Rscript",
#' "figs", "csv", and "ppt" folders. And create two R script files
#' ("00_01_functions.R" and "01_01_readData.R") in Rscript folder. If \code{.git = TRUE},
#' then a git repository will be initialized.
#' @param proj project name
#' @param path the path where the project is created
#' @param .git logical, whether to create a git repository, default is FALSE
#' @param showWarnings logical, whether to show warnings when create the folders, 
#' the default is FALSE
#' @param inheritProj the project folder to be inherited, default is NULL
#' @param inheritFolders the folder in the inheritProj project to be copied, 
#' the default is c("Rscript")
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
#' 
#' # Inherit from a project
#' createProject("tempProj4")
#' file.create("tempProj4/0_RawData/tmpdataFile.txt") # make a file in 0_RawData folder
#' createProject("tempProj5", inheritProj = "tempProj4", inheritFolders = c("Rscript", "0_RawData"))
#' unlink("tempProj4",recursive = TRUE) # remove this folder
#' unlink("tempProj5",recursive = TRUE) # remove this folder
#' }
#' @export
createProject = function(proj, path = "./", .git = FALSE, showWarnings = FALSE, 
                         inheritProj = NULL, inheritFolders = c("Rscript")){
  path1 = file.path(path, proj)
  if(dir.exists(path1)){
    stop(path1, " (", normalizePath(path1), ") exists. Change another proj name.")
  }
  
  dirs = paste0(path1, c("", "/0_RawData", "/Rdata", "/Rscript", "/figs", "/csv", "/ppt"))
  for(mi in dirs){
    dir.create(mi, showWarnings = showWarnings, recursive = T)
  }
  
  if(is.null(inheritProj)){
    files = paste0(path1, c("/Rscript/00_01_functions.R", "/Rscript/01_01_readData.R"))
    if(file.exists(paste0(path1, "/Rscript"))){
      file.create(files)
    }
    
  } else if (!is.null(inheritProj) && dir.exists(file.path(inheritProj, inheritFolders))){
    files = c()
    for(mi in seq_along(inheritFolders)){
      from = file.path(inheritProj, inheritFolders)[mi]
      files0 = dir(from, all.files = T, full.names = T, include.dirs = T, no.. = T)
      folder1 = file.path(path, proj, inheritFolders[mi])
      file.copy(files0, to = folder1, recursive = T)
      files = c(files, file.path(folder1, basename(files0)))
    }
  } else {
    stop("inheritFolders does not exist.")
  }
  x = system("which git", intern = T)
  if(length(x) == 1 && .git){
    cmd = paste("cd", path1, "; git init")
    system(cmd)
  }
  return(list(dirs = dirs, files = files))
}
