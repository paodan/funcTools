#' @title setwd2
#' @description setwd2 is to set working directory in case the folder
#' is on remote computer (e.g. HPC) but accessed locally.
#' @param dir A character string: path started from the root.
#' @param preDir A character string: if the remote path is accessed locally,
#' what path needs to be prefixed? If preDir = NULL, only dir is considered.
#' @param local A character string: local platform name, which can be
#' obtained by running \code{\link{sessionInfo}}()$running. If local = NULL,
#' then preDir will not be considered. Letter case is ignored.
#' @param remote A character string: remote platform name,
#' if NULL then preDir will not be considered. Letter case is ignored.
#' @return The present working directory before setting new one.
#' @seealso \code{\link{setwd}}
#' @examples
#' # set working directory as the local home
#' setwd2("~")
#'
#' # set working directory as remote home mounting path if remote is matched
#' \dontrun{setwd2(dir = "/Users/homename", preDir = "[mounting path]",
#'     local = "ubuntu", remote = "centos")}
#' @export
setwd2 = function(dir, preDir = NULL, local = "Ubuntu", remote = "CentOS"){
  if (is.null(preDir) || is.null(local) || is.null(remote)) {
    oldDir = setwd(dir)
  }
  sinfo = sessionInfo()
  machine = c(local = length(grep(local, sinfo$running, TRUE)) > 0,
              remote = length(grep(remote, sinfo$running, TRUE)) > 0)
  if (all(machine)) stop("Either local or remote can be matched.")
  if (!any(machine)) stop("One and must be one of local and remote can be matched.")
  if (machine[2]){
    oldDir = setwd(dir)
  } else{
    oldDir = setwd(file.path(preDir, dir))
  }
  return(invisible(oldDir))
}


