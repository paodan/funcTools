
#' [!+] Convert string into Windows compatible filename
#'
#' This is a function (make.filenames) from https://github.com/GegznaV/spMisc 
#' repository.
#' This function replaces special symbols (\code{\"  / : * ?  < > |}) in
#' a string \code{s} with \code{replacement} (by default it is underscore
#'  \code{_}). After replacement the  string can be used as a file name
#'  in Windows.
#'
#'
#' @param s A string.
#' @param replacement A symbol to replce unallowed symbols with.
#'        \code{replacement} must be an allowed symbol.
#' @param allow.space Logical. If \code{FALSE}, space will be
#'        repalaced with \code{replacement}. Default is \code{TRUE}.
#'
#' @return A string without special symbols.
#' @export
#'
#' @examples
#'  s <- '\\ / : * ? " < > |'
#'  makeFilenames(s)
#'  #> "_ _ _ _ _ _ _ _ _"
#'
#'  makeFilenames(s, allow.space = FALSE)
#'  #> "_________________"
#'
#'
#'  s2 <- "/Hello?"
#'  makeFilenames(s2)
#'  #> "_Hello_"
#'
#'  makeFilenames(s2, replacement = "-")
#'  #> "-Hello-"
#'
#' @family \pkg{spMisc} utilities

makeFilenames <- function(s, replacement = "_", allow.space = TRUE){
    replacement <- as.character(replacement)

    # Check if `replacement` is an allowed symbol:
    if (grepl(replacement, '\\\\/\\:\\*\\?\\"\\<\\>\\|') == TRUE | nchar(replacement)!=1)
    {
        stop(sprintf("Replacement symbol '%s' is not allowed.", replacement))
    }

    # Do the replacement
    s <- gsub('[\\\\/\\:\\*\\?\\"\\<\\>\\|]',replacement,s)
    if (allow.space == FALSE)
        s <- gsub('[[:space:]]',replacement,s)

    # Return result
    return(s)
}
