#' @title Split charactors vecotrs and save as a matrix\cr
#' @description funCode function is to obtain the source code of an R function
#' @param x character vector or factor, each element of which is to be split.
#' @param split character vector (or object which can be coerced to such)
#' containing regular expression(s) (unless fixed = TRUE) to use for splitting.
#' If empty matches occur, in particular if split has length 0,
#' x is split into single characters.
#' If split has length greater than 1, it is re-cycled along x.
#' @param fixed logical. If TRUE match split exactly, otherwise use regular
#' expressions. Has priority over perl.
#' @param perl logical. Should Perl-compatible regexps be used?
#' @param useBytes logical. If TRUE the matching is done byte-by-byte rather
#' than character-by-character, and inputs with marked encodings are not
#' converted. This is forced (with a warning) if any input is found which
#' is marked as "bytes" (see Encoding).
#' @param nameRow logical. Give names to each row if x has names and nameRow TRUE.
#'
#' @return A matrix of splited characters
#' @export
#' @examples {
#' \dontrun{
#' x = c(as = "asfef", qu = "qwerty", "yuiop[", "b", "stuff.blah.yech")
#' # split x on the letter e
#' strSplit(x, "e")
#' }
#' }
#'
strSplit = function(x, split, fixed = FALSE, perl = FALSE,
                     useBytes = FALSE, nameRow = FALSE) {
  nm = names(x)
  x = as.character(x)
  n = length(x)
  s = strsplit(x, split = split, fixed = fixed,
               perl = perl, useBytes = useBytes)
  nc = unlist(lapply(s, length))
  out = matrix("", n, max(nc))
  for (i in 1:n) {
    if (nc[i]) out[i, 1:nc[i]] = s[[i]]
  }
  if (nameRow && !is.null(nm)){
    row.names(out) = nm
  }
  return(out)
}

