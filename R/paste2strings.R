#' @title Paste two strings without a space
#' 
#' @return Paste two strings without a space
#' @param x character
#' @param y character
#' @rdname Paste2strings
#' @export
"%,%" = function(x, y){
  paste0(x, y)
}

#' @title Paste two strings with a space
#' 
#' @return Paste two strings with a space
#' @param x character
#' @param y character
#' @rdname Paste2strings
#' @export
#' @examples {
#' "a" %,% "b"
#' "a" %, % "b"
#' "folder" %/% "file"
#' }
"%, %" = function(x, y){
  paste(x, y)
}

#' @title Paste two strings with a space
#' 
#' @return Paste two strings with a slash (/)
#' @param x character
#' @param y character
#' @rdname Paste2strings
#' @export
"%//%" = function(x, y){
  paste0(x, "/", y)
}
