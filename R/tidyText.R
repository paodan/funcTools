#' Print text in a tidy way.
#' @param text the text to be tidy.
#' @param leftSign the left one of paired sign, default is "\\{".
#' @param commaSign the unpaired sign, default is ",".
#' @param rightSign the right one of paired sign, default is "\\}".
#' @param showN integer, the number of characters to show.
#' @param removeSpace logical, remove unnecessary space, the default is TRUE.
#' @export 
#' @examples 
#' text = 'list1:{A:{a = 1, b = 3, c = "4"}, B:{x = "", y = "1-3"}}'
#' tidyText(text)
#' 
tidyText = function(text, 
                    leftSign = "\\{", 
                    commaSign = ",", 
                    rightSign = "\\}",
                    showN = 1000,
                    removeSpace = TRUE){
  
  # looking for {
  left = gregexpr(pattern = leftSign, text)[[1]]
  names(left) = rep("left", length(left))
  
  # looking for ,
  comma = gregexpr(pattern = commaSign, text)[[1]]
  names(comma) = rep("comma", length(comma))
  
  # looking for }
  right = gregexpr(pattern = rightSign, text)[[1]]
  names(right) = rep("right", length(right))
  
  leftRight = sort(c(left, comma, right))
  cnt = cumsum(c(left = 1, comma = 0, right = -1)[names(leftRight)])
  
  whichLeft = which(names(cnt) %in% c("left"))
  whichComma = which(names(cnt) %in% c("comma"))
  whichLeftComma = which(names(cnt) %in% c("left", "comma"))
  
  
  pasteN = unlist(lapply(c(0, cnt[whichLeftComma]), 
                         function(x) {
                           paste0(c("\n", rep("\t", x)), collapse = "")}), 
                  use.names = F)
  
  leftOrComma = names(leftRight)[!names(leftRight) %in% "right"]
  ss_l_c = strSplit(text, paste0(leftSign, "|", commaSign))[1,]
  if(removeSpace){
    ss_l_c = removeSpace(ss_l_c)
  }
  
  # ss_l_c_lc = paste0(ss_l_c, c(left = "{", comma = ",")[c(leftOrComma, "")])
  ss_l_c_lc = paste0(ss_l_c, c(c(left = "{", comma = ",")[leftOrComma], ""))
  
  text2 = paste0(head(paste0(pasteN, ss_l_c_lc), -1), collapse = "")
  text2 = paste0(paste0(pasteN, ss_l_c_lc), collapse = "")
  cat(substr(text2, 1, 2000))
  return(invisible(text2))
}
