#' Check the package reports on Bioconductor
#' @param package the package name
#' @param url0 the url of the latest bioconductor package reports.
#' @importFrom xml2 read_html
#' @importFrom XML htmlTreeParse xmlValue
#' @export
#' @examples 
#' \dontrun{
#' checkBiocReports()
#' checkBiocReports("FlowSOM")
#' }
checkBiocReports = function(package = "RegEnrich", 
                            url0 = "http://bioconductor.org/checkResults/devel/bioc-LATEST/"){
  url = paste0(url0, package)
  x = read_html(url)
  y = htmlTreeParse(x)
  
  tb = y$children$html[['body']][[6]]
  
  text = c()
  for(mi in seq_along(tb)){
    # print(tb[[mi]])
    # Sys.sleep(2)
    for (ni in seq_along(tb[[mi]])){
      tmp = xmlValue(tb[[mi]][[ni]])
      if (length(tmp) == 0) tmp = ""
      
      text = c(text, tmp)
    }
  }
  txt1 = matrix(head(text, 16), ncol = 8, byrow = T)
  txt2 = matrix(tail(text, -16), ncol = 7, byrow = T)
  res = rbind(txt1, cbind(rep("", nrow(txt2)), txt2))
  info = res[1:2, 1]
  # print(info)
  res_colname = res[1, c(-1, -8)]
  # print(res_colname)
  res = data.frame(res[-1, c(-1, -8)])
  colnames(res) = res_colname
  cat(info, " ", sep = "\n")
  
  # Check warning and error
  nError = sum(apply(res, 2, function(ii) substr(ii, 2, 6) == "ERROR" ))
  nWarning = sum(apply(res, 2, function(ii) substr(ii, 2, 8) == "WARNING"))
  if(nError > 0){
    message(nError, " ERROR(s) exists.")
  }
  if(nWarning > 0){
    message(nError, " WARNING(s) exists.")
  }
  
  return(res)
}

