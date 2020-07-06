##### copyright: Weiyang Tao 2020-07-06

# signPosition is to find out where the sign is and make a summary table
signPosition = function(text, patterns = strsplit(c("\\{|\\}|\\[|\\]|\\:|,"), "\\|")[[1]],
                        pair1 = c("\\{", "\\}"), pair2 = c("\\[", "\\]")){
  patterns = strsplit(c("\\{|\\}|\\[|\\]|\\:|,"), "\\|")[[1]]
  indx = list()
  for(pattern in patterns){
    extrPos = gregexpr(pattern, text)
    indx = c(indx, extrPos)
  }
  names(indx) = patterns
  
  cbn = sort(do.call("c", indx))
  cbn_name = names(cbn)
  
  signPos = data.frame(indx = seq_along(cbn), Name = cbn_name,
                       sign = sub("[0-9]+", "", cbn_name),
                       eachId = gsub(paste0(".*([0-9]+)$"), "\\1", cbn_name),
                       pos = cbn)
  
  f = function(sp, p){
    signValue = c("\\{" = 1, "\\[" = 1, "\\}" = -1, "\\]" = -1, "\\:" = 0, "," = 0)
    sp$p = 0
    sp$p[sp$sign %in% p] = signValue[sp$sign[sp$sign %in% p]]
    return(cumsum(sp$p))
  }
  
  if (!is.null(pair1)){
    signPos$pair1 = f(signPos, pair1)
  }
  
  if(!is.null(pair2)){
    signPos$pair2 = f(signPos, pair2)
  }
  
  return(subset(signPos, pos >0))
}

# itemValue function to separate strings with ":" and ","
itemValue = function(str, z0){
  listStr = strsplit(str, ",")
  lapply(listStr, function(x){
    if(length(x) == 0){
      x = ""
    }
    y = strSplit(x, ":")
    
    z = if(ncol(y) == 2){
      setNames(y[,2], y[,1])
    } else {
      y[,1]
    }
    
    zz = lapply(z, function(w) {if (length(z0) != 0 && w %in% names(z0)){
      z0[w]
    } else {w}
    })
    
    return(zz)
  })
}


### convert the parameters of javascript into a list object
collapseJsCode = function(text){
  iv = list()
  repeat{
    signPos = signPosition(text)
    
    sum_p12 = apply(signPos[,6:ncol(signPos)], 1, sum)
    max_p12 = max(0, sum_p12)
    cat(max_p12, "\t")
    
    if(max_p12 == 0){
      break
    }
    
    maxIdx = which(sum_p12 == max(sum_p12))
    
    ref = data.frame(s1 = firstContinue(maxIdx),
                     s2 = firstContinue(maxIdx, last = T) + 1)
    ref$p1 = signPos[ref$s1, "pos"]
    ref$p2 = signPos[ref$s2, "pos"]
    
    ref_text = apply(ref, 1, function(x) substr(text, x[3]+1, x[4]-1))
    ref_textTitle = paste0("__", ref$p1, "_", ref$p2, "__")
    
    resTmp = setNames(as.list(ref_text), ref_textTitle)
    
    iv = itemValue(unlist(resTmp), iv)
    
    textSep = unlist(apply(data.frame(x1 = c(1, ref$p2 + 1),
                                      x2 = c(ref$p1 - 1, nchar(text))),
                           1, function(x) substr(text, x[1], x[2])))
    text = paste0(paste0(textSep, c(ref_textTitle, "")), collapse = "")
  }
  cat("\n")
  
  names(iv) = NULL
  return(iv)
}


text = textNew = '{"service":"GUIDED_HELP","params":[{"key":"context","value":"yt_web_kevlar_watch"}]},{"service":"CSI","params":[{"key":"c","value":"WEB"},{"key":"cver","value":"2.20200701.03.01"},{"key":"yt_li","value":"1"},{"key":"GetWatchNext_rid","value":"0x7eb3f0213efd0eb4"}]}'
iv = collapseJsCode(textNew)
str(iv)

urlSeed = "https://www.youtube.com/watch?v=iTV89Tqfmgk&list=PLsyeobzWxl7rXr9qxVZPbaoU7uUqP7iPM"
urlpage = readLines(urlSeed, warn = FALSE)
titleLine = grep("prefetchHintConfig",
                 urlpage,
                 perl = FALSE,
                 value = TRUE)
iv = collapseJsCode(text = titleLine)
str(iv)
