#' The first/last values in a number sequence with a certain step
#' 
#' @param v a numeric vector.
#' @param step numeric, the step in the number sequence that you are interested
#' @param last logical, if TRUE the last values in the sequence are returned.
#' Default is FALSE, by which the first values are returned.
#' @param showWarning logical, whether to show warning. Default is TRUE.
#' @return a vector of first/last values in the sequence with a certain step.
#' This vector also contains an attribute (index), which shows the index of 
#' these first/last values in the vector v.
#' @examples 
#' # Number sequence is: (1, 2, 3), (5, 6, 7, 8, 9), (12), (22)
#' x = c(1:3, 5:9, 12, 22)
#' print(x)
#' 
#' # to show the first values in a number sequence with step = 1
#' firstContinue(x)
#' # To show the last values in a number sequence with step = 1
#' firstContinue(x, last = TRUE)
#' 
#' # (100), (10, 9), (3, 2, 1), (-10) 
#' x = c(100, 10, 9, 3, 2, 1, -10)
#' print(x)
#' 
#' # To show the first values in a number sequence with step = 1
#' firstContinue(x, -1)
#' # to show the last values in a number sequence with step = 1
#' firstContinue(x, -1, last = TRUE)
#' 
#' x = c(100, 10, 9, 3, 3, 3, 2, 1, 1, 1, -10)
#' # when step is 0, it is equivalent to unique function.
#' firstContinue(x, 0)
#' unique(x)
#' 
#' # When x is not sorted, by default a warning will be raised
#' # (100), (10, 9), (3, 2, 1), (11, 10), (-10)
#' x = c(100, 10, 9, 3, 2, 1, 11, 10, -10)
#' print(x)
#' firstContinue(x, -1)
#' # To suppress warning
#' firstContinue(x, -1, showWarning = FALSE)
#' 
#' @export
firstContinue = function(v = p0, step = 1, last = FALSE, 
                         showWarning = TRUE){
  dv = diff(v)
  if(showWarning){
    if(step > 0){
      if(any(dv < 0)){
        warning("When step >= 0, the lagged differences of v (diff(v)) should not be < 0,
              unless you do it on purpose.")
      }
    } else if (step < 0){
      if(any(dv > 0)){
        warning("When step < 0, the lagged differences of v (diff(v)) should not be > 0,
              unless you do it on purpose.")
      }
    }
  }
  
  idx = if (last){
    which(c(diff(v) != step, TRUE))
  } else {
    which(c(TRUE, diff(v) != step))
  }
  res = v[idx]
  attr(res, "index") = idx
  return(res)
}
