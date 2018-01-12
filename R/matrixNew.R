#' @description Generate random/diagonal/zeros/ones Matrix, the default is 3 by 3 matrix of 0
#' @name matrixNew
#' @title matrixNew
#' @param m Number of rows.
#' @param n  Number of columns. If n = NULL, then n = m.
#' @param x The value filling in the matrix .
#' @param diagonal Logical, only diagonal remains if TRUE
#' @param distr Distribution functions, e.g. rnorm, rt, runif, etc. 
#' If distr is provided, then x will be masked.
#' @param dimnames A dimnames attribute for the matrix: 
#' NULL or a list of length 2 giving the row and column names, respectively.
#' @return A random/diagonal/zeros/ones Matrix.
#' @examples 
#' # The zeros/ones matrix
#' matrixNew(4) # zeros of 4 by 4 matrix
#' matrixNew(4, 5) # 4 by 5
#' matrixNew(x = 1) # ones of 3 by 3 matrix
#' 
#' # The eye/diagonal matrix
#' matrixNew(m = 3, x = 1, diagonal = TRUE)
#' matrixNew(m = 5, n = 3, x = 1, diagonal = TRUE)
#' matrixNew(m = 3, x = 4, diagonal = TRUE)
#' 
#' # The random matrix
#' matrixNew(m = 4, n = 5, distr = rnorm, dimnames = list(letters[1:4], letters[1:5]))
#' matrixNew(m = 4, n = 5, distr = rnorm, diagonal = TRUE)
#' @export
matrixNew = function(m=3, n=NULL, x = 0, diagonal = FALSE, 
                     distr = NULL, dimnames = NULL){
  # m row, n column, x value
  # distr is rnorm, runif, ... random number producer function or defined by user
  if (is.null(n)) n = m
  stopifnot(m*n>0)
  if(is.null(distr) && !is.null(x)){
    x = rep(x, m*n)
  } else if(!is.null(distr)){
    x = distr(m*n)
  } else stop("At least provide x or distr!")
  y = matrix(x, nrow = m, ncol = n, dimnames = dimnames)
  
  # only diagonal remains
  if (diagonal){
    y[!diag(TRUE, m, n)] <- 0
  }
  return(y)
}
