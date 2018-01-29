#' repeat matrix x by m rows and n columns
#'
#' @param x a matrix or a numeric vector.
#' @param m the desired number of rows.
#' @param n the desired number of columns.
#' @return Return a matrix repeated m (rows) * n (columns) times
#' @export
#' @examples
#' matrixRep(rnorm(3), m = 2, n = 3)
#' matrixRep(matrix(rnorm(6), ncol = 2), m = 3, n = 4)
#'
#' matrixRep(t(c(T, F)), m = 4, n = 3)
#' matrixRep(t(letters[1:5]), m = 4, n = 1)
#' @seealso \code{\link{matrixNew}}
matrixRep = function(x, m = 1, n = 1){
  stopifnot(is.numeric(x) || is.matrix(x))
  stopifnot(m > 0 && n > 0)
  if(is.numeric(x)) x = as.matrix(x)
  # repeat row
  if (m > 1){
    mx = nrow(x)
    nx = ncol(x)
    z = matrix(rep(t(x), m), nrow = m*mx, ncol = nx, byrow = TRUE)
  } else z = x

  # repeat column
  if (n > 1){
    z = matrix(rep(z, n), nrow = m*mx, ncol = nx *n, byrow = FALSE)
  }
  return(z)
}
