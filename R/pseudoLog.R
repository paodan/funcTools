#' The Signed Pseudo Logarithm
#'
#' The domain of logarithm function is >0. However, sometimes it is required to
#' transform numbers using sort of "signed logarithm". The pseudoLog function
#' can achieve this goal.
#' @param x numeric value or vector that need to transform.
#' @param base base of logarithm.
#' @param Scaling factor for the linear part of pseudo-log transformation.
#' @param inverse whether the inverse function of pseudoLog is applied.
#'
#' @export
#' @examples
#' \dontrun{
#' x = c(-10^(4:1), 0, 10^(1:4))
#' y = pseudoLog(x)
#' print(y)
#' z = pseudoLog(x, inverse = TRUE)
#' }
#'
pseudoLog = function(x, base = 10, sigma = 1, inverse = FALSE){
  stopifnot(is.numeric(x))
  if (inverse){
    2 * sigma * sinh(x * log(base))
  } else {
    asinh(x/(2 * sigma))/log(base)
  }
}
