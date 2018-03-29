#' Save a plot
#'
#' `plotSave()` is a convenient function for saving a plot. It saves
#' the plot that the plotCMD is to make, using the size of the current
#' graphics device by default. It also guesses the type of graphics device
#' from the extension. This function is very convinent when saving a plot
#' as a pdf or svg file and the letters in the plot need to be able to copied.
#'
#' @param filename File name to create on disk.
#' @param plotCMD Plot based on this plot command, cannot use a return
#' value of a plot command. for example define h = hist(rnorm(100)) and then
#' use plotCMD = h.
#' @param device Device to use. Can be either be a device function
#'   (e.g. [png()]), or one of "eps", "ps", "tex" (pictex),
#'   "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).
#' @param path Path to save plot to (combined with filename).
#' @param scale Multiplicative scaling factor.
#' @param width,height,units Plot size in `units` ("in", "cm", or "mm").
#'   If not supplied, uses the size of current graphics device.
#' @param dpi Plot resolution. Also accepts a string input: "retina" (320),
#'   "print" (300), or "screen" (72). Applies only to raster output types.
#' @param limitsize When `TRUE` (the default), `plotSave` will not
#'   save images larger than 50x50 inches, to prevent the common error of
#'   specifying dimensions in pixels.
#' @param returnPlot When `FALSE` (the default), plotSave will not return
#' the value that plotCMD might return.
#' @param ... Other arguments passed on to graphics `device`.
#' @export
#' @examples
#' \dontrun{
#' plotSave("mtcars.pdf", plotCMD = plot(mtcars$mpg, mtcars$wt))
#' unlink("mtcars.pdf")
#' unlink("mtcars.png")
#'
#' # work for only objects that \code{\link{print}} function can
#' # deal with (for example ggplot object)
#' g = ggplot(mtcars, aes(mpg, wt)) + geom_point()
#'
#' plotSave("mtcars.pdf", plotCMD = g)
#' plotSave("mtcars.png", plotCMD = g)
#'
#' plotSave("mtcars.pdf", width = 4, height = 4)
#' plotSave("mtcars.pdf", width = 20, height = 20, units = "cm")
#'
#' @seealso Similar usage can refer to \code{\link{ggplot}()}.
plotSave = function(filename, plotCMD, device = NULL, path = NULL, scale = 1,
                    width = NA, height = NA, units = c("in", "cm", "mm"),
                    dpi = 300, limitsize = TRUE, ...){
  plotCMDClass = class(plotCMD)
  notWorkClass = c("list", "numeric", "matrix", "data.frame",
                   "integer", "character", "logical")
  dev <- ggplot2:::plot_dev(device, filename, dpi = dpi)
  dim <- ggplot2:::plot_dim(c(width, height), scale = scale, units = units,
                            limitsize = limitsize)
  dev(file = filename, width = dim[1], height = dim[2])
  on.exit(utils::capture.output(grDevices::dev.off()))
  if (any(plotCMDClass %in% notWorkClass)){
    stop("Please use a right plot command but not return values of a plot command.")
  } else if (is.ggplot(plotCMD)){
    print(plotCMD)
  } else {
    plotCMD
  }
  invisible()
}
