#' Save a plot
#' @description `plotSave()` is a convenient function for saving a plot. It saves
#' the plot that the plotCMD is to make, using the size of the current
#' graphics device by default. It also guesses the type of graphics device
#' from the extension. This function is very convinent when saving a plot
#' as a pdf or svg file and the letters in the plot need to be able to copied.
#' @param filename File name to create on disk.
#' @param Plot Plot based on a return value of a plot command. The class of the
#' return value should be able to dealed by `plot`() function.
#' @param plotCMD Plot based on this plot command, cannot use a return
#' value of a plot command.
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
#' @import grid
#' @import ggplot2
#' @seealso Similar usage can refer to \code{\link{ggplot}()}.
#' @export
#' @examples {
#' \dontrun{
#' # using plotCMD parameter
#' plotSave("mtcars.pdf", plotCMD = plot(mtcars$mpg, mtcars$wt))
#' plotSave("mtcars.pdf", plotCMD = ggplot(mtcars, aes(mpg, wt)) + geom_point())
#'
#' plotSave("mtcars.pdf", plotCMD = plot(mtcars$mpg, mtcars$wt),
#'          width = 4, height = 4)
#' plotSave("mtcars.pdf", plotCMD = plot(mtcars$mpg, mtcars$wt),
#'          width = 20, height = 20, units = "cm")
#'
#' # using Plot parameter
#' g = ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' h = hist(rnorm(100))
#'
#' plotSave("mtcars.pdf", Plot = g)
#' plotSave("mtcars.png", Plot = h)
#'}
#'}
plotSave = function(filename, Plot = NULL, plotCMD = NULL, device = NULL,
                    path = NULL, scale = 1,width = NA,
                    height = NA, units = c("in", "cm", "mm"),
                    dpi = 300, limitsize = TRUE, ...){
  supportedClassesPlot = c(names(funCode(plot)@fS3@fName))
  supportedClassesGrid.draw = names(funCode(grid.draw)@fS3@fName)
  dev <- ggplot2:::plot_dev(device, filename, dpi = dpi)
  dim <- ggplot2:::plot_dim(c(width, height), scale = scale, units = units,
                            limitsize = limitsize)
  dev(file = filename, width = dim[1], height = dim[2])
  on.exit(utils::capture.output(grDevices::dev.off()))
  if (!is.null(Plot)){
    # if (!any(class(Plot) %in% supportedClassesPlot)){
    #   stop(paste0(class(Plot), " is not supported by the 'Plot' parameter",
    #               " presently, please use 'plotCMD' parameter instead."))
    # }
    if (any(class(Plot) %in% supportedClassesPlot)){
      plot(Plot)
    } else if (any(class(Plot) %in% supportedClassesGrid.draw)){
      grid.draw(Plot)
    } else {
      stop(paste0(class(Plot), " is not supported by the 'Plot' parameter",
                  " presently, please use 'plotCMD' parameter instead."))
    }

  } else {
    tmp = plotCMD
    if(is.ggplot(tmp)) plot(tmp)
  }
  invisible()
}
