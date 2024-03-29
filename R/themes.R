#' ggplot theme for publication ready Plots
#' @param base_size font base size.
#' @param base_family font base family.
#' @param x_angle x label angle.
#' @param x_hjust x horizontal alignment.
#' @param x_vjust x vertical alignment.
#' @param y_angle y label angle.
#' @param y_hjust y horizontal alignment.
#' @param y_vjust y vertical alignment.
#' @param legend.key.size legend key size.
#' @import ggplot2
#' @import ggthemes
#' @export
theme_Publication <- function(base_size=14, base_family=c("Helvetica", "sans"),
                              x_angle = 0, x_hjust = 0.5, x_vjust = 0.5,
                              y_angle = 0, y_hjust = 0.5, y_vjust = 0.5,
                              legend.key.size= unit(0.3, "cm"), gridLineSize = .8) {
  base_family = match.arg(base_family)
  (theme_foundation(base_size=base_size, base_family=base_family)
   + theme(plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
           text = element_text(),
           panel.background = element_rect(fill = "transparent", colour = NA) # bg of the panel
           , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
           , legend.background = element_rect(fill = "transparent", colour = NA) # get rid of legend bg
           , legend.box.background = element_rect(fill = "transparent", colour = NA),
           # panel.background = element_rect(colour = NA),
           # plot.background = element_rect(colour = NA),
           panel.border = element_rect(colour = NA),
           axis.title = element_text(face = "bold",size = rel(1)),
           # axis.title.y = element_text(vjust =y_vjust, hjust = y_hjust, face="plain"),
           # axis.title.x = element_text(vjust =x_vjust, hjust = x_hjust, face="plain"),#(vjust = -0.2),
           axis.title.y = element_text(vjust =0.5, hjust = 0.5, face="plain"),
           axis.title.x = element_text(vjust =0.5, hjust = 0.5, face="plain"),#(vjust = -0.2),
           axis.text = element_text(),
           axis.text.x = element_text(angle=x_angle, vjust =x_vjust, hjust = x_hjust), ## maybe need to remove
           axis.text.y = element_text(angle=y_angle, vjust =y_vjust, hjust = y_hjust), ## maybe need to remove
           axis.line = element_line(colour="black"),
           axis.ticks = element_line(),
           panel.grid.major = element_line(colour="#f0f0f0", size = gridLineSize),
           panel.grid.minor = element_blank(),
           legend.key = element_rect(colour = NA, fill = "transparent"),
           legend.position = "right",
           legend.direction = "vertical", #"horizontal",
           legend.key.size= legend.key.size,
           # legend.margin = unit(0, "cm"),
           # legend.margin = margin(1,1,1,1, "cm"),
           # legend.title = element_text(face="italic"),
           legend.title = element_text(),
           plot.margin=unit(c(10,5,5,5),"mm"),
           strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
           strip.text = element_text(face="plain")
   ))
}

#' Change discrete fill color of ggplot
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @export
scale_fill_Publication <- function(fillColor = c("#386cb0","#fdb462","#7fc97f","#ef3b2c",
                                                 "#662506","#a6cee3","#fb9a99","#984ea3",
                                                 "#ffff33"), ...){
  discrete_scale("fill","Publication",
                 manual_pal(values = fillColor), ...)
}

#' Change discrete colour color of ggplot
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @export
scale_colour_Publication = function(colourColor = c("#386cb0","#fdb462","#7fc97f","#ef3b2c",
                                                    "#662506","#a6cee3","#fb9a99","#984ea3",
                                                    "#ffff33"), ...){
  discrete_scale("colour","Publication",
                 manual_pal(values = colourColor), ...)
}


#' Make the ggplot transparent
#' @import ggplot2
#' @import ggthemes
#' @export
theme_transparent <- function(){
  (theme_foundation()
   + theme(
     panel.background = element_rect(fill = "transparent") # bg of the panel
     , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
     , legend.background = element_rect(fill = "transparent") # get rid of legend bg
     , legend.box.background = element_rect(fill = "transparent")
   ))
}



#' Change the x and y axis ticks of ggplot
#' @param gplot a ggplot object.
#' @param xBreaks the x axis ticks.
#' @param yBreaks the y axis ticks.
#' @return a ggplot object with changed x and y axis ticks.
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' dat = data.frame(x = rnorm(100), y = rnorm(100))
#' g = ggplot(dat, aes(x,y)) +
#'   geom_point()
#' scale_xy_ticks(g, pretty(dat$x, n = 10), pretty(dat$y, n = 10))
#' }
scale_xy_ticks = function(gplot, xBreaks = NULL, yBreaks = NULL){
  if (!is.null(xBreaks)){
    gplot = gplot + scale_x_continuous(breaks = xBreaks)
  }
  if (!is.null(yBreaks)){
    gplot = gplot + scale_y_continuous(breaks = yBreaks)
  }
  return(gplot)
}
