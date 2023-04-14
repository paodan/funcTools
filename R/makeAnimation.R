
#' Use the image files to create a gif animation
#' @param imgs image file full names
#' @param pattern if imgs is missing, then the files with this pattern in the 
#' names will be included from path.
#' @param path image file path
#' @param fps number of frames per second
#' @param outputgif output gif file names
#' @import gtools
#' @import magick
#' @export
makeAnimation = function(imgs, pattern = NULL, path = NULL, fps = 2, 
                         outputgif = "output.gif"){
  dir.create(dirname(outputgif),F)
  if(missing(imgs)){
    stopifnot(!is.null(path))
    imgs = list.files(path, pattern, full.names = TRUE)
    imgs = gtools::mixedsort(imgs)
  }
  # img_list <- lapply(imgs, image_read)
  
  img_list = list()
  for(mi in seq_along(imgs)){
    print(mi)
    img_list[[mi]] = image_read(imgs[mi])
  }
  
  ## join the images together
  img_joined <- image_join(img_list)
  
  ## animate at 2 frames per second
  img_animated <- image_animate(img_joined, fps = fps)
  
  # ## view animated image
  # img_animated
  
  ## save to disk
  image_write(image = img_animated,
              path = outputgif)
  
}
