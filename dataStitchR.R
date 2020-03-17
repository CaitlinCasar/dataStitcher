pacman::p_load(spatstat, geostatsp, maptools, cluster, stringr, smoothr, sf, lwgeom, units, raster, rgeos, imager,ggnewscale,  magick, stars, fasterRaster, ggplot2, cowplot, tidyverse, rgdal, rasterVis)

path <- "example_dataset"

files <- dir(path, pattern = "*.tif") # get file names
files <- paste(path, '/', files, sep="")

xy <- data.frame(x=c(0, 713, 1420, 2301, 2846, 3559, 4281), y=c(6, 5, 3, 40, 3, 2, 0))

panorama <- list()

for(i in 1:length(files)){
    image <- files[i] %>% image_read() %>% 
      image_quantize(colorspace = 'gray') %>% 
      image_equalize() 
    temp_file <- tempfile()
    image_write(image, path = temp_file, format = 'tiff')
    image <- raster(temp_file)
    image_extent <- extent(matrix(c(xy$x[i], xy$x[i] + 1024, xy$y[i], xy$y[i]+704), nrow = 2, ncol = 2, byrow = T))
    image_raster <- setExtent(raster(nrows = 704, ncols = 1024), image_extent, keepres = F)
    values(image_raster) <- values(image)
    panorama[[i]] <- image_raster
    #plot(image_raster, asp=1)
}
panorama_merged <- do.call(merge, panorama)
plot(panorama_merged, col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL))
