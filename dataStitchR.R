#!/usr/bin/env Rscript

#foreword 
print("This script was created by Caitlin Casar. DataStitchR stitches panoramic images of SEM images coupled to 
      x-ray energy dispersive spectroscopy.")

#load dependencies 
print("Loading script dependencies: optparse, raster, magick, tidyverse, rasterVis, randomcoloR...")
pacman::p_load(optparse, raster, magick, tidyverse, rasterVis, randomcoloR, ggnewscale)
print("...complete.")

option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="dataset file name", metavar="character"),
  make_option(c("-o", "--out"), type="character", default="out.txt", 
              help="output file name [default= %default]", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);


#user should run this script in the appropriate directory 
setwd("~/Desktop/dataStitcher/example_dataset")

#create list all sub-directories within main directory
directories <- list.files(full.names = F , recursive =F)
directories <- directories[!str_detect(directories, "Unknown|Os|SEM")]
positions <- str_extract(dir("SEM_images", pattern = "*.tif"),  "-?\\d") 

#set image coordinates
xy<- read_delim("../coordinates.txt", delim = "\t")
#xy <- read_delim(opt$file, delim = "\t")

# create base SEM image ---------------------------------------------------

SEM_images <- dir("SEM_images", pattern = "*.tif") # get file names
SEM_image_path <- paste("SEM_images", '/', SEM_images, sep="")
SEM_panorama <- list()

print("Stitching SEM images into panorama...")
for(i in 1:length(SEM_images)){
  image <- SEM_image_path[i] %>% image_read() %>%
    image_quantize(colorspace = 'gray') %>%
    image_equalize()
  temp_file <- tempfile()
  image_write(image, path = temp_file, format = 'tiff')
  image <- raster(temp_file)
  image_extent <- extent(matrix(c(xy$x[i], xy$x[i] + 1024, xy$y[i], xy$y[i]+704), nrow = 2, ncol = 2, byrow = T))
  image_raster <- setExtent(raster(nrows = 704, ncols = 1024), image_extent, keepres = F)
  values(image_raster) <- values(image)
  SEM_panorama[[i]] <- image_raster
  #plot(image_raster, asp=1)
}
SEM_panorama_merged <- do.call(merge, SEM_panorama)
print("...complete.")

#flush everything we don't need from memory
remove(list = c("SEM_panorama", "SEM_image_path", "SEM_images", "image", "image_extent", "image_raster"))

#optinal plot to check if SEM image merge looks correct 
#plot(SEM_panorama_merged, col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL))



# stitch xray images ------------------------------------------------------

#stitch xray images into panoramas and store in raster brick 
xray_brick_list <- list()
xray_data <- list()
for(j in 1:length(directories)){
  path = directories[j]
  files <- dir(path, pattern = "*.tif") 
  files <- paste(path, '/', files, sep="")
  files <- files[!str_detect(files, "overview")]
  if(length(files) >0){
    xray_data[[j]] <- path
    print(paste0("Stitching ", xray_data[[j]], " data (element ", j, " of ", length(directories), ")..."))
    xy_id <- which(positions %in% str_extract(files, "-?(?<![Kα1||Kα1_2])\\d+"))
    panorama <- list()
    for(i in 1:length(files)){
      print(paste0("Processing image ", i, " of ", length(files), "..."))
      image <- files[i] %>% image_read() %>% 
        image_quantize(colorspace = 'gray') %>% 
        image_equalize() 
      #print("...equalizing complete.")
      temp_file <- tempfile()
      image_write(image, path = temp_file, format = 'tiff')
      #print("...image write complete.")
      image <- raster(temp_file) %>%
        cut(breaks = c(-Inf, 150, Inf)) - 1
      #print("...rasterization complete.")
      image <- aggregate(image, fact=4)
      #print("...downsampling complete.")
      image_extent <- extent(matrix(c(xy$x[xy_id[i]], xy$x[xy_id[i]] + 1024, xy$y[xy_id[i]], xy$y[xy_id[i]]+704), nrow = 2, ncol = 2, byrow = T))
      image_raster <- setExtent(raster(nrows = 704, ncols = 1024), image_extent, keepres = F)
      values(image_raster) <- values(image)
      #print("...final raster complete.")
      panorama[[xy_id[i]]] <- image_raster
      #print("...appended to panorama queue.")
      #print("...complete.")
    }
    #if(length(compact(panorama))>1){
      empty_xy_id <- which(!positions %in% str_extract(files, "-?(?<![Kα1||Kα1_2])\\d+"))
      if(length(empty_xy_id) > 0){
        for(k in 1:length(empty_xy_id)){
          empty_raster_extent <- extent(matrix(c(xy$x[empty_xy_id[k]], xy$x[empty_xy_id[k]] + 1024, xy$y[empty_xy_id[k]], xy$y[empty_xy_id[k]]+704), nrow = 2, ncol = 2, byrow = T))
          empty_raster <- setExtent(raster(nrows = 704, ncols = 1024), empty_raster_extent, keepres = F)
          values(empty_raster) <- 0
          panorama[[empty_xy_id[k]]] <- empty_raster
          #print("...empty raster complete.")
        }
      }
      panorama_merged <- do.call(merge, panorama)
      #print("...raster stitching complete.")
      xray_brick_list[[j]] <- panorama_merged
      #print("...stitched rasters appended to brick list.")
      #print(paste0("...element ", j, " of ", length(directories), "complete."))
  }
}

print("Stitching complete. Creating x-ray brick...")
xray_brick <- do.call(brick, xray_brick_list)
names(xray_brick) <- unlist(xray_data)
print("...complete.")

#flush everything we don't need from memory
remove(list = c("xray_brick_list", "xray_data", "empty_raster", "empty_raster_extent",
                "i", "j", "k", "path", "positions", "xy_id", "xy", 
                "panorama_merged", "panorama", "empty_xy_id", "files", "directories"))



# plot the data -----------------------------------------------------------

print("Generating plot...")
# Set color palette
#zeroCol <-NA 
element_colors <- distinctColorPalette(k = length(names(xray_brick)))
names(element_colors) <- names(xray_brick)

xray_frame <- as.data.frame(xray_brick, xy=TRUE) %>%
     gather(element, value, Al:Si)

###Testing###
test_frame <- xray_frame %>%
  filter(element == "Al" & value!=0) 

SEM_plot <- rasterVis::gplot(SEM_panorama_merged) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = 'black', high = 'white') +
  ggnewscale::new_scale_fill() +
  geom_raster(test_frame, mapping = aes(x, y, fill = element, alpha = value)) +
  scale_fill_manual(values = element_colors) +
  ggnewscale::new_scale_fill() 
###Testing end###


element_plotter<-function(coord_frame, brick, SEM_image, colors){
  p <-rasterVis::gplot(SEM_image) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient(low = 'black', high = 'white') +
    ggnewscale::new_scale_fill()
  for(i in unlist(names(brick))){ 
    element_coords <- coord_frame %>%
      filter(element == names(brick[[i]]) & value!=0)
    p<-p+geom_raster(element_coords, mapping = aes(x, y, fill = element, alpha = value)) +
      scale_fill_manual(values = colors) +
      ggnewscale::new_scale_fill()
    break
  }
  suppressWarnings(print(p + coord_fixed()))
}

element_plotter(xray_frame, xray_brick, SEM_panorama_merged, element_colors)


# generate PDF ------------------------------------------------------------

pdf(opt$out,
    width = 13.33, 
    height = 7.5)

print(element_plot)

dev.off()

print("...complete.")


# old plotting code ----------------------------------------------------------------

#level plot
# element_theme <- list()
# for (i in 1:length(names(xray_brick))) {
#   id <- which(names(element_colors) %in% names(xray_brick)[i])
#   element_theme[[i]] <- rasterTheme(region = c(zeroCol, element_colors[[id]]))
# }
# 
# SEM_plot <- levelplot(SEM_panorama_merged, par.settings = GrTheme, margin = FALSE)
# xray_plots <- list()
# for(n in 1:length(names(xray_brick))){
#   assign(paste(names(xray_brick)[n], "plot", sep = "_"), 
#          levelplot(xray_brick[[n]], par.settings = element_theme[[n]], margin = FALSE, alpha.regions = 0.35))
#   xray_plots[[n]] <- paste(names(xray_brick)[n], "plot", sep = "_")
# }
# 
# eval(parse(text=paste0(c("SEM_plot", unlist(xray_plots)), collapse = "+")))










# xray_frame <- as.data.frame(xray_brick, xy=TRUE) %>%
#   gather(element, value, Al:Si)
# 
# rasterVis::gplot(SEM_panorama_merged) +
#   geom_raster(aes(fill = value)) +
#   scale_fill_gradient(low = 'black', high = 'white') +
#   ggnewscale::new_scale_fill() +
#   annotate(geom="raster", x=xray_frame$x, y=xray_frame$y, fill = xray_frame$element, alpha=xray_frame$value) +
#   coord_fixed()
# 
# 
# colfunc <- colorRampPalette(c(NA, "red"))
# element_colors <- c(NA, colfunc(9))
# 
# plot(SEM_panorama_merged, col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL)) 
# #plot(xray_brick[[1]],col=alpha(element_colors, 0.4), add = T)
# plot(xray_brick[[1]],col=NA, add = T)
# 
# 
# rasterVis::gplot(xray_brick) + 
#   geom_tile(data = xray_brick[xray_brick >0], aes(fill = value), alpha = 0.5) +
#   scale_fill_gradient(low = NA, high = 'blue') +
#   geom_tile(data = xray_brick[xray_brick== 0], aes(colour = NA),
#             linetype = 0, fill = NA)
# 
# rasterVis::gplot(xray_brick) + 
#   geom_tile(aes(fill = names, alpha = value))
#   
# 
# 
# 
# element_colors <- c("r")
# names(element_colors) <- directories
# 
# rasterVis::gplot(SEM_panorama_merged) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_gradient(low = 'black', high = 'white') +
#   ggnewscale::new_scale_fill() +
#   geom_raster(data=xray_frame, mapping = aes(x, y, fill = element, alpha = value)) +
#   coord_fixed()
# 
# 
# levelplot(xray_brick) # plot the raster
# 
# bf <- writeRaster(b, filename=file.path(tmp, "multi.grd"), bandorder='BIL', overwrite=TRUE)
# 
# 
