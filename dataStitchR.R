#!/usr/bin/env Rscript

#foreword 
print("This script was created by Caitlin Casar. DataStitchR stitches panoramic images of SEM images coupled to 
      x-ray energy dispersive spectroscopy.")

#load dependencies 
print("Loading script dependencies: optparse, raster, rdgal, magick, tidyverse, rasterVis, ggnewscale...")
pacman::p_load(optparse, raster, rdgal, magick, tidyverse, rasterVis, ggnewscale)
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
directories <- directories[!str_detect(directories, "Unknown|Os|SEM|.tif")]
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

#write the brick 
print("Writing brick...")
sample_id <- str_extract(getwd,"([^/]+$)")

x <- writeRaster(xray_brick, paste0(sample_id,'.tif'), overwrite=TRUE, format="GTiff",options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

print("...complete.")

#flush everything we don't need from memory
remove(list = c("x", "xray_brick_list", "xray_data", "empty_raster", "empty_raster_extent",
                "i", "j", "k", "path", "positions", "xy_id", "xy", 
                "panorama_merged", "panorama", "empty_xy_id", "files", "directories"))



# plot the data -----------------------------------------------------------

print("Generating plot...")
# Set color palette


element_colors <- c("#FFFFFF", "#D9FFFF", "#CC80FF", "#C2FF00", "#FFB5B5", "#909090", "#3050F8",
                    "#FF0D0D", "#90E050", "#B3E3F5", "#AB5CF2", "#8AFF00", "#BFA6A6", "#F0C8A0",
                    "#FF8000", "#FFFF30", "#1FF01F", "#80D1E3", "#8F40D4", "#3DFF00", "#E6E6E6",
                    "#BFC2C7", "#A6A6AB", "#8A99C7", "#9C7AC7", "#E06633", "#F090A0", "#50D050",
                    "#C88033", "#7D80B0", "#C28F8F", "#668F8F", "#BD80E3", "#FFA100", "#A62929",
                    "#5CB8D1", "#702EB0", "#00FF00", "#94FFFF", "#94E0E0", "#73C2C9", "#54B5B5",
                    "#3B9E9E", "#248F8F", "#0A7D8C", "#006985", "#C0C0C0", "#FFD98F", "#A67573",
                    "#668080", "#9E63B5", "#D47A00", "#940094", "#429EB0", "#57178F", "#00C900",
                    "#70D4FF", "#FFFFC7", "#D9FFC7", "#C7FFC7", "#A3FFC7", "#8FFFC7", "#61FFC7",
                    "#45FFC7", "#30FFC7", "#1FFFC7", "#00FF9C", "#00E675", "#00D452", "#00BF38",
                    "#00AB24", "#4DC2FF", "#4DA6FF", "#2194D6", "#267DAB", "#266696", "#175487",
                    "#D0D0E0", "#FFD123", "#B8B8D0", "#A6544D", "#575961", "#9E4FB5", "#AB5C00",
                    "#754F45", "#428296", "#420066", "#007D00", "#70ABFA", "#00BAFF", "#00A1FF",
                    "#008FFF", "#0080FF", "#006BFF", "#545CF2", "#785CE3", "#8A4FE3", "#A136D4",
                    "#B31FD4", "#B31FBA", "#B30DA6", "#BD0D87", "#C70066", "#CC0059", "#D1004F",
                    "#D90045", "#E00038", "#E6002E", "#EB0026")
names(element_colors) <- c("H",  "He", "Li", "Be", "B",  "C",  "N",  "O",  "F",  "Ne", "Na", "Mg", "Al", "Si",
                           "P",  "S",  "Cl", "Ar", "K",  "Ca", "Sc", "Ti", "V",  "Cr", "Mn", "Fe", "Co", "Ni",
                           "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y",  "Zr", "Nb", "Mo",
                           "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I",  "Xe", "Cs", "Ba",
                           "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb",
                           "Lu", "Hf", "Ta", "W",  "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po",
                           "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa", "U",  "Np", "Pu", "Am", "Cm", "Bk", "Cf",
                           "Es", "Fm", "Md", "No", "Lr", "Rf", "Db", "Sg", "Bh", "Hs", "Mt")

xray_frame <- as.data.frame(xray_brick, xy=TRUE) %>%
     gather(element, value, Al:Si)

###Testing###
# test_frame <- xray_frame %>%
#   filter(element == "Al" & value!=0) 
# 
# SEM_plot <- rasterVis::gplot(SEM_panorama_merged) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_gradient(low = 'black', high = 'white') +
#   ggnewscale::new_scale_fill() +
#   geom_raster(test_frame, mapping = aes(x, y, fill = element, alpha = value)) +
#   scale_fill_manual(values = element_colors) +
#   ggnewscale::new_scale_fill() 
###Testing end###


element_plotter<-function(coord_frame, brick, SEM_image, colors){
  p <-rasterVis::gplot(SEM_image) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient(low = 'black', high = 'white') +
    ggnewscale::new_scale_fill()
  for(i in names(brick)){ 
    print(paste0("Adding ", names(brick[[i]]), " to plot..."))
    element_coords <- coord_frame %>%
      filter(element == names(brick[[i]]) & value!=0)
    p <- p+geom_raster(element_coords, mapping = aes(x, y, fill = element, alpha = value)) +
      scale_fill_manual(values = colors) +
      ggnewscale::new_scale_fill()
  }
  print("Writing plot...")
  suppressWarnings(print(p + 
                           coord_fixed() +
                           guides(col = guide_legend(ncol = 3)) +
                           theme(axis.title = element_blank(),
                                 axis.text = element_blank(),
                                 legend.position = "bottom",
                                 legend.title = ggplot2::element_blank(), 
                                 legend.text = ggplot2::element_text(size = 8),
                                 legend.key.size = unit(0.25, "cm"))))
}

element_plot <- element_plotter(xray_frame, xray_brick, SEM_panorama_merged, element_colors)
element_plot

# generate PDF ------------------------------------------------------------

pdf("element_plot.pdf",
    width = 13.33, 
    height = 7.5)

print(element_plot)

dev.off()

print("...complete.")


# old plotting code ----------------------------------------------------------------

#level plot
#zeroCol <-NA 
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