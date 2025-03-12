#This script creates 100m x 100m grid with track densities for the complete 
# Oostvaarders plassen
# SCRIPT COMPLETED
#load in the functions
source("C:/Internship/Rscripts/deerFunctions.R")

#load in necessary libraries
library(terra)
library(tidyverse)

#create function to batch process

batchProcesser <- function(file_path){
  
  #copy & store the suffix
  suffix <- str_sub(file_path, -8, -5)
  
  #load in the tile 
  tile_x_x <- rast(file_path)
  
  #create grid based on tile with length of 100
  grid_x_x <- gridMaker(tile_x_x, 100)
  
  #Make the tiles based on the 100m x 100m grid
  sub_tiles <- RasterTiler(tile_x_x, grid_x_x, rm= FALSE)
  
  #Calculate the track area for all the subTiles
  list_TrackArea <- map(sub_tiles, trackAreaCalculator)
  
  #Merge the tiles together to the fragmentation raster
  fragRast_x_x <- list_TrackArea%>%
    sprc() %>%
    merge()
  
  #Write the fragmentation raster
  writeRaster(fragRast_x_x, paste0("output/frag", suffix, ".tif"))
  
  #remove the tempory folder 
  unlink("temp", recursive = TRUE)
  
}

#try with multiple files
trackDirectory <- list.files("data/deerTracks/1_GeoTIFF/geotiff",
                            full.names = TRUE)

#Process on all tiles
testBatch <- map(trackDirectory, batchProcesser, 
                 .progress = TRUE)

