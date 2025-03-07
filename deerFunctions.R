#This file consist of all the functions that make it past
#the scratch phase.
#import with this command: source("C:/Internship/Rscripts/testScriptFragmentation.R")
####1) gridMaker #####
gridMaker <- function(x, cellLength){
  #This function takes in a raster/vec and makes a grid based on the extent
  #gridSize is in meters
  #source:  https://gis.stackexchange.com/questions/431873/creating-regular-sampling-grid-with-specific-distance-between-points-using-r
  library(terra)
  
  #take extent
  ext_x <- ext(x)
  #Make raster based on extent and resolution
  spr_grid <- rast(ext_x,
                   resolution = c(cellLength, cellLength))
  
  #Set unique values
  values(spr_grid) = 1:ncell(spr_grid)
  
  #make it polygons
  sv_grid <- as.polygons(spr_grid)
  
  return(sv_grid)
}

####2) trackAreaCalculator #####
trackAreaCalculator <- function(spr, pixelSize, grid){
  #Calculate the area of deertracks based on the pixel size(m^2)
  library(terra)
  
  #Set all the NA's to zero 
  spr[is.na(spr)] <- 0
  
  #Set all the pixels to the pixel size 
  spr[spr_DeerTracks != 0] <- pixelSize
  
  #use terra zonal to sum all the pixels per grid cell
  spr_PathArea <- terra::zonal(spr, grid,
                               fun = "sum", as.raster = TRUE)
  
  return(spr_PathArea)
  
}

####3) gapRemover####
gapRemover <- function(rast, nPix){
  #Removes small gaps in the deer tracks and connect tracks that are laying close
  #to eachother. nPix is the number of pixels  
  library(terra)
  
  #Set kernel size 
  kernel <- matrix(1, nPix, nPix)  # in decimeter (pixel size 10cm x 10cm)
  
  #Use focal to connect the part together
  filled_tracks <- focal(rast, w = kernel, 
                         fun = max, na.policy = "all",
                         na.rm= TRUE)
  
  return(filled_tracks)
}


