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
trackAreaCalculator <- function(spr, pixelSize = 0.01){
  #Calculate the area of deertracks based on the pixel size(m^2)
  library(terra)
  
  #Make sure grid is a vector
  #sv_grid <- vect(grid)
  
  #Set all the NA's to zero 
  spr[is.na(spr)] <- 0
  

  #Set all the pixels to the pixel size 
  spr[spr[[1]] != 0] <- pixelSize
  
  
  #use terra zonal to sum all the pixels per grid cell
  spr_PathArea <- terra::zonal(spr, as.polygons(ext(spr)),
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


####4) Vector Tiler ####
VectorTiler <- function(rast, grid, dir){
  #This function takes in a raster and retiles it with a grid, and makes spatvectors out of them
  #and loads them into the right folder
  #NOT COMPLETE YET
  library(terra)
  
  #Create directory based on input
  dir.create(dir, recursive = TRUE)
  
  #retile the raster 
  makeTiles(rast, grid, paste(dir,"tile_.tif", sep = "/"))
  
  #List the folder with the tiles
  tile_list <- list.files(dir,
                          full.names = TRUE)
  
  #load them in as spatraster
  sprList_tiles <- map(tile_list, terra::rast)
  
  #Create a function to polygonize them
  polygonize_fun <- function(x){
    vec <- as.polygons(x)
    
    return(vec)
  }
  
  #Polygonize all the spatrasters and make t
  svc_DeerPaths <- map(sprList_tiles, polygonize_fun)
  
  #Give them names 
  names(svc_DeerPaths) <-paste0("tile_", 1:36)

  #remove the folder again
  unlink(paste(dir, "*", sep = "/"))
  
  #Save as geopackages
    # Ensure the output directory exists
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
    
    # Loop through each SpatVector in the list and write to a file
    for (name in names(spat_list)) {
      filepath <- file.path(dir, paste0(name, ".gpkg"))
      writeVector(spat_list[[name]], filepath)
      message("Saved: ", filepath)
    }
  }
  
####4) Raster Tiler ####
RasterTiler <- function(rast, grid, dir = "temp", rm = TRUE){
  #This function takes in a raster and retiles it with a grid
  #Standard directory is temp, select rm = FALSE if you want to keep 
  #the folder
  library(terra)
  
  #Create directory based on input
  dir.create(dir, recursive = TRUE)
  
  #retile the raster 
  makeTiles(rast, grid, paste(dir,"tile_.tif", sep = "/"))
  
  #List the folder with the tiles
  tile_list <- list.files(dir,
                          full.names = TRUE)
  
  #load them in as spatraster
  sprList_tiles <- map(tile_list, terra::rast)
  

  if(rm == TRUE){
    #remove the folder again
    unlink(dir, 
           recursive = TRUE)
    
    #return output
    return(sprList_tiles)
    
  }else 
    
    #return output without removed files
    return(sprList_tiles)
  
}
  
