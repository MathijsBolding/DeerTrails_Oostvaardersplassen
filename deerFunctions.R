#This file consist of all the functions that make it past
#the scratch phase.
#import with this command: source("C:/Internship/Rscripts/deerFunctions.R")
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
  
  return(spr_grid)
}

####2) trackAreaCalculator #####
trackAreaCalculator <- function(spr, pixelSize = 0.01){
  #Calculate the area of deertracks based on the pixel size(m^2)
  library(terra)
  
  #Set all the NA's to zero 
  spr[is.na(spr)] <- 0
  
  
  #Set all the pixels to the pixel size 
  spr[spr[[1]] != 0] <- pixelSize
  
  
  #use terra zonal to sum all the pixels per grid cell
  spr_PathArea <- terra::aggregate(spr, fact = 500, fun = "sum",
                                   cores = 12)
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
RasterTiler <- function(rast, grid, dir = "temp", rm = FALSE){
  #This function takes in a raster and retiles it with a grid
  #Standard directory is temp, select rm = FALSE if you want to keep 
  #the folder
  library(terra)
  
  #Create directory based on input
  dir.create(dir, recursive = TRUE)
  
  #retile the raster 
  makeTiles(rast, grid, 
            paste(dir,"tile_.tif", sep = "/"), na.rm = TRUE)
  
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

#### 5) Batchprocessor function#####
batchProcesser_v2 <- function(tile, outputName = "PathSizeRast.tif"){
  #This function runs the functions: gridMaker, RasterTiler and trackAreaCalculator.  
  #Then it combines all the spatrasters into one big .tif
  
  #create grid based on tile with length of 100m
  grid_x_x <- gridMaker(tile, 100)
  
  #Make the tiles based on the 100m x 100m grid
  sub_tiles <- RasterTiler(tile, grid_x_x, rm= FALSE)
  
  #Calculate the track area for all the subTiles
  list_TrackArea <- map(sub_tiles, trackAreaCalculator,
                        .progress = TRUE)
  
  #Merge the tiles together to the fragmentation raster
  fragRast_x_x <- list_TrackArea%>%
    sprc() %>%
    merge()
  
  #Write the fragmentation raster
  writeRaster(fragRast_x_x, outputName)
  
  #remove the tempory folder 
  #unlink("temp", recursive = TRUE)
  
}

####6) PatchBatchCalculator#####
PatchBatchCalculator <- function(tile, outputName = "MeanPatchSize.tif"){
  #This function takes in a tile with deer paths and calculates
  #the mean patch area for the tile of 100m x 100m
  
  #necessary libraries
  library(terra)
  library(landscapemetrics)
  
  #Make a grid using the gridmaker
  grid_x_x <- gridMaker(tile, 100)
  
  #Make the tiles based on the 100m x 100m grid
  sub_tiles <- RasterTiler(tile, grid_x_x, rm= FALSE)
  
  #Create the function to calculate the mean path csize
  meanPatchCalculator <- function(x, y){
    
    #Set all the NA value's to zero (patches)
    x[is.na(x)] <- 0
    
    #Set all the path area to NA
    x[x[[1]] == 1] <- NA
    
    #Get the extent of the raster
    extRast <- ext(x)
    
    #calculate area
    meanArea <- lsm_l_area_mn(x)
    
    #create a rastercell with the value of the mean area
    CellMean <- rast(vals = meanArea$value, ext = extRast, 
                     ncols = 1, nrows = 1)
    
    return(CellMean)
  }
  
  #Map it to calculate it for each cell 
  meanPatchCellList <- map(sub_tiles, meanPatchCalculator) 
  
  #Merge the list to form one raster
  meanPatchRast <- sprc(meanPatchCellList)%>%
    merge()
  
  #Write the output just in case
  writeRaster(meanPatchRast, outputName)
  
  return(meanPatchRast)
  
}

