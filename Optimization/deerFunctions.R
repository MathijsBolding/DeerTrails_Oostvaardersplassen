#This file consist of all the functions that make it past
#the scratch phase.
#import with this command: source("C:/Users/mathi/Internship/Rscripts/deerFunctions.R")

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

####3a) gapRemover####
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

####3b) gapRemover2 ####
gapRemover2 <- function(rast, nPix) {
  # Define the kernel
  kernel <- matrix(1, nPix, nPix)
  
  # Perform a closing operation: dilation followed by erosion
  dilated <- focal(rast, w = kernel, fun = max, 
                   na.policy = "all", 
                   na.rm = TRUE)
  eroded <- focal(dilated, w = kernel, fun = min, 
                  na.policy = "all", 
                  na.rm = TRUE)
  
  return(eroded)
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
            paste(dir,"tile_.gpkg", sep = "/"), na.rm = TRUE)
  
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
  writeRaster(meanPatchRast,
              "output/meanPatchSize2_0.tif")
  
  return(meanPatchRast)
  
}

#### 7) Simple plot function ####
plotFunction <- function(dat, x_var, y_var, scale = "normal"){
  #This function creates a nice x-yplot to explore the data
  
  library(tidyverse)
  #The function without a log scale 
  ggplt <- ggplot(data =  dat, 
                  mapping = aes(dat[,x_var], dat[,y_var]))+
    geom_point()+
    xlab(x_var)+
    ylab(y_var)+
    theme_bw(base_size = 15)+
    theme(text = element_text(face="bold"))
  
  #Make if statement for log
  if(scale == "log"){
    ggplt  +
      scale_x_log10()+
      scale_y_log10()
  }else
    #return the ggplot without the axis in log scale
    ggplt
}

#####8) Sample function #####
sampleFun <- function(dat, n){
  #This function takes a sample of size n and calculates the mean of the sample
  #load in the library
  library(tidyverse)
  
  #take the sample 
  sample <- sample_n(dat, size = n)
  
  #Use summarize to summarize
  sample_summarized <- summarize(sample, 
                                 mean_value = mean(value))%>%
    select(mean_value)
  
  
  #return the summarized
  return(sample_summarized)
}

#####9) TrailFractionCalculator #####

#Create function to calcalate the proportion trails for a folder
TrailFractionCalculator <- function(folder){
  #This function loads in a whole folder of .xyz files and calculates the
  # proportion of trail points in there
  library(tidyverse)
  
  #load in the folder
  list_files <- list.files(folder,
                           pattern = "*xyz", full.names = TRUE)
  head(list_files)
  
  #First store the names of the files
  nameVector <- map(list_files, basename)%>%
    as.data.frame()%>%
    pivot_longer(cols = everything())%>%
    select("value")
  
  #Debug print
  print("nameVector Created")
  
  #Remove the .xyz 
  nameVector <- str_sub(nameVector$value, end = -5)%>%
    as.data.frame()
  
  #Now load in the .xyz data
  list_XYZ<- map(list_files, read.table)
  
  #Create a function to calculate the fraction of paths for each file 
  fractionCalculator <- function(list){
    
    #Select the fourth column
    Trail_NoTrail <- list[4]
    
    #Count the total amount
    count_Total <- count(Trail_NoTrail, name = "count_Total")
    
    #Count the trails
    count_Trail <- Trail_NoTrail %>%
      filter(Trail_NoTrail[1] == 1)%>%
      count(name = "count_Trail")
    
    #Bind the columns of both 
    list_TrailTotal <- bind_cols(count_Total, count_Trail)
    
    #Calculate the fraction 
    list_TrailTotal$fraction_Trail <- list_TrailTotal$count_Trail/list_TrailTotal$count_Total
    
    #Select only the fraction to keep 
    list_Fraction <- list_TrailTotal$fraction_Trail
    
    return(list_Fraction)
  }
  
  #Iterate it over the complete list to calculate it as fraction 
  list_TrailFraction <- map(list_XYZ, fractionCalculator)
  
  #make them a dataframe 
  df_TrailFraction <- as.data.frame(list_TrailFraction)
  
  #Make the format long
  df_TrailFractionLong <- pivot_longer(df_TrailFraction, 
                                       cols_vary = "slowest",
                                       cols = everything())%>%
    select("value")
  
  #Bind with nameVector to get back the original names
  df_TrailFractionLong <- bind_cols(nameVector, df_TrailFractionLong)
  
  #Set names
  names(df_TrailFractionLong) <- c("Plot", "FractionTrails")
  
  return(df_TrailFractionLong)
}

#####10) xyx2Spatraster
xyz2rast <- function(xyz_file,
                     polygonize = FALSE,
                     colNames= c("X1", "X2", "X3")) {
  #This function loads in the extracted trail points and converts
  #them to a spatrasters 
  library(terra)
  library(tidyverse)
  #load in the file list
  ExtTrails <- read_table(xyz_file, col_names = FALSE) %>%
    select(colNames)

    #get the name 
  raster_name <- basename(xyz_file)%>%
    str_sub(1,6)
  

  #Read points 
  pts <- vect(ExtTrails, geom = c("X1", "X2"), crs = "EPSG:28992")%>%
    project("EPSG:28992")
  
  #Load in the points as vector
  ext <- ext(pts)
  
  #Create a raster with the desirered reslution
  r_template <- rast(ext, resolution = 0.1, crs = "EPSG:28992")%>%
    project("EPSG:28992")
  
  #Rasterize the plot
  spr_Ext <- rasterize(pts, r_template, fun = "max", 
                       background = 0)
  #Remove gaps
  spr_ExtGapRemoved <- gapRemover2(spr_Ext, 3)
  
  #Set the name 
  names(spr_ExtGapRemoved) <- raster_name
  
  if(polygonize == FALSE){
    
  return(spr_ExtGapRemoved)
  }else
    #Create a spatvector 
    spr_ExtGapRemoved<- subst(spr_ExtGapRemoved, 0, NA)
    sv_ExtGapRemoved <- as.polygons(spr_ExtGapRemoved,
                                    disolve = TRUE)
  
  return(sv_ExtGapRemoved)
    
}

#####11) Confusions_maker
Confusion_maker <- function(file, sprc, rast = TRUE){
  #This function takes in a file with the validation plots, and a sprc. 
  # Returns FN, FP, TN, TP, F1-score & balanced accuracy
  # If rast = false, function returns a data.frame with the confusionscores
  # If rast = true, function returns a raster
  #of all the trails
  library(terra)
  library(tidyverse)
  
  #Print the file name of the validation plot to see which one breaks
  print(file)
  #Take the plot name out of the file
  plot_name <- basename(file)
  plot_name <- str_sub(plot_name, end = -5)
  original_name <- names(sprc[1])
  
  

  #Load in the .xyz files as table, remove height, and make it a raster
  ValPlot <- read_table(file, col_names = FALSE)%>%
    select(X1, X2, X4)
  
  #Make it a raster
  ValPlot <- ValPlot %>%
    rast(crs = "EPSG:28992")
  
  #Crop the sprc with the extent of the validation plot, merge it to get
  # a spatraster (faster than cropping the spatraster)
  #Give it numbers 0, 2 to later distiguish 
  DeerTrail <- crop(sprc, ext(ValPlot)) %>%
    merge()%>%
    subst(c(NA,0, 1), c(0,0, 2))%>%
    #Resample to the validation plot
    resample(ValPlot, method = "near")
  
  print("merging")
  
  #Sum both rasters
  sumRaster <- DeerTrail + ValPlot

  
  if(rast == FALSE){
    
  #Make it a dataframe
  df_sumRaster <- as.data.frame(sumRaster)
  
  #Rename the column to a logical name
  names(df_sumRaster) <- "ConfusionScore"
  
  #Give them correct labels
  df_Confusion <- df_sumRaster%>%
    mutate(ConfusionScore =  recode(ConfusionScore,'0' = "TN", 
                                    '1' = "FN",
                                    '2' = "FP",
                                    '3' = "TP", 
                                    .default ="Unknown"))%>%
    #filter(ConfusionScore != "Unknown")%>%
    group_by(ConfusionScore)%>%
    summarize(n = n())%>%
    #Pivot wider
    pivot_wider(names_from = "ConfusionScore", values_from = "n")%>%
    mutate(Plot = plot_name)%>%
    #This drops a few pixels that are unidentified
    #Should be made different eventually, this case 
    #amount of pixels is very little 
    select(-any_of("Unknown"))    
  
  # If FN column does not exist, create it and set to 0
   if (!"FN" %in% colnames(df_Confusion)) {
      df_Confusion$FN <- 0
    }
  if (!"TP" %in% colnames(df_Confusion)) {
    df_Confusion$TP <- 0
  }
  
  #The same for the false positives
   if (!"FP" %in% colnames(df_Confusion)) {
    df_Confusion$FP <- 0
  }
  
  #Calculate Recall, Precision, F1_score & Balanced accuracy 
  df_Confusion <- df_Confusion %>%
    mutate(Recall = TP/(TP+FN),
           Precision = TP/(TP+FP),
           Specificity = TN/(TN+FP))%>%
    mutate(F1_score = 2 * (Precision *Recall/ (Precision + Recall)))%>%
    mutate(Bal_Accuracy = (Recall +Specificity)/2)%>%
    mutate(Accuracy = (TP +TN)/(TP+TN+FP+FN))
  
  return(df_Confusion)
  }else{
    
    #Set the name to the original raster name
    #The same for the spatraster 
    original_name <- names(sprc[1])
    names(sumRaster) <- original_name
    
    
    return(sumRaster)
    
  }
}

#####12) Validation DatasetCreator#####
#Create a function to run a whole folder at a time  
datasetCreator <- function(extTrailsDeer, extTrailsDeerGeese, 
                           ValPlotsDeer, ValPlotsDeerGeese, Par){
  library(terra)
  library(tidyverse)
  
  print(extTrailsDeer)
  print(extTrailsDeerGeese)
  #Function to automatically run a whole folder of different parameter settings
  #First list the files with the extracted trails and get the label of parameters
  list_extTrailsDeer <- list.files(extTrailsDeer,
                                   full.names = TRUE)
  
  #Copy the par settings (same for both datasets)
  #parSettings <- str_sub(extTrailsDeer, -11,-6)
  parSettings <- ""
  list_extTrailsDeerGeese <- list.files(extTrailsDeerGeese,
                                        full.names = TRUE)

  #List the files of the validation plots
  list_ValPlotsDeer <- list.files(ValPlotsDeer,
                                  full.names = TRUE)
  list_ValPlotDeerGeese <- list.files(ValPlotsDeerGeese,
                                      full.names = TRUE)
  
  #Load in the extracted trails and fill the gaps in between
  sprc_extTrailsDeer <- map(list_extTrailsDeer, xyz2rast)%>%
    sprc()
  
  sprc_extTrailsDeerGeese <- map(list_extTrailsDeerGeese,
                                 xyz2rast)%>%
    sprc()
  
  #Calculate the confusion matrix, turn in dataset and name the group 
  df_ConfDeer <- map(list_ValPlotsDeer, ~Confusion_maker(.x, 
                                                         sprc = sprc_extTrailsDeer,
                                                         FALSE))%>%
    reduce(bind_rows) %>%
    mutate(Group = "DeerOnly")
  

    df_ConfDeerGeese <- map(list_ValPlotDeerGeese, 
                            ~Confusion_maker(.x, 
                                             sprc = sprc_extTrailsDeerGeese,
                                             FALSE))%>%
    reduce(bind_rows)%>%
    mutate(Group = "DeerGeese")

  #Bind rows and name the dataset and paramater setting
  df_Confusion <- bind_rows(df_ConfDeerGeese, 
                            df_ConfDeer)%>%
    mutate(dataset = "AHN5") %>%
    mutate(Par = Par)%>%
    mutate(Parset = parSettings)
  
  #return everything
  return(df_Confusion)
}

#####13) Geosaver #####
GeoConverter <- function(fun, dir, ..., save_dir = NULL,
                     split_geometries = FALSE){
  #Function to save the results of a convertion function as a .tif of geopackage. 
  #St
  library(terra)
  library(tidyverse)
  library(sf)
  
  #Extract the basename of the file and remove extention 
  basename <- basename(dir)%>%
    tools::file_path_sans_ext()
  
  # Run the function with do.call
  result <- do.call(fun, list(dir, ...))
  
  # Determine output path (use provided save_dir or default to same folder)
  if (is.null(save_dir)) {
    save_dir <- dirname(dir)
  }
  
  #Set the output file
  out_file <- file.path(save_dir, 
                        paste0(basename,
                               if (inherits(result,
                                            "SpatRaster")) ".tif" else ".gpkg"))
  # Save based on class
  if (inherits(result, "SpatRaster")) {
    writeRaster(result, out_file, overwrite = TRUE)
    
  } else if (inherits(result, "SpatVector")) {
    #check whether split geometries is true
    if(split_geometries == TRUE){
      #Convert to sf
      sf_result <- st_as_sf(result)%>%
        st_cast("POLYGON")
      
      #From multipolygon to polygon
      list_polygons <- split(sf_result, 
                             seq(nrow(sf_result)))
      
      #Save all the geometries seperately
      walk2(.x = list_polygons, 
            .y = seq_along(list_polygons),
            .f = ~ st_write(.x, file.path(save_dir, paste0(basename, 
                                                           .y, ".gpkg"))))
    }else
      
    writeVector(result, out_file, overwrite = TRUE, filetype = "GPKG")

    
      } else {
    warning("Result is not a SpatRaster or SpatVector")
  }
  
  return(result)
  
}

######13) Confusion_centerline #####
ConfusionCenterline <- function(extr_cen, extr_pol,
                                 val_plot, val_cen){
  #The confusion maker if the centerline approach is used
  #As entry everything needs to be a spatvector collection except the 
  #Validation plot as polygon
  library(terra)
  library(tidyverse)
  
  # Load in the validation plot as spatvector 
  sv_ValPlot <- vect(val_plot, crs = "EPSG:28992")

  #Get the plot number
  Plot <- str_extract(val_plot, "[0-9]+_Plot")
  
  #Get DeerOnly/DeerGeese from folder name
  Group <- basename(dirname(dirname(val_plot)))
  
  #Set directory for the TP, FP, FN geopackages
  dir_TP <- file.path("/media/mathijs/Shared/UvA_baan/Workflow/TheCleanRoom/Output/TP",
                    paste0(Plot,"_", Group,".gpkg"))
  
  dir_FN <- file.path("/media/mathijs/Shared/UvA_baan/Workflow/TheCleanRoom/Output/FN",
                      paste0(Plot,"_", Group,".gpkg"))
  
  dir_FP <- file.path("/media/mathijs/Shared/UvA_baan/Workflow/TheCleanRoom/Output/FP",
                      paste0(Plot,"_", Group,".gpkg"))

  # Get bounding box of the validation plot
  bb_val <- ext(sv_ValPlot)  
  
  print("succes")
  
  #Create a function to filter the spatvectorcollection by extent
  FilterFun <- function(vec){
    #Set the correct crs 
    crs(vec) <- "EPSG:28992"
    
    #Get the bounding box of the vector 
    bb_extr <- ext(vec)

    #Check if bounding boxes intersect
    intersects <- !is.na(terra::intersect(bb_extr, bb_val))
    
    #Filter based on bounding boxes
    vec[intersects, ]
    
  }

  #Map over the filter function to return the matching spatvectors 
  sv_ExtrCen <- map(extr_cen, FilterFun)%>%
     discard(~ nrow(.x) == 0)
  
  sv_ExtrPol <- map(extr_pol, FilterFun) %>%
   discard(~ nrow(.x) == 0)

  sv_ValCen <- map(val_cen, FilterFun) %>%
     discard(~ nrow(.x) == 0)
  
  print("it works very nicely")
  
  #Return the spatvector from the list
  sv_ExtrCen <- sv_ExtrCen[[1]]%>%
    terra::crop(bb_val)
  sv_ExtrPol <- sv_ExtrPol[[1]]%>%
    terra::crop(bb_val)
  sv_ValCen <- sv_ValCen[[1]]%>%
    terra::crop(bb_val)
  

  print("sv returned")
  #Get the true positives
  TP <- terra::intersect(sv_ExtrCen, sv_ValPlot)%>%
    #writeVector(dir_TP, overwrite = TRUE) #%>%
    perim()%>%
    sum()
  #Get the false positives
  FP <- terra::erase(sv_ExtrCen, sv_ValPlot)%>%
    #writeVector(dir_FP, overwrite = TRUE)#%>%
    perim()%>%
    sum()
  
  #Get the false negatives
  FN <- terra::erase(sv_ValCen, sv_ExtrPol)%>%
    #writeVector(dir_FN, overwrite = TRUE)%>%
    perim()%>%
    sum()
  
  #Combine the values together in rows
  df_Confusion <- tibble(Plot, TP, FP, FN)
  
  
  return(df_Confusion)
  
}

######14) rast2Polygon #####
rast2polygon <- function(rast, colNames){
  # #Function to polygonize xyz file with the scalar field (drops the z), 
  # differs from xyz2rast(Polygonize=TRUE) by not needing to fit the xyz points
  # into a raster (Later going to be merged)
    library(terra)
    library(tidyverse)
    
    print(rast)
  
    #Read in the xyz file
    xyz_table <- read_table(rast, col_names = FALSE)
    
    
    #Take the name of the file 
    #Plot_number <- str_extract(xyz, "Plot_[0-9]+")
    
    #Paste it towards the directory
    #directory <- paste0(dir, Plot_number, ".gpkg")
    
    #Drop the z column (height)
    polygon <- xyz_table %>%
      select(colNames)%>%
      rast() %>%
      gapRemover2(nPix = 3) %>%
      subst(0, NA)%>%
      as.polygons(dissolve = TRUE)
    
    #Save the resulting spatvector if save is true
    
    
    #Return the polygon if save is of
    
    return(polygon)
    
 }

######15) virtualRasterMerger #####
virtualRasterMerger <- function(folder, polygonize = TRUE){
  library(terra)
  
  #Exract the basename
  dir_name <- dirname(folder)
  
  #Create the virtual raster name
  vrt_name <- file.path(dir_name, paste0("VirtualRaster", ".vrt"))
  
  #List the files of the input folder
  file_list <- list.files(folder,
                          full.names = TRUE)
  
  #Create the virtual raster
  vrt(file_list, vrt_name, overwrite = TRUE)
  
  if(polygonize == TRUE){
    #Create a polygon
    sf_Trails <- rast(vrt_name)%>%
      subst(0, NA)%>%
      as.polygons(dissolve = TRUE)
    
    return(sf_Trails)
  }else{
    spr_Trails <- rast(vrt_name)
    
    writeRaster(spr_Trails, paste0(vrt_name, ".tif"))
  }
  
}