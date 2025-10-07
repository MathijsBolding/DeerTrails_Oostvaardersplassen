#Script to turn the xyz files from the workflow into polygons ready for 
# taking the centerline. 

#Load in the necessary libraries
library(tidyverse)
library(terra)
#Load in the deer functions
source("TheCleanRoom/GithubRepository/DeerTrails_Oostvaardersplassen/Optimization/deerFunctions.R")


#Set the location of the extracted trails 
xyz_trails <- list.files("D:/UvA_baan/Workflow/TheCleanRoom/Data/Optimization/AHN4/OriginalTrails/extr",
                   full.names = TRUE)

#Set the output folder
tif_trails <- "TheCleanRoom/Data/Optimization/AHN4/OriginalTrails/extr_tif"

#Convert the xyz trails to tif trails
walk(xyz_trails, ~GeoConverter(fun = xyz2rast, 
                                dir = .x, 
                                polygonize = FALSE,
                                save_dir = tif_trails))


#Convert the tif trails to polygons (split each polygon to seperate file)
GeoConverter(fun = virtualRasterMerger,
             dir = tif_trails,
             polygonize = TRUE,
             split_geometries = TRUE, 
             save_dir = "temp")

