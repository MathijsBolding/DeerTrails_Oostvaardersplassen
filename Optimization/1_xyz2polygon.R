#Script to turn the xyz files from the workflow into polygons ready for 
# taking the centerline. 

#Load in the necessary libraries
library(tidyverse)
library(purrr)
library(terra)

#Load in the deer functions
source("deerFunctions.R")

args <- commandArgs(trailingOnly = TRUE)

#Set the location of the extracted trails 
files_DeerGeese <- list.files(args[1],
                              full.names = TRUE)

output_DeerGeese <- args[2]


#Convert the xyz files to gpkg's 
#For the deergeese
walk(files_DeerGeese, ~GeoConverter(fun = xyz2rast, 
                               dir = .x, 
                               polygonize = TRUE,
                               save_dir = output_DeerGeese))

