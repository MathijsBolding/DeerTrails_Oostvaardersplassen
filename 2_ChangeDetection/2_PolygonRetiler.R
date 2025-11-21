####first scratch to retile the large trail polygon to smaller ones. 
library(terra)
library(tidyverse)



TrailPolygons <- vect("Source/TrailsInternship/TestTrails/CenterlineTester/CenterlineTesterPolygons.gpkg")

#Create a grid
grid <- gridMaker(TrailPolygons, 100)

sub_tiles <- RasterTiler(grid, grid, 
                         dir = "temp", 
                         rm = FALSE)

#Create Polygons from the sub_tiles
sub_polygons <- map(sub_tiles, 
                    ~as.polygons(.x))

#Create a buffer around it 
buffer_polygons <- map(sub_polygons, 
                       ~buffer(.x, 
                               5,
                               joinstyle = "mitre"))

#Crop the tiles with the buffered polygons
clippedTrails <- map(buffer_polygons, 
                     ~crop(TrailPolygons,
                           .x))

list_names <- paste0("Source/TrailsInternship/TestTrails/CenterlineTester/TestTrailRetiledBuffer/polygons",
                     "/tile_",
                     1:36,
                     ".gpkg")

file.exists(list_names[[1]])
#create vector
walk2(clippedTrails, list_names, ~ 
     writeVector(.x, 
                 .y))


