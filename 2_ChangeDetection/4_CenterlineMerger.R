library(terra)
library(tidyverse)
library(sf)

source("D:/UvA_baan/Workflow/TheCleanRoom/GithubRepository/DeerTrails_Oostvaardersplassen/Optimization/deerFunctions.R")

#List the centerlines
files_centerlines <- list.files("Source/TrailsInternship/TestTrails/CenterlineTester/TestTrailRetiledBuffer/centerlines",
                                full.names = TRUE)

#Read test tiles 
list_centerlines <- map(files_centerlines, 
                        st_read)

list_borders <- map(list_centerlines,
                    ~as.polygons(ext(.x)))

#Make it a sf
list_borders_sf <- map(list_borders, 
                       st_as_sf)
#Make it linestring
list_borders_line <- map(list_borders_sf,~
                         st_cast(.x, "LINESTRING")) 

list_borders_line <- map(list_borders_line, 
                         ~st_set_crs(.x, 28992))

#Get the intersection points 
list_IntersectionPoints <- map2(list_centerlines,
                                list_borders_line,
                                st_intersection)

#Get the intersection points
IntersectionPoints <- bind_rows(list_IntersectionPoints)

#Snap them together
Points_snapped <- st_snap(IntersectionPoints, IntersectionPoints,
                          tolerance = 1)

#Snap all the lines to the points
centerlineSnapped <- map(list_centerlines, 
                         ~st_snap(.x, 
                                  Points_snapped,
                                  tolerance = 1))

#Merge the centerlines together
centerlinesMerged <- bind_rows(list_centerlines)

#Snap the lines
centerlinesSnapped <- st_snap(centerlinesMerged, 
                              centerlinesMerged, 
                              tolerance = 1)

plot(centerlinesMerged)
#Write it to a file
st_write(centerlinesSnapped,
            "Source/TrailsInternship/TestTrails/CenterlineTester/TestTrailRetiled/SnappedTestTrail_5.gpkg")

plot(centerlineSnapped[[1]])
plot(list_centerlines[[1]])
?s

plot(Points_snapped)
plot(IntersectionPoints)

IntersectionPoints
#merge the testTrails

st_border_1 <- as.polygons(sub_tiles[[3]])%>%
  set.crs(crs(testTrails))%>%
  st_as_sf()%>%
  st_cast("LINESTRING")

#2
st_border_2 <- as.polygons(sub_tiles[[2]])%>%
  set.crs(crs(testTrails))%>%
  st_as_sf()%>%
  st_cast("LINESTRING")


#
CroppedTrails_1 <- st_crop(testTrails, st_border_1)
CroppedTrails_2 <- st_crop(testTrails, st_border_2)


borderPoints_1 <- st_intersection(CroppedTrails_1, st_border_1)
borderPoints_2 <- st_intersection(CroppedTrails_2, st_border_2)

#bind the points 
combinedPoints <- rbind(borderPoints_1,
                    borderPoints_2)

#Snap the point to eachother:
snapped <- st_snap(borderPoints_1, borderPoints_2, 
                   tolerance = 1)

#Snap the lines to the snapped points
snappedLines_1 <- st_snap(CroppedTrails_1,
                        snapped,
                        tolerance = 0.5)

snappedLines_2 <- st_snap(CroppedTrails_2,
                          snapped, tolerance = 0.5)

#Combine the snapped lines
combinedLines <- rbind(snappedLines_1, snappedLines_2)
croppedTrails <- rbind(CroppedTrails_1,
                       CroppedTrails_2)

plot(croppedTrails)

?st_snap
plot(snappedLines_2)
plot(CroppedTrails_2)

borderPoints[1]
IntersectTrails <- st_snap(CroppedTrails,
                                 polygonCell,
                           tolerance = 0.05)
CroppedTrails

?st_snap
IntersectTrails
plot(CroppedTrails)

sf_TestNetwork <- read_sf("Source/TrailsInternship/TestTrails/CenterlineTester/CenterlineResults/Centerline_Densify.gpkg") %>%
  st_cast("LINESTRING")














