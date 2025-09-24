#This step uses ConfusionCenterline function to calculate the performance
#scores of the workflow after centerlining
library(tidyverse)
library(terra)

#Load in the deerFunctions
source("D:/UvA_baan/Workflow/TheCleanRoom/Scripts/Optimization/deerFunctions.R")

#Take the input arguments 
args <- commandArgs(trailingOnly = TRUE)

ahn5_cen <- "D:/UvA_baan/Workflow/TheCleanRoom/Data/Optimization/AHN5/Ratio/rat0.4/cen"

ahn5_pol <- "D:/UvA_baan/Workflow/TheCleanRoom/Data/Optimization/AHN5/Ratio/rat0.4/pol"


#Extract the parameter settings from the folder name: 
ParSetting <- basename(dirname(args[1]))

#Extract the parameter from the folder path
Parameter <- basename(dirname(dirname(args[1])))

#Extract the dataset
Dataset <- basename(dirname(dirname(dirname(args[1]))))

##### Calculate the confusion matrix for the deergeese files.  #####
#Load in the the centerlines and the polygons for the extracted and validated
Centerlines <- list.files(ahn5_cen,
                          full.names = TRUE)%>%
  map(vect)%>%
  svc()


Polygons <- list.files(ahn5_pol,
                       full.names = TRUE)  %>%
  map(vect)%>%
  svc()

DeerGeese_ValCenterlines <- list.files("D:/UvA_baan/Workflow/TheCleanRoom/Data/ValidationPlots/AHN5/DeerGeese/cen",
                                       full.names = TRUE)%>%
  map(vect)%>%
  svc()

DeerGeese_ValPolygons <- list.files("D:/UvA_baan/Workflow/TheCleanRoom/Data/ValidationPlots/AHN5/DeerGeese/pol",
                                    full.names = TRUE)


#Use the confusion centerline function to calculate the performance metrics
DeerGeese_confusion <- map(DeerGeese_ValPolygons,
                           ~ConfusionCenterline(extr_cen = Centerlines,
                                                extr_pol = Polygons,
                                                val_cen = DeerGeese_ValCenterlines,
                                                val_plot = .x))%>%
  #Bind rows to merge the tibbles together
  bind_rows() %>%
  #Calculate the performance metrics
  mutate(Recall = TP/(TP+FN),
         Precision = TP/(TP+FP),
         F1_score = 2 * (Recall*Precision)/(Recall+Precision))
print("DeerGeese Created")


plot(Centerlines[1])
plot(sv_ValPlot[1])


DeerGeese_confusion$Group <- "DeerGeese"


##### Calculate the confusion matrix for the DeerOnly files.  #####
DeerOnly_ValCenterlines <- list.files("/media/mathijs/Shared/UvA_baan/Workflow/TheCleanRoom/Data/ValidationPlots/AHN5/DeerOnly/cen",
                                      full.names = TRUE)%>%
  map(vect)%>%
  svc()

DeerOnly_ValPolygons <- list.files("/media/mathijs/Shared/UvA_baan/Workflow/TheCleanRoom/Data/ValidationPlots/AHN5/DeerOnly/pol",
                                   full.names = TRUE)


#Calculate the confusion matrix for the DeerOnly group
DeerOnly_confusion <- map(DeerOnly_ValPolygons,
                          ~ConfusionCenterline(extr_cen = Centerlines,
                                               extr_pol = Polygons,
                                               val_cen = DeerOnly_ValCenterlines,
                                               val_plot = .x))%>%
  #Bind rows to merge the tibbles together
  bind_rows() %>%
  #Calculate the performance metrics
  mutate(Recall = TP/(TP+FN),
         Precision = TP/(TP+FP),
         F1_score = 2 * (Recall*Precision)/(Recall+Precision))

print("DeerOnly Created")
#Name the DeerOnly group
DeerOnly_confusion$Group <- "DeerOnly"
#
# #Bind groups together
df_Confusion <- bind_rows(DeerOnly_confusion,
                          DeerGeese_confusion)


print("df_Confusion Created")

#Store all other useful information
#Create a column to and set the name
df_Confusion$ParSetting <- ParSetting
df_Confusion$Parameter <- Parameter
df_Confusion$Dataset <- Dataset

#Take in the argument with the location of the .csv
path_Confusion <- file.path(args[3])


#Check if the file exists, if yes append otherwise create
if(file.exists(path_Confusion)){
  write_csv(df_Confusion, path_Confusion,
            append  = TRUE)
  
}else write_csv(df_Confusion, path_Confusion,
                col_names = TRUE)


