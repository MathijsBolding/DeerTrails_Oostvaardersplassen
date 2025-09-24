#This step uses ConfusionCenterline function to calculate the performance
#scores of the workflow after centerlining
library(tidyverse)
library(terra)

#Load in the deerFunctions
source("deerFunctions.R")

#Take the input arguments 
args <- commandArgs(trailingOnly = TRUE)

#ahn5_cen <- "/media/mathijs/Shared/UvA_baan/Workflow/TheCleanRoom/Data/Optimization/AHN5/Ratio/rat0.4/cen"

#ahn5_pol <- "/media/mathijs/Shared/UvA_baan/Workflow/TheCleanRoom/Data/Optimization/AHN5/Ratio/rat0.4/pol"


#Extract the parameter settings from the folder name: 
ParSetting <- basename(dirname(args[1]))

#Extract the parameter from the folder path
Parameter <- basename(dirname(dirname(args[1])))

#Extract the dataset
Dataset <- basename(dirname(dirname(dirname(args[1]))))

##### Calculate the confusion matrix for the deergeese files.  #####
#Load in the the centerlines and the polygons for the extracted and validated
Centerlines <- list.files(args[1],
                          full.names = TRUE)%>%
  map(vect)%>%
  svc()

Polygons <- list.files(args[2],
                       full.names = TRUE)  %>%
  map(vect)%>%
  svc()

DeerGeese_ValCenterlines <- list.files("/media/mathijs/Shared/UvA_baan/Workflow/TheCleanRoom/Data/ValidationPlots/AHN5/DeerGeese/cen",
                                       full.names = TRUE)%>%
  map(vect)%>%
  svc()

DeerGeese_ValPolygons <- list.files("/media/mathijs/Shared/UvA_baan/Workflow/TheCleanRoom/Data/ValidationPlots/AHN5/DeerGeese/pol",
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


#Bind the groups together for the dataset with the results of all the plots
df_Confusion <- bind_rows(DeerOnly_confusion,
                          DeerGeese_confusion)%>%
  mutate(ParSetting = ParSetting,
         Parameter = Parameter,
         Dataset = Dataset)

print("df_Confusion Created")

#Create the dataset with only the summed confusion matrix 
df_Summary <- bind_rows(DeerOnly_confusion,
                       DeerGeese_confusion) %>%
  select(TP, FP, FN)%>%
  summarise(TP = sum(TP),
            FP = sum(FP),
            FN = sum(FN))%>%
  mutate(Recall = TP/(TP+FN),
         Precision = TP/(TP+FP),
         F1_score = 2 * (Recall*Precision)/(Recall+Precision),
         Plot = "All_Plots",
         ParSetting = ParSetting,
         Parameter = Parameter,
         Dataset = Dataset,
         Group = "Both")

#Take in the argument with the location of the .csv
path_Confusion <- file.path(args[3])

#Create the file path names
file_ConfusionMatrix <- file.path(path_Confusion, "ConfusionMatrix.csv")
file_ConfusionSummary <- file.path(path_Confusion, "ConfusionSummary.csv")


#Check if the file exists, if yes append otherwise create
if(dir.exists(path_Confusion)){
  write_csv(df_Confusion, file_ConfusionMatrix,
            append  = TRUE)
  
  write_csv(df_Summary, file_ConfusionSummary,
            append = TRUE)
  
}else{
  #Create the path
  dir.create(path_Confusion)
  
  #Create the files
  write_csv(df_Confusion, file_ConfusionMatrix,
            col_names = TRUE)
  
  write_csv(df_Summary,file_ConfusionSummary,
            col_names = TRUE)
  
} 
 


