#4-BoxPlotter.R 
#This function is the last step and returns the boxplots of all the parameter settings
library(tidyverse)
library(ggpubr)

#Take the entries from the terminal again
args <- commandArgs(trailingOnly = TRUE)

#Move one up to get the output folder direction 
OutputFolder <- dirname(args[1])

#Get the parameter name  from the folder path
ParameterName <- basename(args[2])

#Read in the summary file
df_ConfusionSummary <- read.csv(file.path(args[1], "ConfusionSummary.csv"))%>%
    filter(Parameter == ParameterName)


#Make the format long for plotting
df_ConfusionLong <- df_ConfusionSummary %>%
  group_by(ParSetting)%>%
  summarise(F1_score = list(F1_score), 
            Recall = list(Recall),
            Precision = list(Precision))%>%
  unnest(cols = c(F1_score, Recall, 
                  Precision))%>%
  pivot_longer(cols = c(Recall, Precision, 
                        F1_score),
               names_to = "Metric",
               values_to = "Value")

#Create the path name
plotPath <- file.path(OutputFolder, "Graphs/Optimization", paste0(ParameterName, ".pdf"))

#Create both plots and combine them 
#Create the plot
gg_line <- ggplot(df_ConfusionLong, aes(x = ParSetting, 
                                   y = Value, 
                                   color = Metric, 
                                   group = Metric)) +
        geom_line(size = 1.3) +
        geom_point(size = 2) +
        ggtitle(paste0("Perfomance metrics: ", ParameterName))+    
        theme_bw(base_size = 15) +
        ylab("Value") +
        xlab("Parameter Setting") +
        labs(color = "")+
        theme(legend.position = "bottom")+
        ylim(0, 1)

gg_PrecisionRecall <- ggplot(df_ConfusionSummary, aes(x = Recall, 
                                                      y = Precision)) +
  geom_line(size = 1.3) +
  geom_point(size = 2) +
  theme_bw(base_size = 15) +
  ggtitle(paste0("Precision-Recall: ", ParameterName))+
  ylab("Precision") +
  xlab("Recall") +
  labs(color = "")+
  theme(legend.position = "bottom")+
  ylim(0,1)+
  xlim(0,1)

#Create a PDF and plot the plots in the pdf
pdf(plotPath, onefile = TRUE)

gg_line
gg_PrecisionRecall

#Save it to the disk 
dev.off()

