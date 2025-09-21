#4-BoxPlotter.R 
#This function is the last step and returns the boxplots of all the parameter settings
library(tidyverse)

#Take the entries from the terminal again
args <- commandArgs(trailingOnly = TRUE)

#Move one up to get the output folder direction 
OutputFolder <- dirname(args[1])

#Get the parameter name  from the folder path
ParameterName <- basename(args[2])

#Read in the confusion dataset
df_Confusion <- read_csv(args[1])

#Make the format long for plotting
df_ConfusionLong <- df_Confusion %>%
  filter(Parameter == ParameterName)%>%
  group_by(ParSetting) %>%
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
boxplotPath <- file.path(OutputFolder, "BoxPlots", paste0(ParameterName, ".png"))

print(boxplotPath)

png(boxplotPath)
#Create the plot
ggplot(df_ConfusionLong,
                mapping = aes(x = ParSetting, 
                              y = Value, 
                              fill = Metric))+
                  geom_boxplot()+
                  stat_summary(fun = mean, geom = "point", 
                  shape = 20, size = 3, color = "black", 
                  position = position_dodge(width = 0.75))+
                  theme_bw(base_size = 14)+
                  theme(text = element_text(face="bold"),
                        legend.position = "bottom",
                        legend.title =  element_blank())+
                  ggtitle(paste0("Parameter: ", ParameterName))+
                  xlab("Parameter setting")


#Save it to the disk 
dev.off()


