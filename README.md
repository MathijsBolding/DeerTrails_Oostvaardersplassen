# deerTrails_Oostvaarders


Folder **[Workflow]** contains the source code for the workflow, with its contents described below:

## Table of Contents

- [Overview](#overview)
- [File Structure](#file-structure)
- [Requirements](#requirements)
- [Usage Instructions](#usage-instructions)
- [Example](#example)
- [License](#license)
- [Contact](#contact)

## Overview
This repository contains the code to detect changes in network structure of deer trails in the Oostvaardersplassen. This project builds on the trail extraction workflow by [Jinhu-Wang]  (https://github.com/Jinhu-Wang/Extracting-ungulate-trails-in-wetlands-using-3D-point-clouds-obtained-from-airborne-laser-scanning)
The centerline is extracted in order to use network topology and graph theory on the trail network. The first part "" is a continuation of the trail extraction workflow of Jinhu (Insert repository). The code is used to extract the centerline of the deer trails to transform them from raster to polylines. This repository also contains the code for calculating the confusion matrix used for optimizing the parameters of the trail_extraction_workflow. . The workflow consists of four major steps:

1. **1-xyz2polygon.R**  
2. **2-Centerliner.py**  
3. **3-ConfusionMaker.R**  
4. **4-BoxPlotter.R**  

## File Structure

```plaintext
Optimization/
├── Environment             #Contains the yaml file needed to create the environment for the 2_Centerliner.py script.
|
├── 0_BatchRunner.sh         #Schell script that returns boxplots with performance metrics for a complete folder. 
|  
├── 1_xyz2polygon.R          # Script that converts the .xyz files to polygons using the Terra library
│
├── 2_Centerliner.py             # Script that extracts the centerline of the extracted deer polygons
│
├── 3_ConfusionMaker.R     # Script to calculate the confusion matrix
│
├── 4_BoxPlotter.R        # Script to plot the boxplots that are used to estimate the best parameters.
│
└── DeerFunctions.R     # Function library used in the R-scripts
```

## Requirements
### OS 
All scripts except the shell script "BatchRunner.sh" work on both Windows and Linux. The script "BatchRunner.sh" runs only on Linux. 
### Python
The python scrips depend: geos, pygeoops, geopandas, shapely, laspy, notebook and matplotlib. The environment for the python script can be installed from the yaml-file: "GeoScripter.yaml"in the environment folder.

### R
The R scripts in this repository use the libraries: tidyverse, terra and tidygraphs. 

## Usage Instructions 

### Optimization:
1) Create and activate the GeoScripter environment:
  - On Linux:
    ```sh
    cd path-to-Optimization/Environment
    conda env create -f GeoScripter.yaml
    conda activate GeoScripter
    ```
2) Set the directories of the manually validated plots in "3_ConfusionMaker.R". In our case the validation plots contain two groups. One grazed by only deers (DO) and one grazed by both deer and geese (DG).
   ```r
   #List and load the manually validated centerlines
   DO_ValidatedCenterlines <- list.files("dir-to-validatedcenterlines",
                                           full.names = TRUE)%>%
   map(vect)%>%
   svc()

   #List and load the manually validated polygons
   DO_ValidatedPolygons <- list.files(""dir-to-validatedcenterlines"",
                                        full.names = TRUE)  map(vect)%>%
   svc()
   ```
3) Run the "BatchRunner.sh" to calculate the confusion matrix and create the boxplots showing the F1-score, precision and recall of different parameter settings.
4) Choose optimal parameter setting. 

### Network Change Detection
**Note:** This section is under construction 

## License

This project is licensed under the **MIT License**.

