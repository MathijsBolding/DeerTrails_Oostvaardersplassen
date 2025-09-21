# DeerTrails_Oostvaardersplassen
## Table of Contents

- [Overview](#overview)
- [File Structure](#file-structure)
- [Requirements](#requirements)
- [Usage Instructions](#usage-instructions)
- [License](#license)


## Overview
This repository contains the code to detect changes in the network structure of deer trails in the Oostvaardersplassen. This project builds on the trail extraction workflow by Jinhu-Wang:(https://github.com/Jinhu-Wang/Extracting-ungulate-trails-in-wetlands-using-3D-point-clouds-obtained-from-airborne-laser-scanning). 

As addition to the original workflow, the centerlines of the extracted trails are extracted and converted to "nodes" and "edges" in order to use network topology and graph theory. The first part of this repository contains the code for calculating the confusion matrix used for optimizing the parameters of the trail extraction workflow. Optimizing the workflow parameters is necessary in order to get comparable results across datasets differing in surveying method and point cloud density. The second part, "Network Change Detection" is used to detect and visualize changes in the trail network structure. 

## File Structure

```plaintext
Optimization/         
├── 0_BatchRunner.sh         # Shell script that generates boxplots with performance metrics.
├── 1_xyz2polygon.R          # Converts the extracted trails from rasters to polygons using the Terra library.
├── 2_Centerliner.py         # Extracts the centerline of the polygonized trails.
├── 3_ConfusionMaker.R       # Calculates the confusion matrix.
├── 4_BoxPlotter.R           # Plots boxplots for parameter optimization.
└── DeerFunctions.R          # Function library for R scripts.

Environments/
└──GeoScripter.yaml          # Contains the YAML file for the 2_Centerliner.py environment.

NetworkChangeDetection/      # Scripts for mapping and detecting changes in network structure.

HandyFunctions/
└── ConverterFunctions.ipynb  # Notebook with file conversion functions.
```

## Requirements
### Operating System 
All scripts except the shell script "BatchRunner.sh" work on both Windows and Linux. The script "BatchRunner.sh" runs only on Linux. 
### Python
The python scrips depend: geos, pygeoops, geopandas, shapely, laspy, notebook and matplotlib. The environment for the python script can be installed from the yaml-file: "GeoScripter.yaml"in the environment folder.

### R
The R scripts in this repository use the libraries: tidyverse, terra and tidygraphs. 

## Usage Instructions 

### Optimization:
0. **Extract deer trails:**
  - Run the original trail extraction workflow (Jinhu-Wang/Extracting-ungulate-trails) with multiple parameter settings to generate the input data for this repository.
  - Save the extracted trails as .xyz files in a dedicated directory for further processing.
1. **Set up the Python environment:**:
  - On Linux:
    ```sh
    cd path-to-Optimization/Environment
    conda env create -f GeoScripter.yaml
    conda activate GeoScripter
    ```
2. **Configure `3_ConfusionMaker.R`:**
  - Set the directories of the manually validated plots in "3_ConfusionMaker.R". In our case the validation plots contain two groups. One grazed by only deers (DO) and one grazed by both deer and geese (DG).
   ```r
   #List and load the manually validated centerlines
   DO_ValidatedCenterlines <- list.files("dir-to-validatedcenterlines",
                                           full.names = TRUE)%>%
   map(vect)%>%
   svc()

   #List and load the manually validated polygons
   DO_ValidatedPolygons <- list.files(""dir-to-validatedcenterlines"",
                                        full.names = TRUE)%>%
   map(vect)%>%
   svc()
   ```
3. **Run the analysis:**
   - Execute `0_BatchRunner.sh` to generate confusion matrices and boxplots.
4. **Select parameters:**
   - Use the boxplots to identify the optimal parameter settings.

### Network Change Detection
 **Under Construction:** This section will include instructions for detecting and visualizing changes in trail networks.
 
## License

This project is licensed under the **MIT License**.

