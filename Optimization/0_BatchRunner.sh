#!/bin/bash

# Check if two arguments are provided
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 input_folder final_output_folder"
    echo "Please provide both the initial input folder and ConfusionFileName"
    exit 1
fi

#Assign the variables 
input_folder="$1"
Confusion_file="$2"



echo "Input folder path: '$input_folder'"
echo "Output file path: '$Confusion_file' " 

if [ ! -d "$input_folder" ]; then
    echo "Error: Input folder '$input_folder' does not exist."
    exit 1
fi


find "$input_folder" -type d -path '*/extr' | while read -r folder; do
    echo "Processing folder: $folder"
    
      # Navigate one folder level up from the found 'extr' folder
	parent_folder=$(dirname "$folder")
	echo "Parent folder: $parent_folder"
	
        pol_folder="$parent_folder/pol"
        cen_folder="$parent_folder/cen"
	
	mkdir -p "$pol_folder"
  	mkdir -p "$cen_folder"
  	 

	# Check if input folder exists
	if [ ! -d "$input_folder" ]; then
	    echo "Error: Input folder '$input_folder' does not exist."
	    exit 1
	fi

	# Run the scripts in sequence, passing output as input to the next
	echo "Running 1_xyz2polygon.R..."
	Rscript "$(dirname "$0")/1_xyz2polygon.R" "$folder" "$pol_folder"
	if [ $? -ne 0 ]; then
	    echo "Error: xyz2polygon.R failed."
	    exit 1
	fi

	echo "Running Centerline.py..."
	python "$(dirname "$0")/2_Centerliner.py" "$pol_folder" "$cen_folder"
	if [ $? -ne 0 ]; then
	    echo "Error: Centerline.py failed."
	    exit 1
	fi
	
	#Last script to create the confusion matrix
		# Run the scripts in sequence, passing output as input to the next
	echo "Running 3_ConfusionMaker.R..."
	Rscript "$(dirname "$0")/3-ConfusionMaker.R" "$cen_folder" "$pol_folder" "$Confusion_file"
	if [ $? -ne 0 ]; then
	    echo "Error: 3-ConfusionMaker.R failed."
	    exit 1
	fi

	

	echo "All scripts completed successfully. Confusion scores appended to: $Confusion_file"
done

#Run the last R script 
Rscript "$(dirname "$0")/4_BoxPlotter.R" "$Confusion_file" "$input_folder"

echo "Boxplots for this parameter created"

echo "All folders processed."
