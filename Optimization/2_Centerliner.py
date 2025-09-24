#This script extracts the centerline of the created polygons 
#Import the important packages 
from multiprocessing import Pool
import pygeoops 
import geopandas as gpd
import shapely
import os
import sys
import concurrent.futures
#Create the centerline function
def Centerliner(path_name, output_folder,
               branch_length, extend, densify_distance):
        """
    Reads a .gpkg file, extracts the centerline of the polygon and saves it as a gpkg.

    Parameters
    ----------
    path_name : str
        Path to the input .gpkg file to process.
    output_folder : str
        Path to the output folder where the processed files will be saved.
    branch_length : float
        minimum length of the branches 

    Returns
    -------
    None
        Saves the centerline as a gpkg file.
    """
        #Store the important names
        file_name = os.path.basename(path_name)
    
        #Read the file
        polygonTrails = gpd.read_file(path_name, engine="pyogrio")

        #Convert from multipolygon to single
        polygonTrails = polygonTrails.explode(index_parts=True)
    
        #Extract the polygons 
        polygons = list(polygonTrails.geometry)


    
    # Initialize a list to store centerlines
        centerlines = []

        # Iterate over each polygon and compute the centerline
        for polygon in polygonTrails.geometry:
            # Process one polygon at a time
            centerline = pygeoops.centerline([polygon], min_branch_length=branch_length, extend = extend, densify_distance = densify_distance)
            centerlines.extend(centerline)
        

        #Convert it to a geoseries with the correct crs
        centerline_gs = gpd.GeoSeries(centerlines, crs= "EPSG:28992")
    
        #Save it as a geopackage
        output_gpkg_file = os.path.splitext(file_name)[0] + "_cen.gpkg"
        output_gpkg_path = os.path.join(output_folder, output_gpkg_file)
    
        centerline_gs.to_file(output_gpkg_path)
    
        print(f"Processed {file_name}") 
        
#Run the function for the complete complete folder
directory = sys.argv[1] 

def main():
    directory = sys.argv[1]
    output_folder = sys.argv[2]
    branch_length = 1
    extend = True
    densify_distance = 0.2
    # Get all files to process
    files = [entry.path for entry in os.scandir(directory) if entry.is_file()]

    # Parallelize file processing
    with concurrent.futures.ProcessPoolExecutor() as executor:
        futures = [
            executor.submit(
                Centerliner,
                path_name=file,
                output_folder=output_folder,
                branch_length=branch_length,
                extend=extend,
                densify_distance=densify_distance
            )
            for file in files
        ]
        # Wait for all tasks to complete
        concurrent.futures.wait(futures)

if __name__ == "__main__":
    main()
