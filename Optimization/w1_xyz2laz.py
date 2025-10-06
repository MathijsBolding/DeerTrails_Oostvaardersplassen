import laspy
import os
import numpy as np
import sys

def xyz2laz(xyz_file, output_folder):
    #Save input file name
    file_name = os.path.basename(xyz_file)

    #Create output laz-file
    output_laz_file = os.path.splitext(file_name)[0] + ".laz"
    output_laz_path = os.path.join(output_folder, output_laz_file)

    # Load in the .xyz data
    data = np.loadtxt(xyz_file, usecols=(0, 1, 2))
    x, y, z = data[:, 0], data[:, 1], data[:, 2]

    #Create a LAS header
    header = laspy.LasHeader(point_format=1, version="1.2")
    header.scales = (0.01, 0.01, 0.01)  # Set scale factors
    header.offsets = (np.min(x), np.min(y), np.min(z))
    
    # Create LAS data
    las = laspy.LasData(header)
    las.x = x
    las.y = y
    las.z = z
    
    #Add the scalar field
    #las.add_extra_dim(laspy.ExtraBytesParams(name="scalar", type=np.float32))
    #las["scalar"] = scalar.astype(np.float32)

    #Save as laz
    with laspy.open(output_laz_path, mode="w", header=header) as las_writer:
       las_writer.write_points(las.points)

       print(f"saved as LAZ: {output_laz_path}")

#Loop it over teh folder
for entry in os.scandir(sys.argv[1]):  
    if entry.is_file():  
        xyz2laz(entry.path, output_folder = sys.argv[2])
