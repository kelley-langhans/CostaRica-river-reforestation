# Author: Jeffrey R. Smith, Kelley Langhans
# Given input of landcover map, generates new landcover map with a 1 pixel forest buffer around all water bodies within specified land use classes
# For our land cover map, 1 pixel=10m, so this is a 10m buffer

#Libraries
import rasterio # like gdal
import numpy as np # important science library
import matplotlib.pyplot as plt # base plotting
import scipy
import scipy.ndimage

#Base lulc map
#Opens the raster (contains data and spatial info)
lulc = rasterio.open(r"/Users/kelleylanghans/Desktop/Old_comp_transfer/Stanford/Research/Costa Rica/Modeling/CR_rivers_github_repo/Data/LULC/lulc.tif")
# Will not work if there are spaces in filepath

#Read in data
#Grabs just the data from raster (a matrix of values with each pixel value, saves space!!)
data = lulc.read(1)

# Make water pixels = 1 and everyhting else equal 0
# converts to matrix of 0s and 1s where there is water. Not actually boolean, so would need to convert to save space.
convertable = [30, 1210]
water =  np.where(np.isin(data, convertable), 1, 0)

#Buffer rivers out (3 = 1 pixel in each direction, 5 = 2 pixels in each direction)
#function already written: iterates through matrix. For each value/pixel, searches the a certain number of pixels (here 1, creating a submatrix that is 3x3) in every direction and assigns the pixel with the max value (here, max possible is 1) that it finds in that box
#Creates a buffer where all pixels that are a river or adjacent to a river are converted to 1, rest are 0
#If want to rewrite for larger buffers will have to be careful here
water =  scipy.ndimage.maximum_filter(water, size = 3)

#Select lulc classes eligable for conversion
#make another mask where all convertable LULC values are 1, all other values 0
convertable = [1,3,4,7,9,10,11,13,20,21,22,23]
mask =  np.where(np.isin(data, convertable), 1, 0)

#Overlap mask and water layer
#multiply together to create a mask of only those convertable values w/in buffer
#set water object to 0 afterwards to not keep saving it in memory
mask = water * mask
water = 0

#Replace them with your indicated values
#Create a new matrix, which is 40 where mask is 1, and original values everywhere else
lulcToBurn = 40
mask = np.where(mask == 1, lulcToBurn, data)

#Dat
#set the type of number stored in the layer (unsigned interger 16)
#for more, see https://numpy.org/devdocs/user/basics.types.html
mask = mask.astype(np.uint16)

#Save file
#Will have to change the path to make this work for me
with rasterio.Env():
	profile = lulc.profile
	profile.update(
		dtype=rasterio.uint16,
		count=1,
		driver="GTiff",
		height=mask.shape[0],
		width=mask.shape[1],
		transform=lulc.transform,
		compress = 'lzw')


	file = r"/Users/kelleylanghans/Desktop/Old_comp_transfer/Stanford/Research/Costa Rica/Modeling/CR_rivers_github_repo/Data/LULC/scenario.tif"

	with rasterio.open(file, 'w', **profile, tile = False) as dst:
		dst.write_band(1,mask)
