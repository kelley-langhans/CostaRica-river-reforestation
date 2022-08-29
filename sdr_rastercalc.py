"""These calculations are for the Costa Rica Riparian Reforestation paper."""
# Author: Richard P. Sharp, SPRING
# modified by Kelley Langhans to do SDR raster calculations (baseline minus scenario, diff for uncertainty analysis)
# can continue to add modalities to this script for raster manipulations
# see pygeoprocessing for documentation: https://github.com/natcap/pygeoprocessing/blob/main/src/pygeoprocessing/geoprocessing.py
# run with docker on terminal using the following command: docker run --rm -it -v `pwd`:/usr/local/workspace -v /Volumes/pden2010_60m.bmp/Stanford_externaldrive/Courses/Bio_202/Data/CalForObs/CanopyCover/:/data  therealspring/inspring:latest pgp_entry_point.py
# run with docker on terminal using the following command line 1: cd Desktop/Old_comp_transfer/Docker_scripts
# line 2: docker run --rm -it -v `pwd`:/usr/local/workspace -v "/Users/kelleylanghans/Desktop/Old_comp_transfer/Stanford/Research/Costa Rica/Modeling/SDR/SDR_InVEST_output/28Feb21":/data  therealspring/inspring:latest sdr_rastercalc_3Mar21.py
# after making sure docker running, and navigating to Docker_scripts where this script is stored

# Import packages, python ones first
import logging
import pygeoprocessing
import numpy
from osgeo import gdal
from osgeo import osr #how gdal interacts with projections

# set up logging
gdal.SetCacheMax(2**27)
logging.basicConfig(
    level=logging.DEBUG,
    format=(
        '%(asctime)s (%(relativeCreated)d) %(levelname)s %(name)s'
        ' [%(funcName)s:%(lineno)d] %(message)s'))
LOGGER = logging.getLogger(__name__)

# define functions to execute

# Define subtraction function for raster calculator
# this type of array-based function is much faster than looping!!

def _diff_op(array_1, array_1_nodata, array_2, array_2_nodata, target_nodata): # each raster is a numpy array, also create arrays for nodata values, and to store the nodata value for output raster
    result = numpy.empty(array_1.shape) # create result array of same dimensions
    result[:] = target_nodata # fill it in with all no data values
    valid_mask = (
        (array_1 != array_1_nodata) & (array_2 != array_2_nodata)) # create a validity mask, true for all areas where both input rasters are NOT no data
    result[valid_mask] = array_1[valid_mask] - array_2[valid_mask] # for only values where input arrays are NOT no data, subtract those two arrays and input into output raster
    return result # spit out the result raster!

# Define main function we want to carry out

def main():
    """Entry point."""
    LOGGER.debug('hello it worked') # print to test function is running

    # set up a target projection to reproject into
    targetprojection = osr.SpatialReference()
    targetprojection.ImportFromEPSG(26917) #customize with EPSG wanted
    # Set up paths to rasters dealing with
    rasterpath1 = '/Data/SDR/SDR_InVEST_output/Baseline_28Feb21/sed_export_CR_baseline_3.7.0_28Feb21.tif' # pulling from "data" directory we set up on Docker in the above command (since we're pulling data from a different folder than we're running in
    rasterpath2 = '/Data/SDR/SDR_InVEST_output/Scenario_hicforest_19Feb21/sed_export_CR_scen10_3.7.0_hicforest_19Feb21.tif'
    rasterpath3 = '/Data/SDR/SDR_InVEST_output/Scenario_medcforest_19Feb21/sed_export_CR_scen10_3.7.0_medcforest_19Feb21.tif'
    rasterpath4 = '/Data/SDR/SDR_InVEST_output/Scenario_locforest_19Feb21/sed_export_CR_scen10_3.7.0_lowcforest_19Feb21.tif'
    # set up raster to write results to for calculator
    target_raster_path2 = '/Data/SDR/SDR_InVEST_output/sed_export_CR_scen10_hicforest_28Feb21_diff.tif'
    target_raster_path3 = '/Data/SDR/SDR_InVEST_output/sed_export_CR_scen10_medcforest_28Feb21_diff.tif'
    target_raster_path4 = '/Data/SDR/SDR_InVEST_output/sed_export_CR_scen10_locforest_28Feb21_diff.tif'
    # set no data value for target raster (make sure not a possible result of calculations)
    target_nodata = -9999
    # Get info about rasters (this is a dictionary entry aka a way of storing human readablel info values matched with keys) in order to grab no data values
    rasterinfo1 = pygeoprocessing.get_raster_info(rasterpath1)
    rasterinfo2 = pygeoprocessing.get_raster_info(rasterpath2)
    rasterinfo3 = pygeoprocessing.get_raster_info(rasterpath3)
    rasterinfo4 = pygeoprocessing.get_raster_info(rasterpath4)

    # Grab no data key, index the 0th item (in this case, no data value from 1st raster band, although we only have one raster band)
    raster_1_nodata = rasterinfo1['nodata'][0]
    raster_2_nodata = rasterinfo2['nodata'][0]
    raster_3_nodata = rasterinfo3['nodata'][0]
    raster_4_nodata = rasterinfo4['nodata'][0]

    # Run raster calculator to do subtraction function defined above, grab first band of rasters, 'raw' to tell python just a single #
    # Note that deciding to write output to FLoat32, can customize this based on type of values expected
    # Here, subtract baseline from scenario
    pygeoprocessing.raster_calculator(
        [(rasterpath2, 1), (raster_2_nodata, 'raw'),
         (rasterpath1, 1), (raster_1_nodata, 'raw'),
         (target_nodata, 'raw')], _diff_op, target_raster_path2,
        gdal.GDT_Float32, target_nodata)
    pygeoprocessing.raster_calculator(
        [(rasterpath3, 1), (raster_3_nodata, 'raw'),
         (rasterpath1, 1), (raster_1_nodata, 'raw'),
         (target_nodata, 'raw')], _diff_op, target_raster_path3,
        gdal.GDT_Float32, target_nodata)
    pygeoprocessing.raster_calculator(
        [(rasterpath4, 1), (raster_4_nodata, 'raw'),
         (rasterpath1, 1), (raster_1_nodata, 'raw'),
         (target_nodata, 'raw')], _diff_op, target_raster_path4,
        gdal.GDT_Float32, target_nodata)


    # ask to print raster info as a check
    LOGGER.debug(rasterinfo1)

    # Subtract hi c (worse buffers) difference from lo c (better buffers) to get difference of difference
    # Read in files just wrote
    rasterpath5 = '/Data/SDR/SDR_InVEST_output/sed_export_CR_scen10_locforest_28Feb21_diff.tif'
    rasterpath6 = '/Data/SDR/SDR_InVEST_output/sed_export_CR_scen10_hicforest_28Feb21_diff.tif'
    # Write path
    target_raster_path5 = '/Data/SDR/SDR_InVEST_output/sed_export_CR_scen10_lominhicforest_28Feb21_diffofdiff.tif'
    # Get raster info
    rasterinfo5 = pygeoprocessing.get_raster_info(rasterpath5)
    rasterinfo6 = pygeoprocessing.get_raster_info(rasterpath6)
    # Grab no data key, index the 0th item (in this case, no data value from 1st raster band, although we only have one raster band)
    raster_5_nodata = rasterinfo5['nodata'][0]
    raster_6_nodata = rasterinfo6['nodata'][0]
    # Perform subtraction
    pygeoprocessing.raster_calculator(
        [(rasterpath5, 1), (raster_5_nodata, 'raw'),
         (rasterpath6, 1), (raster_6_nodata, 'raw'),
         (target_nodata, 'raw')], _diff_op, target_raster_path5,
        gdal.GDT_Float32, target_nodata)

    # ask to print raster info as a check
    LOGGER.debug(rasterinfo5)


    # # set up a vector to mask to
    # vectorpath= '/data/Bay_Area_Counties/BA_9countyextent_32610.shp'
    # vectorinfo = pygeoprocessing.get_vector_info(vectorpath)
    # # function to grab and transform bb from vector to target projection
    # bb=pygeoprocessing.transform_bounding_box(
    #     vectorinfo['bounding_box'], vectorinfo['projection_wkt'], targetprojection.ExportToWkt())
    # # function to transform raster
    # pygeoprocessing.warp_raster(
    #     rasterpath1, (10,-10), '/data/NLCD_2001_Impervious_L48_20190405/NLCD_2001_Impervious_L48_20190405_BAclip_32610.tif', # specify input, pixel size write to (here in degrees, get by measuring original pixels in degrees), path to write to
    #     'near'
    #     , target_bb=bb
    #     , target_projection_wkt=targetprojection.ExportToWkt()
    #     ,vector_mask_options={'mask_vector_path':vectorpath}
    #     ) # interpolation method, set extent by bb, set projection, mask to vector

    LOGGER.debug('all done!') # print done to debug

if __name__ == '__main__':
    main()



# 4326 transformation
# for Cal For Obs data (.00011, -.00009)
# For pden (ppl per km2) (.0007,-.0005)
# NLCD (.0003,-.00027)
