# CostaRica-river-reforestation
Companion repository to paper "Modeling multiple ecosystem services and beneficiaries of riparian reforestation in Costa Rica"

Contains R and python code used in scenario creation and post-InVEST modeling analysis
All data is stored at this link: https://osf.io/srjwx/
Directory structure within the OSF project matches directory structure in R code

Brief descriptors of code:

# CR_InVEST_results_analysis.R
Contains script for doing post-InVEST modeling analysis and figure creation
Detailed description:
1. Estimate how much only buffering rivers by 10m underestimates the amount of forest that would need to be added
2. Calculate raw increase and percent increase in each ecosystem service
3. Calculate forest area added in the reforestation scenario, including as % of current forest cover from QGIS SAGA zonal statistics count tables of LULCs
4. Create new shapefiles that include the baseline-scenario difference per canton, normalized by canton area for visualization
5. Intersect with beneficiaries data for each canton
6. Intersect with Indigenous territories
7. Visualization

# Riparian_buffer_scenario_generation.py
Used to create riparian reforestation scenario landcover map, with 10 m of forest buffer around all rivers
Modified from code of Jeffrey R. Smith, used with permission

# sdr_rastercalc.py
Used for performing calculations on baseline and scenario rasters of the InVEST SDR model
Used to get differences between baseline and scenario and difference of difference rasters for uncertainty analysis
Modified from code of Richard P. Sharp, used with permission

# ndr_n_rastercalc.py
Same as above, but for InVEST NDR model for N

# ndr_p_rastercalc.py
Same as above, but for InVEST NDR model for P
