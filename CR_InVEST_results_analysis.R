# Author: Kelley E. Langhans
# Companion code for the following manuscript:
# Modeling multiple ecosystem services and beneficiaries of riparian reforestation in Costa Rica
# Published in Ecosystem Services in 2022

# Goal: Do post-modeling analysis on InVEST results from Costa Rica riparian buffer reforestation project
# 1. Estimate how much only buffering rivers by 10m underestimates the amount of forest that would need to be added
# 2. Calculate raw increase and percent increase in each ecosystem service
# 3. Calculate forest area added in the reforestation scenario, including as % of current forest cover from QGIS SAGA zonal statistics count tables of LULCs
# 4. Create new shapefiles that include the baseline-scenario difference per canton, normalized by canton area for visualization
# 5. Intersect with beneficiaries data for each canton
# 6. Intersect with Indigenous territories
# 7. Visualization

# Note that some analysis was performed in Python (for creation of reforestation scenario), ArcGIS (for scenario creation), and QGIS (for geospatial analysis)

# Input data available here:
# Link: https://osf.io/srjwx/
# Citation: Langhans, Kelley E. 2022. “Costa Rica Riparian Reforestation.” OSF. August 26. osf.io/srjwx.
# DOI 10.17605/OSF.IO/SRJWX

#################
# Set WD
#################

setwd("~/Desktop/Old_comp_transfer/Stanford/Research/Costa Rica/Modeling/CR_rivers_github_repo")
# set this to wherever you store the file for the input data

###############
# load libraries
################

library(plyr)
library(ggplot2)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(exactextractr)
library(sf)
library(openxlsx)
library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(reshape2)
library(pals)
library("PupillometryR")


######################
# define functions
####################

# Plotting function

# Adapted from:
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
# 
# Title can be passed as c("title1", "title2"). If only one title is
# provided it will be used as single title for all columns. If several titles
# are provided each column will then have it's own title.
#
# Title size, font and face can also be provided
multiplot <- function(..., plotlist = NULL, cols = 1, layout = NULL, title = NULL, 
                      fontsize = 14, fontfamily = "Helvetica", fontface = "bold") {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (length(title)>0){
    layout <- rbind(rep(0, ncol(layout)), layout)
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), 
                                               ncol(layout), 
                                               heights = if (length(title)>0) {unit(c(0.5, rep(5,nrow(layout)-1)), "null")}
                                               else {unit(c(rep(5, nrow(layout))), "null")})))
    if(length(title) > 1){
      ncols <- 1:ncol(layout)
      for(i in seq(ncols)){
        grid.text(title[i], 
                  vp = viewport(layout.pos.row = 1, layout.pos.col = i),
                  gp = gpar(fontsize = fontsize, fontfamily = fontfamily, fontface = fontface))
      }
    } else {
      grid.text(title, 
                vp = viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(layout)),
                gp = gpar(fontsize = fontsize, fontfamily = fontfamily, fontface = fontface))
    }
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Normalizing
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# color setting
bivcol <- function(pal, nx=3, ny=3){
  tit <- substitute(pal)
  if(is.function(pal)) pal <- pal()
  ncol <- length(pal)
  if(missing(nx)) nx <- sqrt(ncol)
  if(missing(ny)) ny <- nx
  image(matrix(1:ncol, nrow=ny), axes=FALSE, col=pal)
  mtext(tit)
}

############# The following sections can be commented out to save run time when recalculation of zonal statistics and data cleaning isn't needed. Instead, there is an option to read in resulting dataframes for visualization ######################

####################
# read in data
###################

### InVEST watershed shapefile outputs

# SDR export wsheds baseline
basesdr_wsheds <- readOGR( dsn = "Data/SDR/SDR_InVEST_output/Baseline_28Feb21", layer = "watershed_results_sdr_CR_baseline_3.7.0_28Feb21")

# SDR export wsheds scenario hi c for forest buffers
scenhisdr_wsheds <- readOGR( dsn = "Data/SDR/SDR_InVEST_output/Scenario_hicforest_19Feb21", layer = "watershed_results_sdr_CR_scen10_3.7.0_hicforest_19Feb21")

# SDR export wsheds scenario med c for forest buffers
scenmedsdr_wsheds <- readOGR( dsn = "Data/SDR/SDR_InVEST_output/Scenario_medcforest_19Feb21", layer = "watershed_results_sdr_CR_scen10_3.7.0_medcforest_19Feb21")

# SDR export wsheds scenario lo c for forest buffers
scenlosdr_wsheds <- readOGR( dsn = "Data/SDR/SDR_InVEST_output/Scenario_locforest_19Feb21", layer = "watershed_results_sdr_CR_scen10_3.7.0_lowcforest_19Feb21")

# NDR export wsheds baseline
basendr_wsheds <- readOGR( dsn = "Data/NDR/NDR_InVEST_output/Baseline_22Jan21", layer = "watershed_results_ndr_CR_baseline_3.7.0_22Jan21")

# NDR export wsheds scenario hi retention efficiency for forest buffers
scenhindr_wsheds <- readOGR( dsn = "Data/NDR/NDR_InVEST_output/Scenario10_hireforest_22Jan21", layer = "watershed_results_ndr_CR_scenario_hieffforest_3.7.0_22Jan21")

# NDR export wsheds scenario med retention efficiency for forest buffers
scenmedndr_wsheds <- readOGR( dsn = "Data/NDR/NDR_InVEST_output/Scenario10_medreforest_22Jan21", layer = "watershed_results_ndr_CR_scenario_medeffforest_3.7.0_22Jan21")

# NDR export wsheds scenario lo retention efficiency for forest buffers
scenlondr_wsheds <- readOGR( dsn = "Data/NDR/NDR_InVEST_output/Scenario10_loreforest_22Jan21", layer = "watershed_results_ndr_CR_scenario_loeffforest_3.7.0_22Jan21")


### Difference rasters
# Created in QGIS raster calculator by subtracting baseline scenario from reforestation scenario

# SDR export difference raster ( med scenario-baseline)
sdr_diff <- raster("Data/SDR/SDR_InVEST_output/sed_export_CR_scen10_medcforest_28Feb21_diff.tif")

# SDR export difference raster ( lo scenario-baseline)
sdrlo_diff <- raster("Data/SDR/SDR_InVEST_output/sed_export_CR_scen10_locforest_28Feb21_diff.tif")

# SDR export difference raster ( hi scenario-baseline)
sdrhi_diff <- raster("Data/SDR/SDR_InVEST_output/sed_export_CR_scen10_hicforest_28Feb21_diff.tif")

# # SDR diff of diff between lo and hi c (not used in current analysis, although visualized for analysis, but could be used for canton zonal stats if desireable)
# sdr_diffdiff <- raster("Data/SDR/SDR_InVEST_output/sed_export_CR_scen10_lominhicforest_28Feb21_diffofdiff.tif")


# N export difference raster (med scenario-baseline )
n_diff <- raster("Data/NDR/NDR_InVEST_output/n_export_CR_scen10_medeffforest_22Jan21_diff.tif")

# N export difference raster (lo scenario-baseline )
nlo_diff <- raster("Data/NDR/NDR_InVEST_output/n_export_CR_scen10_loeffforest_22Jan21_diff.tif")

# N export difference raster (hi scenario-baseline )
nhi_diff <- raster("Data/NDR/NDR_InVEST_output/n_export_CR_scen10_hieffforest_22Jan21_diff.tif")

# # N export diff of diff between lo and hi ret eff (see note above)
# n_diffdiff <- raster("Data/NDR/NDR_InVEST_output/n_export_CR_scen10_himinloeffforest_22Jan21_diffofdiff.tif")


# P export difference raster (med scenario-baseline)
p_diff <- raster("Data/NDR/NDR_InVEST_output/p_export_CR_scen10_medeffforest_22Jan21_diff.tif")

# P export difference raster (lo scenario-baseline )
plo_diff <- raster("Data/NDR/NDR_InVEST_output/p_export_CR_scen10_loeffforest_22Jan21_diff.tif")

# P export difference raster (hi scenario-baseline )
phi_diff <- raster("Data/NDR/NDR_InVEST_output/p_export_CR_scen10_hieffforest_22Jan21_diff.tif")

# # P export diff of diff between lo and hi ret eff (see note above)
# p_diffdiff <- raster("Data/NDR/NDR_InVEST_output/p_export_CR_scen10_himinloeffforest_22Jan21_diffofdiff.tif")


# Carbon (ipcc Baccini hybrid all tree, see text for details) difference raster (scenario - baseline)
carbon_bac_diff <- raster("Data/Carbon/Carbon_InVEST_output/delta_cur_fut_CR_ipccBaccini_3.7.0_14June21.tif")

#Carbon (ipcc only) difference raster (scenario - baseline)
carbon_ipcc_diff <- raster("Data/Carbon/Carbon_InVEST_output/delta_cur_fut_CR_ipcc_3.7.0_24Jul20.tif")


### Baseline rasters

# Carbon (ipcc Baccini all tree) baseline raster
carbon_bac_base <- raster("Data/Carbon/Carbon_InVEST_output/tot_c_cur_CR_ipccBaccini_3.7.0_14June21.tif")

#Carbon (ipcc) baseline raster
carbon_ipcc_base <- raster("Data/Carbon/Carbon_InVEST_output/tot_c_cur_CR_ipcc_3.7.0_24Jul20.tif")

# N baseline raster
n_base <- raster("Data/NDR/NDR_InVEST_output/Baseline_22Jan21/n_export_CR_baseline_3.7.0_22Jan21.tif")

# P baseline raster
p_base <- raster("Data/NDR/NDR_InVEST_output/Baseline_22Jan21/p_export_CR_baseline_3.7.0_22Jan21.tif")

# SDR baseline raster
sdr_base <- raster("Data/SDR/SDR_InVEST_output/Baseline_28Feb21/sed_export_CR_baseline_3.7.0_28Feb21.tif")


### Scenario rasters

# Carbon (ipcc Baccini all tree) scenario raster
carbon_bac_scen <- raster("Data/Carbon/Carbon_InVEST_output/tot_c_fut_CR_ipccBaccini_3.7.0_14June21.tif")

#Carbon (ipcc) scenario raster
carbon_ipcc_scen <- raster("Data/Carbon/Carbon_InVEST_output/tot_c_fut_CR_ipcc_3.7.0_24Jul20.tif")


#### Forest cover rasters

# Baseline (0/1 raster where 1 is forest, 0 is otherwise. Pixels are 10x10m, or 100m2)
fc_base <- raster("Data/Forest_Cover/CR_LULC_baseline_forestmask.tif")

# Scenario (0/1 raster where 1 is forest, 0 is otherwise. Pixels are 10x10m, or 100m2)
fc_scen <- raster("Data/Forest_Cover/CR_LULC_scen10_forestmask.tif")

#Increase with reforestation
fc_diff <- raster("Data/Forest_Cover/CR_FCdiff_scen10minbase.tif")


### Other data

# Cantons
# Cantons used in 2011 census
cantones11 <- readOGR(dsn = "Data/Beneficiaries/Cantones_de_Costa_Rica_2011", layer = "Cantones_de_Costa_Rica_26917")
# All cantons as of 2022 (3 cantons added 2018-2022)
cantones22 <- readOGR(dsn = "Data/Beneficiaries/Cantones_de_Costa_Rica_2022", layer = "Cantones_CostaRica_2022_26917")
# Note: later in the code there is an option to chose which one of these to use to calculate zonal statistics.

# Water access per canton
water <- read_xlsx("Data/Beneficiaries/CR_censusdata/CR_2011census_watersource.xlsx", sheet=3, col_names = TRUE, trim_ws=TRUE)

# Racial indicators per canton
race <- read_xlsx("Data/Beneficiaries/CR_censusdata/Cuadro4_Indicadores_raciales_por_canton.xlsx", sheet=2, col_names = TRUE, trim_ws=TRUE)

# Poor households per canton (below the poverty line)
pov <- read_xlsx("Data/Beneficiaries/CR_censusdata/Cuadro1_Hogares_pobres_por_canton.xlsx", sheet=2, col_names = TRUE, trim_ws=TRUE)

# Women-led households per canton
wom <- read_xlsx("Data/Beneficiaries/CR_censusdata/Cuadro35_Jefatura_hogares_por_distrito.xlsx", sheet=2, col_names = TRUE, trim_ws=TRUE)

# Indigenous territories shapefile
ind_terr <- readOGR(dsn = "Data/Beneficiaries/Indigenous_territories_CostaRica/Costa_Rica_indigenous_territories_shapefile", layer = "Costa_Rica_indigenous_territories_26917_fixedgeom")

# Non-indigenous land shapefile
non_ind_terr <- readOGR(dsn = "Data/Beneficiaries/Indigenous_territories_CostaRica/Costa_Rica_nonindigenous_land_shapefile", layer = "Costa_Rica_nonidigenous_land_onepolygon")

# Pixel counts per landcover class in baseline landcover, derived in QGIS
baselu_ct <- read.csv("Data/LULC/CR_base_LULC_classcount.csv")

# Pixel counts per landcover class in scenario landcover, derived in QGIS
scenlu_ct <- read.csv("Data/LULC/CR_scen10_LULC_classcount.csv")

# LULC class names
lulc_names <- read.csv("Data/LULC/lulc_names.csv")

# Stream reaches with 50 m buffer and slope shapefile
reaches <- st_read("Data/LULC/Watersheds_clipped_to_SDR_streams_50m_buffer_with_slope.gpkg")

########################################
# Slope underestimation analysis
#######################################

# Using data on 95th percentile of pixel slope in 50 m buffered stream reaches in Costa Rica, answer the following questions:
# 1. What percentage of stream reaches are steep (95th percentile > 40% slope)?
# 2. Of those steep stream reaches, what is their current LULC?
# 3. Of non-steep stream reaches, what % are in urban and what % are in rural areas?

# Q1 What percentage of stream reaches are steep?
steep <- subset(reaches, reaches$slp_95quantile_perc>=40)
nsteep <- length(steep$slp_95quantile_perc)
flat <- subset(reaches, reaches$slp_95quantile_perc<40)
nflat <- length(flat$slp_95quantile_perc)
ntotal <- length(reaches$slp_95quantile_perc) # some reaches are missing, check out why

# explore missing reaches
names1 <- steep$Wtrshd_ID
names2 <- flat$Wtrshd_ID
x <- subset(reaches, !(reaches$Wtrshd_ID %in% names1))
y <- subset(x, !(x$Wtrshd_ID %in% names2))
dim(y) #85 not in either, why?
y # all NAs, appear to be empty polygons. Leave them out of analysis.

# % steep reaches
(nsteep / (nsteep + nflat))*100 #7.3%

# Q2 Of those steep stream reaches, what is their current LULC?

# Export steep stream reaches to mask baseline LULC (will have to buffer shapefile to 0 m first to remove self-intersections), and then count up pixels of LULC in that new layer, in QGIS
st_write(steep, dsn = "Outputs", layer = "Steep_stream_reaches", driver = "ESRI Shapefile")

# QGIS steps:
# Buffer steep reach shapefile with 0 m to remove self intersectionts
# Mask baseline landcover with steep reach shapefile to extract only landcover w/in those reaches
# Generate raster unique values report to get a count of those pixels in each LULC type

# Read in pixel counts from doing this
steeplu_ct <- read.csv("Data/LULC/Steepreach_LULC_classcount.csv") 
steeplu_ct2 <- merge(lulc_names, steeplu_ct, by = c("value"))

# Calculating % of each LULC
steeptotct <- sum(steeplu_ct2$count) # total pixels
steeptotct_nowater <- steeptotct - sum(subset(steeplu_ct2, steeplu_ct2$English == "Streams from DEM" | steeplu_ct2$English == "Water bodies - ESA")$count) # total pixels not counting water
steeplu_ct2$Perc_landcover <- round((steeplu_ct2$count/steeptotct_nowater)*100, 2)
# note that this generates a % value for the water categories, this should be ignored. 
# of all the steep reaches, most is forest 89%
# 6.84% is pasture
# less than 1% for any other individual crop category

write.csv(steeplu_ct2, "./Outputs/Steepreach_lulc_count.csv", row.names = FALSE)

# Q3 Of non-steep stream reaches, what % are in urban and what % are in rural areas?

# Calculating % of each LULC
# Each reach individually should probably be buffered based on whether they are surrounded by urban or rural areas.
# How urban and rural are defined is up for debate, but here we are approximating the presence of urban pixels w/in 50 m of the stream, which seems reasonable--if there are enough urban pixels in the surrounding area, the stream should be treated as urban.
# We use 50% urban pixels as our cutoff for whether a stream should be considered urban
# even if streams are already buffered with 10 m of forest in urban areas, these reaches should still have over 50% urban pixels because our buffer is 50 m

# Export flat stream reaches to mask baseline LULC 
st_write(flat, dsn = "Outputs", layer = "Flat_stream_reaches", driver = "ESRI Shapefile")

# QGIS steps:
# Buffer flat reach shapefile with 0 m to remove self intersectionts
# Mask baseline landcover with flat reach shapefile to extract only landcover w/in those reaches
# Create a mask of that layer where water = NA, urban LULC =1, all other LULC = 0 using raster calculater
# Use zonal statistics to calculate mean value of cells for that mask within each stream reach, so that the mean value represents percent urban pixels 

# Read in zonal statistics
flat_zstats <- read.csv("Data/LULC/zstats_flatreaches_urban.csv")

# Calculate % of reaches with over 50% urban pixels
urban <- subset(flat_zstats, flat_zstats$X_mean >= .5)
rural <- subset(flat_zstats, flat_zstats$X_mean < .5)

urbanct <- length(urban$Wtrs_ID)
ruralct <- length(rural$Wtrs_ID)

(urbanct/(urbanct+ruralct))*100 #~2% of flat stream reaches are urban 

# Sensitivity test: if define urban reaches as those with >20% urban cover
urban <- subset(flat_zstats, flat_zstats$X_mean >= .2)
rural <- subset(flat_zstats, flat_zstats$X_mean < .2)

urbanct <- length(urban$Wtrs_ID)
ruralct <- length(rural$Wtrs_ID)

(urbanct/(urbanct+ruralct))*100 #~3.5% of flat stream reaches are urban, doesn't make much of a difference so we'll leave it at 50%. 

#Therefore, the overall breakdown of categories is:
# 7% steep streams
# 92.7*.02 = 2% flat urban streams
# 92.7*.98 = 91% flat rural streams



##################################################
# Zonal statistics per canton
#####################################################

# Could also use this to make map of  difference of difference per canton

# decide which set of cantons to use (calculate zonal statistics for both in paper, but only used 2011 set for beneficiaries analysis)
cantones <- cantones11
# cantones <- cantones22

cantones.sf <- st_as_sf(cantones)

# Calculate area of each canton
cantones$area_m <- area(cantones)
cantones$area_km <- cantones$area_m/1000000

# Zonal stats on difference rasters for each service, including normalizing by canton area
cantones$Sed_diff_sum <- exact_extract(sdr_diff, cantones.sf, fun = 'sum')
cantones$Sed_diff_sum_perkm <- round(cantones$Sed_diff_sum/cantones$area_km, 2)

cantones$Sedlo_diff_sum <- exact_extract(sdrlo_diff, cantones.sf, fun = 'sum')
cantones$Sedlo_diff_sum_perkm <- round(cantones$Sedlo_diff_sum/cantones$area_km, 2)

cantones$Sedhi_diff_sum <- exact_extract(sdrhi_diff, cantones.sf, fun = 'sum')
cantones$Sedhi_diff_sum_perkm <- round(cantones$Sedhi_diff_sum/cantones$area_km, 2)


cantones$N_diff_sum <- exact_extract(n_diff, cantones.sf, fun = 'sum')
cantones$N_diff_sum_perkm <- round(cantones$N_diff_sum/cantones$area_km, 2)

cantones$Nlo_diff_sum <- exact_extract(nlo_diff, cantones.sf, fun = 'sum')
cantones$Nlo_diff_sum_perkm <- round(cantones$Nlo_diff_sum/cantones$area_km, 2)

cantones$Nhi_diff_sum <- exact_extract(nhi_diff, cantones.sf, fun = 'sum')
cantones$Nhi_diff_sum_perkm <- round(cantones$Nhi_diff_sum/cantones$area_km, 2)


cantones$P_diff_sum <- exact_extract(p_diff, cantones.sf, fun = 'sum')
cantones$P_diff_sum_perkm <- round(cantones$P_diff_sum/cantones$area_km, 2)

cantones$Plo_diff_sum <- exact_extract(plo_diff, cantones.sf, fun = 'sum')
cantones$Plo_diff_sum_perkm <- round(cantones$Plo_diff_sum/cantones$area_km, 2)

cantones$Phi_diff_sum <- exact_extract(phi_diff, cantones.sf, fun = 'sum')
cantones$Phi_diff_sum_perkm <- round(cantones$Phi_diff_sum/cantones$area_km, 2)


cantones$Carbon_bac_diff_sum <- exact_extract(carbon_bac_diff, cantones.sf, fun = 'sum')
cantones$Carbon_bac_diff_sum_perkm <- round(cantones$Carbon_bac_diff_sum/cantones$area_km, 2)

cantones$Carbon_ipcc_diff_sum <- exact_extract(carbon_ipcc_diff, cantones.sf, fun = 'sum')
cantones$Carbon_ipcc_diff_sum_perkm <- round(cantones$Carbon_ipcc_diff_sum/cantones$area_km, 2)


# Extra accounting of baseline + scenario carbon results per canton for country summing up
cantones$carbon_bac_base_sum <- exact_extract(carbon_bac_base, cantones.sf, fun = 'sum')
cantones$carbon_bac_scen_sum <- exact_extract(carbon_bac_scen, cantones.sf, fun = 'sum')
cantones$carbon_ipcc_base_sum <- exact_extract(carbon_ipcc_base, cantones.sf, fun = 'sum')
cantones$carbon_ipcc_scen_sum <- exact_extract(carbon_ipcc_scen, cantones.sf, fun = 'sum')

# Extra accounting of baseline export for NDR, SDR per canton for measuring scope of pollution
cantones$sdr_base_sum <- exact_extract(sdr_base, cantones.sf, fun = 'sum')
cantones$n_base_sum <- exact_extract(n_base, cantones.sf, fun = 'sum')
cantones$p_base_sum <- exact_extract(p_base, cantones.sf, fun = 'sum')

cantones$sdr_base_sum_perkm <- cantones$sdr_base_sum/cantones$area_km
cantones$n_base_sum_perkm <- cantones$n_base_sum/cantones$area_km
cantones$p_base_sum_perkm <- cantones$p_base_sum/cantones$area_km

#Calculating change in FC per canton
# made forest cover rasters in QGIS using raster calculator and this formula (for all forest cover except plantations, mangroves)
# ("CR_LULC_scenario_10m@1" = 6) * 1 +
#   ("CR_LULC_scenario_10m@1" = 8) * 1 +
#   ("CR_LULC_scenario_10m@1" = 17) * 1 +
#   ("CR_LULC_scenario_10m@1" = 40) * 1 +
#   ("CR_LULC_scenario_10m@1" = 1050) * 1 +
#   ("CR_LULC_scenario_10m@1" = 1060) * 1 +
#   ("CR_LULC_scenario_10m@1" = 1080) * 1 +
#   ("CR_LULC_scenario_10m@1" = 1090) * 1
# subtracted baseline from scenario to get mask of increase in FC
# each pixel with forest is 10x10m, or 100m2, and is currently assigned a value of 1

cantones$fc_diff_m2 <- (exact_extract(fc_diff, cantones.sf, fun = 'sum'))*100
cantones$fc_diff_km2 <- cantones$fc_diff_m2/1000000
cantones$fc_diff_km2_perkm <- round(cantones$fc_diff_km2/cantones$area_km, 4)

cantones$fc_base_m2 <- (exact_extract(fc_base, cantones.sf, fun = 'sum'))*100
cantones$fc_base_km2 <- cantones$fc_base_m2/1000000

cantones$fc_scen_m2 <- (exact_extract(fc_scen, cantones.sf, fun = 'sum'))*100
cantones$fc_scen_km2 <- cantones$fc_scen_m2/1000000


# Calculate change in service per change in FC
cantones$Sed_diff_sum_perkmfor <- cantones$Sed_diff_sum / cantones$fc_diff_km2
cantones$N_diff_sum_perkmfor <- cantones$N_diff_sum / cantones$fc_diff_km2
cantones$P_diff_sum_perkmfor <- cantones$P_diff_sum / cantones$fc_diff_km2
cantones$Carbon_bac_diff_sum_perkmfor <- cantones$Carbon_bac_diff_sum /cantones$fc_diff_km2

# multiply export differences by -1 for easier visualization (we're showing increase in retention, not decrease in export)
cantones$sdpkm_n <- cantones$Sed_diff_sum_perkm*(-1)
cantones$ndpkm_n <- cantones$N_diff_sum_perkm*(-1)
cantones$pdpkm_n <- cantones$P_diff_sum_perkm*(-1)

cantones$sldpkm_n <- cantones$Sedlo_diff_sum_perkm*(-1)
cantones$nldpkm_n <- cantones$Nlo_diff_sum_perkm*(-1)
cantones$pldpkm_n <- cantones$Plo_diff_sum_perkm*(-1)

cantones$shdpkm_n <- cantones$Sedhi_diff_sum_perkm*(-1)
cantones$nhdpkm_n <- cantones$Nhi_diff_sum_perkm*(-1)
cantones$phdpkm_n <- cantones$Phi_diff_sum_perkm*(-1)

cantones$sdpkmf_n <- cantones$Sed_diff_sum_perkmfor*(-1)
cantones$ndpkmf_n <- cantones$N_diff_sum_perkmfor*(-1)
cantones$pdpkmf_n <- cantones$P_diff_sum_perkmfor*(-1)


### keep the below section only if you are calculating for 2022 cantones, and aren't going to do beneficiaries analysis later, where it is repeated. 
# Otherwise, leave commented out

# # Create columns dividing services up into three bins (0-33rd, 33rd-66th, 66th-100th percentile)
# 
# cantones@data$Seddiffsum_perkm_bin <- ifelse((cantones@data$Sed_diff_sum_perkm*-1) < unname(quantile((cantones@data$Sed_diff_sum_perkm*-1), probs=c(0.33))), "1",
#                                               ifelse((cantones@data$Sed_diff_sum_perkm*-1) < unname(quantile((cantones@data$Sed_diff_sum_perkm*-1), probs=c(0.66))), "2",
#                                                      "3"))
# cantones@data$Ndiffsum_perkm_bin <- ifelse((cantones@data$N_diff_sum_perkm*-1) < unname(quantile((cantones@data$N_diff_sum_perkm*-1), probs=c(0.33))), "1",
#                                             ifelse((cantones@data$N_diff_sum_perkm*-1) < unname(quantile((cantones@data$N_diff_sum_perkm*-1), probs=c(0.66))), "2",
#                                                    "3"))
# cantones@data$Pdiffsum_perkm_bin <- ifelse((cantones@data$P_diff_sum_perkm*-1) < unname(quantile((cantones@data$P_diff_sum_perkm*-1), probs=c(0.33))), "1",
#                                             ifelse((cantones@data$P_diff_sum_perkm*-1) < unname(quantile((cantones@data$P_diff_sum_perkm*-1), probs=c(0.66))), "2",
#                                                    "3"))
# cantones@data$Seddiffsum_perfc_bin <- ifelse((cantones@data$Sed_diff_sum_perkmfor*-1) < unname(quantile((cantones@data$Sed_diff_sum_perkmfor*-1), probs=c(0.33))), "1",
#                                               ifelse((cantones@data$Sed_diff_sum_perkmfor*-1) < unname(quantile((cantones@data$Sed_diff_sum_perkmfor*-1), probs=c(0.66))), "2",
#                                                      "3"))
# cantones@data$Ndiffsum_perfc_bin <- ifelse((cantones@data$N_diff_sum_perkmfor*-1) < unname(quantile((cantones@data$N_diff_sum_perkmfor*-1), probs=c(0.33))), "1",
#                                             ifelse((cantones@data$N_diff_sum_perkmfor*-1) < unname(quantile((cantones@data$N_diff_sum_perkmfor*-1), probs=c(0.66))), "2",
#                                                    "3"))
# cantones@data$Pdiffsum_perfc_bin <- ifelse((cantones@data$P_diff_sum_perkmfor*-1) < unname(quantile((cantones@data$P_diff_sum_perkmfor*-1), probs=c(0.33))), "1",
#                                             ifelse((cantones@data$P_diff_sum_perkmfor*-1) < unname(quantile((cantones@data$P_diff_sum_perkmfor*-1), probs=c(0.66))), "2",
#                                                    "3"))
# cantones@data$Sedbase_perkm_bin <- ifelse((cantones@data$sdr_base_sum_perkm) < unname(quantile((cantones@data$sdr_base_sum_perkm), probs=c(0.33))), "1",
#                                            ifelse((cantones@data$sdr_base_sum_perkm) < unname(quantile((cantones@data$sdr_base_sum_perkm), probs=c(0.66))), "2",
#                                                   "3"))
# cantones@data$Nbase_perkm_bin <- ifelse((cantones@data$n_base_sum_perkm) < unname(quantile((cantones@data$n_base_sum_perkm), probs=c(0.33))), "1",
#                                          ifelse((cantones@data$n_base_sum_perkm) < unname(quantile((cantones@data$n_base_sum_perkm), probs=c(0.66))), "2",
#                                                 "3"))
# cantones@data$Pbase_perkm_bin <- ifelse((cantones@data$p_base_sum_perkm) < unname(quantile((cantones@data$p_base_sum_perkm), probs=c(0.33))), "1",
#                                          ifelse((cantones@data$p_base_sum_perkm) < unname(quantile((cantones@data$p_base_sum_perkm), probs=c(0.66))), "2",
#                                                 "3"))
# cantones@data$Sedlodiffsum_perkm_bin <- ifelse((cantones@data$Sedlo_diff_sum_perkm*-1) < unname(quantile((cantones@data$Sedlo_diff_sum_perkm*-1), probs=c(0.33))), "1",
#                                                 ifelse((cantones@data$Sedlo_diff_sum_perkm*-1) < unname(quantile((cantones@data$Sedlo_diff_sum_perkm*-1), probs=c(0.66))), "2",
#                                                        "3"))
# cantones@data$Nlodiffsum_perkm_bin <- ifelse((cantones@data$Nlo_diff_sum_perkm*-1) < unname(quantile((cantones@data$Nlo_diff_sum_perkm*-1), probs=c(0.33))), "1",
#                                               ifelse((cantones@data$Nlo_diff_sum_perkm*-1) < unname(quantile((cantones@data$Nlo_diff_sum_perkm*-1), probs=c(0.66))), "2",
#                                                      "3"))
# cantones@data$Plodiffsum_perkm_bin <- ifelse((cantones@data$Plo_diff_sum_perkm*-1) < unname(quantile((cantones@data$Plo_diff_sum_perkm*-1), probs=c(0.33))), "1",
#                                               ifelse((cantones@data$Plo_diff_sum_perkm*-1) < unname(quantile((cantones@data$Plo_diff_sum_perkm*-1), probs=c(0.66))), "2",
#                                                      "3"))
# cantones@data$Sedhidiffsum_perkm_bin <- ifelse((cantones@data$Sedhi_diff_sum_perkm*-1) < unname(quantile((cantones@data$Sedhi_diff_sum_perkm*-1), probs=c(0.33))), "1",
#                                                 ifelse((cantones@data$Sedhi_diff_sum_perkm*-1) < unname(quantile((cantones@data$Sedhi_diff_sum_perkm*-1), probs=c(0.66))), "2",
#                                                        "3"))
# cantones@data$Nhidiffsum_perkm_bin <- ifelse((cantones@data$Nhi_diff_sum_perkm*-1) < unname(quantile((cantones@data$Nhi_diff_sum_perkm*-1), probs=c(0.33))), "1",
#                                               ifelse((cantones@data$Nhi_diff_sum_perkm*-1) < unname(quantile((cantones@data$Nhi_diff_sum_perkm*-1), probs=c(0.66))), "2",
#                                                      "3"))
# cantones@data$Phidiffsum_perkm_bin <- ifelse((cantones@data$Phi_diff_sum_perkm*-1) < unname(quantile((cantones@data$Phi_diff_sum_perkm*-1), probs=c(0.33))), "1",
#                                               ifelse((cantones@data$Phi_diff_sum_perkm*-1) < unname(quantile((cantones@data$Phi_diff_sum_perkm*-1), probs=c(0.66))), "2",
#                                                      "3"))
# cantones@data$Carbonipccdiffsum_perkm_bin <- ifelse((cantones@data$Carbon_ipcc_diff_sum_perkm*-1) < unname(quantile((cantones@data$Carbon_ipcc_diff_sum_perkm*-1), probs=c(0.33))), "1",
#                                                      ifelse((cantones@data$Carbon_ipcc_diff_sum_perkm*-1) < unname(quantile((cantones@data$Carbon_ipcc_diff_sum_perkm*-1), probs=c(0.66))), "2",
#                                                             "3"))
# cantones@data$Carbonbacdiffsum_perkm_bin <- ifelse((cantones@data$Carbon_bac_diff_sum_perkm*-1) < unname(quantile((cantones@data$Carbon_bac_diff_sum_perkm*-1), probs=c(0.33))), "1",
#                                                     ifelse((cantones@data$Carbon_bac_diff_sum_perkm*-1) < unname(quantile((cantones@data$Carbon_bac_diff_sum_perkm*-1), probs=c(0.66))), "2",
#                                                            "3"))
# 
# # Calculate deviation from median for ES and forest cover
# cantones@data$fcperkm_meddev <- cantones@data$fc_diff_km2_perkm - median(cantones@data$fc_diff_km2_perkm)
# cantones@data$sedperfc_meddev <- cantones@data$sdpkmf_n - median(cantones@data$sdpkmf_n)
# cantones@data$nperfc_meddev <- cantones@data$ndpkmf_n - median(cantones@data$ndpkmf_n)
# cantones@data$pperfc_meddev <- cantones@data$pdpkmf_n  - median(cantones@data$pdpkmf_n)
# cantones@data$sedbaseperkm_meddev <- cantones@data$sdr_base_sum_perkm - median(cantones@data$sdr_base_sum_perkm)
# cantones@data$nbaseperkm_meddev <- cantones@data$n_base_sum_perkm - median(cantones@data$n_base_sum_perkm)
# cantones@data$pbaseperkm_meddev <- cantones@data$p_base_sum_perkm  - median(cantones@data$p_base_sum_perkm)
# 
# 
# # Write out shapefile
# shapefile(cantones, filename="./Figures/Cantones22_stats_30Jul22.shp", overwrite=TRUE)
# # Note that column names in shapefile will be truncated
# 
# # Write a dataframe for plotting, and write out
# canton_stats <- cantones@data
# head(canton_stats)
# write.csv(canton_stats, "./Outputs/canton22_zstats_30Jul22.csv", row.names = FALSE)


#################################################################################################################################
# Calculate baseline, difference, and % change in all services (including uncertainty analyses) with reforestation for whole country
#################################################################################################################################

######### Change in land cover between baseline and scenario

# data cleaning

# add a new row with values 0 for forested buffers in baseline LULC for calculations
baselu_ct2 <- rbind(baselu_ct, c(40, 0, 0))
baselu_ct2
# rename columns for merging
baselu_ct3 <- rename(baselu_ct2, 
                      count_base = count,
                      base_m2 = m2)
scenlu_ct2 <- rename(scenlu_ct, 
                     count_scen = count,
                     scen_m2 = m2)
# merge
lulc_change <- merge(baselu_ct3, scenlu_ct2, by = c("value"))
lulc_change2 <- merge(lulc_names, lulc_change, by = c("value"))

# Calculate change in LULC with reforestation

# Calculate diff in each pixel count btwn baseline and scenario
lulc_change2$count_diff <- lulc_change2$count_scen - lulc_change2$count_base
lulc_change$converted_m2 <- lulc_change2$scen_m2 - lulc_change2$base_m2
# pull number of pixels reforested
refor_pix <- subset(lulc_change2, lulc_change2$value==40)[,c("count_scen")]
# Calculate percent of reforested pixels that were previously each LULC type
lulc_change2$perc_diff <- round((lulc_change2$count_diff/refor_pix)*100, 1)

# Calculate percentage of total landcover class converted in reforesting buffers (as compared to baseline landcover amount)
lulc_change2$perc_tot_conv <- round((lulc_change2$count_diff/lulc_change2$count_base)*-100, 1)
# note this prints out a value of -Inf for forested buffers, because this class didn't exist in the baseline scenario. consider it an NA. 

write.csv(lulc_change2, "./Outputs/LULC_refor_change.csv", row.names = FALSE)

######### Watershed based math (ouput of InVEST, easy to do calculations on)
# It's much faster to do this countrywide math on differences in services on watershed shapefiles than rasters

## Calculate total export


# lo sediment
base_tot_sed_exp <- sum(basesdr_wsheds@data$sed_export)
scenlo_tot_sed_exp <- sum(scenlosdr_wsheds@data$sed_export)

diff_lo_tot_sed_exp <- base_tot_sed_exp - scenlo_tot_sed_exp
percchange_lo_tot_sed_exp <- diff_lo_tot_sed_exp/base_tot_sed_exp

# med sediment
scenmed_tot_sed_exp <- sum(scenmedsdr_wsheds@data$sed_export)

diff_med_tot_sed_exp <- base_tot_sed_exp - scenmed_tot_sed_exp
percchange_med_tot_sed_exp <- diff_med_tot_sed_exp/base_tot_sed_exp

# hi sediment
scenhi_tot_sed_exp <- sum(scenhisdr_wsheds@data$sed_export)

diff_hi_tot_sed_exp <- base_tot_sed_exp - scenhi_tot_sed_exp
percchange_hi_tot_sed_exp <- diff_hi_tot_sed_exp/base_tot_sed_exp


# N lo
base_tot_n_exp <- sum(basendr_wsheds@data$n_exp_tot)
scenlo_tot_n_exp <- sum(scenlondr_wsheds@data$n_exp_tot)

diff_lo_tot_n_exp <- base_tot_n_exp - scenlo_tot_n_exp 
percchange_lo_tot_n_exp <- diff_lo_tot_n_exp/base_tot_n_exp 

# N med
scenmed_tot_n_exp <- sum(scenmedndr_wsheds@data$n_exp_tot)

diff_med_tot_n_exp <- base_tot_n_exp - scenmed_tot_n_exp 
percchange_med_tot_n_exp <- diff_med_tot_n_exp/base_tot_n_exp 

# N hi
scenhi_tot_n_exp <- sum(scenhindr_wsheds@data$n_exp_tot)

diff_hi_tot_n_exp <- base_tot_n_exp - scenhi_tot_n_exp 
percchange_hi_tot_n_exp <- diff_hi_tot_n_exp/base_tot_n_exp 


# P lo
base_tot_p_exp <- sum(basendr_wsheds@data$p_exp_tot)
scenlo_tot_p_exp <- sum(scenlondr_wsheds@data$p_exp_tot)

diff_lo_tot_p_exp <- base_tot_p_exp - scenlo_tot_p_exp 
percchange_lo_tot_p_exp <- diff_lo_tot_p_exp/base_tot_p_exp 

# P med
scenmed_tot_p_exp <- sum(scenmedndr_wsheds@data$p_exp_tot)

diff_med_tot_p_exp <- base_tot_p_exp - scenmed_tot_p_exp 
percchange_med_tot_p_exp <- diff_med_tot_p_exp/base_tot_p_exp 

# P hi
scenhi_tot_p_exp <- sum(scenhindr_wsheds@data$p_exp_tot)

diff_hi_tot_p_exp <- base_tot_p_exp - scenhi_tot_p_exp
percchange_hi_tot_p_exp <- diff_hi_tot_p_exp/base_tot_p_exp


########## Canton-based math


# Carbon

# Baccini carbon run
base_tot_carbon_bac <- sum(cantones$carbon_bac_base_sum)
scen_tot_carbon_bac <- sum(cantones$carbon_bac_scen_sum)
diff_tot_carbon_bac <- sum(cantones$Carbon_bac_diff_sum)
percchange_tot_carbon_bac <- (diff_tot_carbon_bac/base_tot_carbon_bac)*100

diff_tot_carbon_bac #6,955,822
percchange_tot_carbon_bac #1.4%
# Calculate CO2e change
diff_tot_carbon_bac*3.664191096 #25,487,461

# IPCC carbon run
base_tot_carbon_ipcc <- sum(cantones$carbon_ipcc_base_sum)
scen_tot_carbon_ipcc <- sum(cantones$carbon_ipcc_scen_sum)
diff_tot_carbon_ipcc <- scen_tot_carbon_ipcc - base_tot_carbon_ipcc
percchange_tot_carbon_ipcc <- (diff_tot_carbon_ipcc/base_tot_carbon_ipcc)*100

diff_tot_carbon_ipcc #7,888,089 
percchange_tot_carbon_ipcc #1.6% 


# Forest cover
diff_tot_fc <- sum(cantones$fc_diff_km2) #522
scen_tot_fc <- sum(cantones$fc_scen_km2)
base_tot_fc <- sum(cantones$fc_base_km2) #28011
percchange_fc <- (sum(cantones$fc_diff_km2)/sum(cantones$fc_base_km2))*100 #1.89%


# Write a table
CR_totals.df <- data.frame(matrix(ncol=7, nrow=10))
colnames(CR_totals.df) <- c("Names", "Sediment", "Nitrogen", "Phosphorus", "Carbon", "Carbon IPCC", "Forest Cover")
CR_totals.df$Names <- c("Baseline",
                       "Scenario low", "Difference scenario low", "Percent difference scenario low",
                       "Scenario medium", "Difference scenario medium", "Percent difference scenario medium",
                       "Scenario high", "Difference scenario high", "Percent difference scenario high")

CR_totals.df$Sediment <- c(base_tot_sed_exp,
                          scenlo_tot_sed_exp, diff_lo_tot_sed_exp, (percchange_lo_tot_sed_exp)*100,
                          scenmed_tot_sed_exp, diff_med_tot_sed_exp, (percchange_med_tot_sed_exp)*100,
                          scenhi_tot_sed_exp, diff_hi_tot_sed_exp, (percchange_hi_tot_sed_exp)*100)

CR_totals.df$Nitrogen <- c(base_tot_n_exp,
                          scenlo_tot_n_exp, diff_lo_tot_n_exp, (percchange_lo_tot_n_exp)*100,
                          scenmed_tot_n_exp, diff_med_tot_n_exp, (percchange_med_tot_n_exp)*100,
                          scenhi_tot_n_exp, diff_hi_tot_n_exp, (percchange_hi_tot_n_exp)*100)

CR_totals.df$Phosphorus <- c(base_tot_p_exp,
                          scenlo_tot_p_exp, diff_lo_tot_p_exp, (percchange_lo_tot_p_exp)*100,
                          scenmed_tot_p_exp, diff_med_tot_p_exp, (percchange_med_tot_p_exp)*100,
                          scenhi_tot_p_exp, diff_hi_tot_p_exp, (percchange_hi_tot_p_exp)*100)

CR_totals.df$'Carbon' <- c(base_tot_carbon_bac,
                                        NA, NA, NA,
scen_tot_carbon_bac, diff_tot_carbon_bac, percchange_tot_carbon_bac,
                                        NA, NA, NA)

CR_totals.df$'Carbon IPCC' <- c(base_tot_carbon_ipcc,
                           NA, NA, NA,
                           scen_tot_carbon_ipcc, diff_tot_carbon_ipcc, percchange_tot_carbon_ipcc,
                           NA, NA, NA)

CR_totals.df$'Forest Cover' <- c(base_tot_fc,
                                        NA, NA, NA,
                                        scen_tot_fc, diff_tot_fc, percchange_fc,
                                        NA, NA, NA)

write.csv(CR_totals.df, "./Figures/CR_service_totals_14June21.csv")


###############################
# Beneficiaries data clean up
###############################

############### Comment out this section until the visualization section if you are using 2022 canton data, which should not be used with 2011 census beneficiary data ################3

### Water source

View(water)

# Get rid of empty rows
water2 <- subset(water, complete.cases(water)==TRUE)
head(water2)
dim(water2)
dim(water)

# Replace dash character standing in for 0 with zero
water2$Viviendas <- gsub("-", "0", water2$Viviendas)
water2$Ocupantes <- gsub("-", "0", water2$Ocupantes)
head(water2)

# Get rid of spaces in numbers
water2$Viviendas <- gsub(" ", "", water2$Viviendas)
water2$Ocupantes <- gsub(" ", "", water2$Ocupantes)
head(water2)

# Parse columns as numbers
water2$Viviendas <- as.numeric(water2$Viviendas)
water2$Ocupantes <- as.numeric(water2$Ocupantes)
head(water2)

# Calculate % of people getting water from river or creek in each canton 
# Calculate total people in each canton
water3 <- water2  %>%
  group_by(Canton) %>%
  summarize(total_Ocupantes = sum(Ocupantes))
head(water3)
dim(water3)

Rios <- subset(water2, water2$Fuente=="Río o quebrada") # this translates to "source = river or creek"
Rios2 <- Rios[,c(1,4)]
colnames(Rios2) <- c("Canton", "Ocupantes_rio")
Rios2

water4 <- merge(water3, Rios2, by = c("Canton"))

water4$Percent_river <- (water4$Ocupantes_rio/water4$total_Ocupantes)*100
water4

# Quick check on results
length(unique(water4$Canton)) #81 cantons
# Do population total match original census datasheet?
sum(water4$total_Ocupantes) #4,283,063 matches sheet
sum(water4$Ocupantes_rio) #107,423 matches sheet!
# checked in sheet by adding up categories, categories are non-overlapping so they should sum up to total!

# Quick visualization
hist(water4$Percent_river) # Most cantons are below 20% ppl dependent on rivers for water

# Name matching stuff to merge this with canton shapefile
water4$Canton
# Aguirre is now named Quepos, check shapefile to match
# Valverde Vega is now named Sarchi

# Check cantones shapefile to check names, see what's missing
dim(cantones@data) # just 81!
setdiff(water4$Canton, str_to_title(tolower(cantones@data$NOM_CANT_1)))
# difference is in accents and capitalization
# old names remain the same across lists

#Get rid of accents in datatable, and capitalize every word
water4$Canton <- str_to_title(stri_trans_general(str = water4$Canton, id = "Latin-ASCII"))
water4$Canton

#Get rid of all caps in shapefile, and capitalize every word
cantones@data$NOM_CANT_1 <- str_to_title(tolower(cantones@data$NOM_CANT_1))
cantones@data$NOM_CANT_1

#Check for match
setdiff(water4$Canton, cantones@data$NOM_CANT_1)
# 2 mismatches, just fix by hand
cantones@data$NOM_CANT_1 <- ifelse(cantones@data$NOM_CANT_1=="Leon Cortes", "Leon Cortes Castro",
                                   ifelse(cantones@data$NOM_CANT_1=="Cañas", "Canas", cantones@data$NOM_CANT_1))
cantones@data$NOM_CANT_1
setdiff(water4$Canton, cantones@data$NOM_CANT_1) # all match

# Rename columns for eventual merge
head(water4)
colnames(water4) <- c("NOM_CANT_1", "total_Ocupantes", "Ocupantes_rio", "Percent_river")


### Poverty

View(pov)

# get rid of decimal point after poor households
pov$'Hogares Pobres' <- round(pov$`Hogares Pobres`, digits=0)

# Calculate % poor households per canton
pov$Hogares_pobres_per <- (pov$`Hogares Pobres`/pov$Total)*100

# get rid of extra columns
pov2 <- pov[,c("Cantón", "Hogares_pobres_per",  "Hogares Pobres")]
head(pov2)

# get rid of number before canton name
pov2$Cantón <- gsub("[[:digit:]][[:digit:]][[:digit:]] ","",pov2$Cantón)
pov2$Cantón

# make canton name match catones@data

#Get rid of accents, get rid of all caps, and capitalize every word
pov2$Cantón <- str_to_title(tolower(stri_trans_general(str = pov2$Cantón, id = "Latin-ASCII")))
pov2$Cantón

setdiff(pov2$Cantón, cantones@data$NOM_CANT_1)
setdiff(cantones@data$NOM_CANT_1, pov2$Cantón)
# alfaro ruiz has been renamed zarcero
# Leon cortes -> Leon cortes castro

# Fix by hand
pov2$Cantón<- ifelse(pov2$Cantón=="Leon Cortes", "Leon Cortes Castro",
                                   ifelse(pov2$Cantón=="Alfaro Ruiz", "Zarcero", pov2$Cantón))
setdiff(pov2$Cantón, cantones@data$NOM_CANT_1)

# Rename columns for eventual merge
pov3 <- pov2
names(pov3) <- c("NOM_CANT_1", "Hogares_pobres_per", "Hogares_pobres")


#### Race

View(race)

# Get rid of empty rows
race2 <- subset(race, complete.cases(race)==TRUE)
head(race2)
dim(race2)

# Match canton names with stats
#Get rid of accents, get rid of all caps, and capitalize every word
race2$Cantón <- str_to_title(stri_trans_general(str = race2$Cantón, id = "Latin-ASCII"))
race2$Cantón

setdiff(race2$Cantón, cantones@data$NOM_CANT_1)
setdiff(cantones@data$NOM_CANT_1, race2$Cantón)
# perfect match!

# Rename columns for eventual merge
race3 <- race2
names(race3) <- c("NOM_CANT_1", "Porcentaje_indigena", "Porcentaje_negra", "Porcentaje_mulata", "Porcentaje_china", "Porcentaje_blanca_mestiza")


#### Women-led households

View(wom)

# Get rid of empty rows
wom2 <- subset(wom, complete.cases(wom)==TRUE)
head(wom2)
dim(wom2)

# Calculate % of women-led households in each canton (hogares = households)
wom2$Percent_womenled <- (wom2$`Jefatura femenina`/wom2$`Total de hogares`)*100
wom2

# Quick check on results
length(unique(wom2$`Provincia, cantón y distrito`)) #81 cantons
# Do population total match original census datasheet?
sum(wom2$`Total de hogares`) #matches sheet
sum(wom2$`Jefatura femenina`) #matches sheet!

# Quick visualization
hist(wom2$Percent_womenled) # Most cantons are around 25%

# Name matching stuff

# #Get rid of accents in datatable, and capitalize every word
wom2$`Provincia, cantón y distrito` <- str_to_title(stri_trans_general(str = wom2$`Provincia, cantón y distrito`, id = "Latin-ASCII"))
wom2$`Provincia, cantón y distrito`

#Check for match
setdiff(wom2$`Provincia, cantón y distrito`, cantones@data$NOM_CANT_1)

# Rename columns for eventual merge
head(wom2)
colnames(wom2) <- c("NOM_CANT_1", "total_hogarges", "Jefatura_compartida", "Jefatura_masculina","Jefatura_femenina", "Percent_womenled")


#########################################
# Combine demographic and service data
#######################################
# Combine into shapefile to write out for maps
# Combine into dataframe to plot from

# Add demographic data to cantones shapefile
cantones2 <- cantones
cantones2@data <- merge(cantones2@data, water4, by = c("NOM_CANT_1"))
cantones3 <- cantones2
cantones3@data <- merge(cantones3@data, pov3, by = c("NOM_CANT_1"))
cantones4 <- cantones3
cantones4@data <- merge(cantones4@data, race3, by = c("NOM_CANT_1"))
cantones4@data <- merge(cantones4@data, wom2, by=c("NOM_CANT_1"))

dim(cantones4@data)
head(cantones4@data)

### Add water need x service increase categories for bivariate plotting and mapping

# Normalize water need by area (services already normalized)
cantones4@data$Ocupantes_rio_km <- cantones4@data$Ocupantes_rio/cantones4@data$area_km

# Create columns dividing water need and hydrological services up into three bins (0-33rd, 33rd-66th, 66th-100th percentile)
cantones4@data$Waterneed_perkm_bin <- ifelse(cantones4@data$Ocupantes_rio_km < unname(quantile((cantones4@data$Ocupantes_rio_km), probs=c(0.33))), "A",
                                             ifelse(cantones4@data$Ocupantes_rio_km < unname(quantile((cantones4@data$Ocupantes_rio_km), probs=c(0.66))), "B",
                                             "C"))
cantones4@data$Seddiffsum_perkm_bin <- ifelse((cantones4@data$Sed_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Sed_diff_sum_perkm*-1), probs=c(0.33))), "1",
                                             ifelse((cantones4@data$Sed_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Sed_diff_sum_perkm*-1), probs=c(0.66))), "2",
                                                    "3"))
cantones4@data$Ndiffsum_perkm_bin <- ifelse((cantones4@data$N_diff_sum_perkm*-1) < unname(quantile((cantones4@data$N_diff_sum_perkm*-1), probs=c(0.33))), "1",
                                              ifelse((cantones4@data$N_diff_sum_perkm*-1) < unname(quantile((cantones4@data$N_diff_sum_perkm*-1), probs=c(0.66))), "2",
                                                     "3"))
cantones4@data$Pdiffsum_perkm_bin <- ifelse((cantones4@data$P_diff_sum_perkm*-1) < unname(quantile((cantones4@data$P_diff_sum_perkm*-1), probs=c(0.33))), "1",
                                              ifelse((cantones4@data$P_diff_sum_perkm*-1) < unname(quantile((cantones4@data$P_diff_sum_perkm*-1), probs=c(0.66))), "2",
                                                     "3"))
cantones4@data$Seddiffsum_perfc_bin <- ifelse((cantones4@data$Sed_diff_sum_perkmfor*-1) < unname(quantile((cantones4@data$Sed_diff_sum_perkmfor*-1), probs=c(0.33))), "1",
                                              ifelse((cantones4@data$Sed_diff_sum_perkmfor*-1) < unname(quantile((cantones4@data$Sed_diff_sum_perkmfor*-1), probs=c(0.66))), "2",
                                                     "3"))
cantones4@data$Ndiffsum_perfc_bin <- ifelse((cantones4@data$N_diff_sum_perkmfor*-1) < unname(quantile((cantones4@data$N_diff_sum_perkmfor*-1), probs=c(0.33))), "1",
                                            ifelse((cantones4@data$N_diff_sum_perkmfor*-1) < unname(quantile((cantones4@data$N_diff_sum_perkmfor*-1), probs=c(0.66))), "2",
                                                   "3"))
cantones4@data$Pdiffsum_perfc_bin <- ifelse((cantones4@data$P_diff_sum_perkmfor*-1) < unname(quantile((cantones4@data$P_diff_sum_perkmfor*-1), probs=c(0.33))), "1",
                                            ifelse((cantones4@data$P_diff_sum_perkmfor*-1) < unname(quantile((cantones4@data$P_diff_sum_perkmfor*-1), probs=c(0.66))), "2",
                                                   "3"))
cantones4@data$Sedbase_perkm_bin <- ifelse((cantones4@data$sdr_base_sum_perkm) < unname(quantile((cantones4@data$sdr_base_sum_perkm), probs=c(0.33))), "1",
                                                                                            ifelse((cantones4@data$sdr_base_sum_perkm) < unname(quantile((cantones4@data$sdr_base_sum_perkm), probs=c(0.66))), "2",
                                                                                                   "3"))
cantones4@data$Nbase_perkm_bin <- ifelse((cantones4@data$n_base_sum_perkm) < unname(quantile((cantones4@data$n_base_sum_perkm), probs=c(0.33))), "1",
                                         ifelse((cantones4@data$n_base_sum_perkm) < unname(quantile((cantones4@data$n_base_sum_perkm), probs=c(0.66))), "2",
                                                "3"))
cantones4@data$Pbase_perkm_bin <- ifelse((cantones4@data$p_base_sum_perkm) < unname(quantile((cantones4@data$p_base_sum_perkm), probs=c(0.33))), "1",
                                         ifelse((cantones4@data$p_base_sum_perkm) < unname(quantile((cantones4@data$p_base_sum_perkm), probs=c(0.66))), "2",
                                                "3"))
cantones4@data$Sedlodiffsum_perkm_bin <- ifelse((cantones4@data$Sedlo_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Sedlo_diff_sum_perkm*-1), probs=c(0.33))), "1",
                                             ifelse((cantones4@data$Sedlo_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Sedlo_diff_sum_perkm*-1), probs=c(0.66))), "2",
                                                    "3"))
cantones4@data$Nlodiffsum_perkm_bin <- ifelse((cantones4@data$Nlo_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Nlo_diff_sum_perkm*-1), probs=c(0.33))), "1",
                                              ifelse((cantones4@data$Nlo_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Nlo_diff_sum_perkm*-1), probs=c(0.66))), "2",
                                                     "3"))
cantones4@data$Plodiffsum_perkm_bin <- ifelse((cantones4@data$Plo_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Plo_diff_sum_perkm*-1), probs=c(0.33))), "1",
                                              ifelse((cantones4@data$Plo_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Plo_diff_sum_perkm*-1), probs=c(0.66))), "2",
                                                     "3"))
cantones4@data$Sedhidiffsum_perkm_bin <- ifelse((cantones4@data$Sedhi_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Sedhi_diff_sum_perkm*-1), probs=c(0.33))), "1",
                                             ifelse((cantones4@data$Sedhi_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Sedhi_diff_sum_perkm*-1), probs=c(0.66))), "2",
                                                    "3"))
cantones4@data$Nhidiffsum_perkm_bin <- ifelse((cantones4@data$Nhi_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Nhi_diff_sum_perkm*-1), probs=c(0.33))), "1",
                                              ifelse((cantones4@data$Nhi_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Nhi_diff_sum_perkm*-1), probs=c(0.66))), "2",
                                                     "3"))
cantones4@data$Phidiffsum_perkm_bin <- ifelse((cantones4@data$Phi_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Phi_diff_sum_perkm*-1), probs=c(0.33))), "1",
                                              ifelse((cantones4@data$Phi_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Phi_diff_sum_perkm*-1), probs=c(0.66))), "2",
                                                     "3"))
cantones4@data$Carbonipccdiffsum_perkm_bin <- ifelse((cantones4@data$Carbon_ipcc_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Carbon_ipcc_diff_sum_perkm*-1), probs=c(0.33))), "1",
                                             ifelse((cantones4@data$Carbon_ipcc_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Carbon_ipcc_diff_sum_perkm*-1), probs=c(0.66))), "2",
                                                    "3"))
cantones4@data$Carbonbacdiffsum_perkm_bin <- ifelse((cantones4@data$Carbon_bac_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Carbon_bac_diff_sum_perkm*-1), probs=c(0.33))), "1",
                                             ifelse((cantones4@data$Carbon_bac_diff_sum_perkm*-1) < unname(quantile((cantones4@data$Carbon_bac_diff_sum_perkm*-1), probs=c(0.66))), "2",
                                                    "3"))


# Concatenate need and service columns for categories
cantones4@data$Sedxwater <- paste(cantones4@data$Waterneed_perkm_bin, cantones4@data$Seddiffsum_perkm_bin, sep="")
cantones4@data$Nxwater <- paste(cantones4@data$Waterneed_perkm_bin, cantones4@data$Ndiffsum_perkm_bin, sep="")
cantones4@data$Pxwater <- paste(cantones4@data$Waterneed_perkm_bin, cantones4@data$Pdiffsum_perkm_bin, sep="")
cantones4@data$Sedfcxwater <- paste(cantones4@data$Waterneed_perkm_bin, cantones4@data$Seddiffsum_perfc_bin, sep="")
cantones4@data$Nfcxwater <- paste(cantones4@data$Waterneed_perkm_bin, cantones4@data$Ndiffsum_perfc_bin, sep="")
cantones4@data$Pfcxwater <- paste(cantones4@data$Waterneed_perkm_bin, cantones4@data$Pdiffsum_perfc_bin, sep="")
cantones4@data$Sedloadxwater <- paste(cantones4@data$Waterneed_perkm_bin, cantones4@data$Sedbase_perkm_bin, sep="")
cantones4@data$Nloadxwater <- paste(cantones4@data$Waterneed_perkm_bin, cantones4@data$Nbase_perkm_bin, sep="")
cantones4@data$Ploadxwater <- paste(cantones4@data$Waterneed_perkm_bin, cantones4@data$Pbase_perkm_bin, sep="")

# Calculate deviation from median for sersvices, demographics and forest cover
cantones4@data$Povper_meddev <- cantones4@data$Hogares_pobres_per - median(cantones4@data$Hogares_pobres_per)
cantones4@data$Ind_meddev <- cantones4@data$Porcentaje_indigena - median(cantones4@data$Porcentaje_indigena)
cantones4@data$Bl_meddev <- cantones4@data$Porcentaje_negra - median(cantones4@data$Porcentaje_negra)
cantones4@data$Wh_meddev <- cantones4@data$Porcentaje_blanca_mestiza - median(cantones4@data$Porcentaje_blanca_mestiza)
cantones4@data$fcperkm_meddev <- cantones4@data$fc_diff_km2_perkm - median(cantones4@data$fc_diff_km2_perkm)
cantones4@data$womper_meddev<- cantones4@data$Percent_womenled - median(cantones4@data$Percent_womenled)
cantones4@data$sedperfc_meddev <- cantones4@data$sdpkmf_n - median(cantones4@data$sdpkmf_n)
cantones4@data$nperfc_meddev <- cantones4@data$ndpkmf_n - median(cantones4@data$ndpkmf_n)
cantones4@data$pperfc_meddev <- cantones4@data$pdpkmf_n  - median(cantones4@data$pdpkmf_n)
cantones4@data$sedbaseperkm_meddev <- cantones4@data$sdr_base_sum_perkm - median(cantones4@data$sdr_base_sum_perkm)
cantones4@data$nbaseperkm_meddev <- cantones4@data$n_base_sum_perkm - median(cantones4@data$n_base_sum_perkm)
cantones4@data$pbaseperkm_meddev <- cantones4@data$p_base_sum_perkm  - median(cantones4@data$p_base_sum_perkm)


# Write out shapefile
shapefile(cantones4, filename="./Figures/Cantones_stats_5Aug22.shp", overwrite=TRUE)
# column names will be truncated

# Write a dataframe for plotting, and write out
canton_stats <- cantones4@data
head(canton_stats)
write.csv(canton_stats, "./Outputs/canton_zstats_5Aug22.csv", row.names = FALSE)

# use these results to make plot, graph

####################### Read in dataframe for visualization if desired to save calculation time ################################
# Comment this out if recalculating
canton_stats <- read.csv( "./Outputs/canton_zstats_5Aug22.csv")


############################################################################
# Calculations on cantons with highest water need, highest service increase
#############################################################################

# What percent of cantons fall into each service/need bin?
sedxwater.ct <- plyr::count(canton_stats, 'Sedxwater')
sedxwater.ct$percent <- (sedxwater.ct$freq/81)*100
sedxwater.ct # 20% fall into highest need, highest service!, and only 2% into highest need, lowest service

Nxwater.ct <- plyr::count(canton_stats, 'Nxwater')
Nxwater.ct$percent <- (Nxwater.ct$freq/81)*100
Nxwater.ct # 17% fall into highest need, highest service!, and only 5% into highest need, lowest service

Pxwater.ct <- plyr::count(canton_stats, 'Pxwater')
Pxwater.ct$percent <- (Pxwater.ct$freq/81)*100
Pxwater.ct # 17% call into highest need, highest service!, and only 5% into highest need, lowest service
# not quite the same as N, but almost

# Take a look at only the cantons falling into highest need, highest service for N and Sed (P ommitted from this exploration because almost the same as N)
Sed.priority <- subset(canton_stats, canton_stats$Sedxwater=="C3")
N.priority <- subset(canton_stats, canton_stats$Nxwater=="C3")

Sed.priority
N.priority

# Calculate median for df, also write out
canton_medians <- apply(canton_stats, 2, median)
median(canton_stats$Sed_diff_sum_perkm) # checks out

# Write these out to check out
write.csv(canton_medians, "./Figures/canton_stats_medians.csv")
write.csv(Sed.priority, "./Figures/Sed_priority_canton_stats.csv")
write.csv(N.priority, "./Figures/N_priority_canton_stats.csv")

# Look at # of people dependent on river water who are in in high priority cantons
sum(Sed.priority$Ocupantes_rio) #48164
sum(Sed.priority$Ocupantes_rio)/sum(canton_stats$Ocupantes_rio)*100 #44%

sum(N.priority$Ocupantes_rio) #15446
sum(N.priority$Ocupantes_rio)/sum(canton_stats$Ocupantes_rio)*100 #14%

# Why this discrepancy? Probably because we're looking at people/area. 
mean(Sed.priority$area_km) #543
mean(N.priority$area_km) #196
mean(Sed.priority$Ocupantes_rio) #2833
mean(N.priority$Ocupantes_rio) #1103

# Look at # people dependent on river water in low ES increase cantons
Sed.low <- subset(canton_stats, canton_stats$Sedxwater=="C1")
N.low <- subset(canton_stats, canton_stats$Nxwater=="C1")

sum(Sed.low$Ocupantes_rio) #10401
sum(Sed.low$Ocupantes_rio)/sum(canton_stats$Ocupantes_rio)*100 #10%

sum(N.low$Ocupantes_rio) #12935
sum(N.low$Ocupantes_rio)/sum(canton_stats$Ocupantes_rio)*100 #12%


# And look at # of people dep on river water if we pool all high priority cantons (prioritize every C3) 
all.priority <- subset(canton_stats, canton_stats$Sedxwater=="C3" | canton_stats$Nxwater=="C3")
dim(all.priority) # 20 cantons
all.priority # one C1

sum(all.priority$Ocupantes_rio) #50679
sum(all.priority$Ocupantes_rio)/sum(canton_stats$Ocupantes_rio)*100 #47%


# And look at # of people dep on river water if we pool all C1 cantons (assumming none are C3 and prioritized otherwise)
all.low <- subset(canton_stats, canton_stats$Sedxwater=="C1" | canton_stats$Nxwater=="C1")
dim(all.low) # 4 cantons
all.low # all C1 for N and P, one C3 for Sed

all.low2 <- subset(all.low, !(all.low$Sedxwater=="C3"))
dim(all.low2)
all.low2

sum(all.low2$Ocupantes_rio) #11201
sum(all.low2$Ocupantes_rio)/sum(canton_stats$Ocupantes_rio)*100 #10%


#########################################
# Indigenous territories analysis
######################################

#################### Can be commented out to save time, uncomment if recalculating


# zonal statistics for levels of service change in each indigenous territory
# Calculating stats normalized by territory area and amount of forest cover added
ind_terr.sf <- st_as_sf(ind_terr)
ind_terr$area_m <- area(ind_terr)
ind_terr$area_km <- ind_terr$area_m/1000000
ind_terr$fc_diff_km2 <- ((exact_extract(fc_diff, ind_terr.sf, fun = 'sum'))*100) /1000000

ind_terr$Sed_diff_sum <- exact_extract(sdr_diff, ind_terr.sf, fun = 'sum')
ind_terr$Sed_diff_sum_perkm <- round(ind_terr$Sed_diff_sum/ind_terr$area_km, 2)
ind_terr$Sed_diff_sum_perkmfc <- round(ind_terr$Sed_diff_sum/ind_terr$fc_diff_km2, 2)

ind_terr$N_diff_sum <- exact_extract(n_diff, ind_terr.sf, fun = 'sum')
ind_terr$N_diff_sum_perkm <- round(ind_terr$N_diff_sum/ind_terr$area_km, 2)
ind_terr$N_diff_sum_perkmfc <- round(ind_terr$N_diff_sum/ind_terr$fc_diff_km2, 2)

ind_terr$P_diff_sum <- exact_extract(p_diff, ind_terr.sf, fun = 'sum')
ind_terr$P_diff_sum_perkm <- round(ind_terr$P_diff_sum/ind_terr$area_km, 2)
ind_terr$P_diff_sum_perkmfc <- round(ind_terr$P_diff_sum/ind_terr$fc_diff_km2, 2)

ind_terr$Carbon_bac_diff_sum <- exact_extract(carbon_bac_diff, ind_terr.sf, fun = 'sum')
ind_terr$Carbon_bac_diff_sum_perkm <- round(ind_terr$Carbon_bac_diff_sum/ind_terr$area_km, 2)
ind_terr$Carbon_bac_diff_sum_perkmfc <- round(ind_terr$Carbon_bac_diff_sum/ind_terr$fc_diff_km2, 2)

ind_terr$Sed_load_sum <- exact_extract(sdr_base, ind_terr.sf, fun = 'sum')
ind_terr$Sed_load_sum_perkm <- round(ind_terr$Sed_load_sum/ind_terr$area_km, 2)

ind_terr$N_load_sum <- exact_extract(n_base, ind_terr.sf, fun = 'sum')
ind_terr$N_load_sum_perkm <- round(ind_terr$N_load_sum/ind_terr$area_km, 2)

ind_terr$P_load_sum <- exact_extract(p_base, ind_terr.sf, fun = 'sum')
ind_terr$P_load_sum_perkm <- round(ind_terr$P_load_sum/ind_terr$area_km, 2)

# ind_terr$Carbon_ipcc_diff_sum <- exact_extract(carbon_ipcc_diff, ind_terr.sf, fun = 'sum')
# ind_terr$Carbon_ipcc_diff_sum_perkm <- round(ind_terr$Carbon_ipcc_diff_sum/ind_terr$area_km, 2)
# ind_terr$Carbon_ipcc_diff_sum_perkmfc <- round(ind_terr$Carbon_ipcc_diff_sum/ind_terr$fc_diff_km2, 2)

# compare to zstats for levels of service change in  rest of non indig territory in the country (made by vector masking)
non_ind_terr.sf <- st_as_sf(non_ind_terr)
non_ind_terr$area_m <- area(non_ind_terr)
non_ind_terr$area_km <- non_ind_terr$area_m/1000000
non_ind_terr$fc_diff_km2 <- ((exact_extract(fc_diff, non_ind_terr.sf, fun = 'sum'))*100) /1000000

non_ind_terr$Sed_diff_sum <- exact_extract(sdr_diff, non_ind_terr.sf, fun = 'sum')
non_ind_terr$Sed_diff_sum_perkm <- round(non_ind_terr$Sed_diff_sum/non_ind_terr$area_km, 2)
non_ind_terr$Sed_diff_sum_perkmfc <- round(non_ind_terr$Sed_diff_sum/non_ind_terr$fc_diff_km2, 2)

non_ind_terr$N_diff_sum <- exact_extract(n_diff, non_ind_terr.sf, fun = 'sum')
non_ind_terr$N_diff_sum_perkm <- round(non_ind_terr$N_diff_sum/non_ind_terr$area_km, 2)
non_ind_terr$N_diff_sum_perkmfc <- round(non_ind_terr$N_diff_sum/non_ind_terr$fc_diff_km2, 2)

non_ind_terr$P_diff_sum <- exact_extract(p_diff, non_ind_terr.sf, fun = 'sum')
non_ind_terr$P_diff_sum_perkm <- round(non_ind_terr$P_diff_sum/non_ind_terr$area_km, 2)
non_ind_terr$P_diff_sum_perkmfc <- round(non_ind_terr$P_diff_sum/non_ind_terr$fc_diff_km2, 2)

non_ind_terr$Carbon_bac_diff_sum <- exact_extract(carbon_bac_diff, non_ind_terr.sf, fun = 'sum')
non_ind_terr$Carbon_bac_diff_sum_perkm <- round(non_ind_terr$Carbon_bac_diff_sum/non_ind_terr$area_km, 2)
non_ind_terr$Carbon_bac_diff_sum_perkmfc <- round(non_ind_terr$Carbon_bac_diff_sum/non_ind_terr$fc_diff_km2, 2)

non_ind_terr$Sed_load_sum <- exact_extract(sdr_base, non_ind_terr.sf, fun = 'sum')
non_ind_terr$Sed_load_sum_perkm <- round(non_ind_terr$Sed_load_sum/non_ind_terr$area_km, 2)

non_ind_terr$N_load_sum <- exact_extract(n_base, non_ind_terr.sf, fun = 'sum')
non_ind_terr$N_load_sum_perkm <- round(non_ind_terr$N_load_sum/non_ind_terr$area_km, 2)

non_ind_terr$P_load_sum <- exact_extract(p_base, non_ind_terr.sf, fun = 'sum')
non_ind_terr$P_load_sum_perkm <- round(non_ind_terr$P_load_sum/non_ind_terr$area_km, 2)


# non_ind_terr$Carbon_ipcc_diff_sum <- exact_extract(Carbon_ipcc_diff, non_ind_terr.sf, fun = 'sum')
# non_ind_terr$Carbon_ipcc_diff_sum_perkm <- round(non_ind_terr$Carbon_ipcc_diff_sum/non_ind_terr$area_km, 2)
# non_ind_terr$Carbon_ipcc_diff_sum_perkmfc <- round(non_ind_terr$Carbon_ipcc_diff_sum/non_ind_terr$fc_diff_km2, 2)

# take a look
head(ind_terr)
non_ind_terr@data

# merge together
ind_terr.df <- ind_terr@data
non_ind_terr.df <- non_ind_terr@data
colnames(non_ind_terr.df)[1] <- "nombre"
non_ind_terr.df$nombre <- "Non-indigenous land"
non_ind_terr.df$indig_land <- 0
ind_terr.df$indig_land <- 1
names <- names(non_ind_terr.df)
ind_mer <- merge(ind_terr.df, non_ind_terr.df, by = names, all.y=TRUE, all.x=TRUE)
head(ind_mer)

# Add identity column for graphing
ind_mer$indig_land_bin <- ifelse(ind_mer$indig_land==0,"n","y")
head(ind_mer)

# Add an abbreviation column for names for y axis labels
ind_mer$abb <- abbreviate(gsub("[()]", "", ind_mer$nombre), minlength=5, strict=FALSE, method="both.sides")
head(ind_mer)
ind_mer$abb

# write out
write.csv(ind_mer, "./Outputs/Indigenous_territories_servicechange.csv")

# Read in dataframe to save calculation time
# Comment this out if recalculating
ind_mer <- read.csv( "./Outputs/Indigenous_territories_servicechange.csv")



###################################################################### Visualization #################################################################################################

###################################
# River dependency
#####################################

# Scatterplot of water need vs increase in service, to match map figure
# points colored with same bivariate color scheme as map
# Tertiles drawn onto plot to delineate priority cantons

### set up bivariate color palette for legend
# For now, just print out and add labels in editor

pdf("./Figures/Bivariate_palette_legend.pdf")
bivcol(stevens.bluered)
dev.off()

#### multiplot with quantiles drawn on graph
# Both statistics normalized by canton area and log-transformed
# No canton labels, but could add them. 

# calculate the lower bound of the  top quantile with the following function: unname(quantile(inputvector, probs=c(0.75)))
# draw this line onto ggplot on x and y axis for both vars

s2 <- ggplot(data = canton_stats, aes(x = log10((Ocupantes_rio_km)+1), y=log10((Sed_diff_sum_perkm*-1)+1), color=Sedxwater)) +
  geom_hline(yintercept = unname(quantile(log10((canton_stats$Sed_diff_sum_perkm*-1)+1), probs=c(0.66))), color="lightgrey") +
  geom_vline(xintercept = unname(quantile(log10((canton_stats$Ocupantes_rio_km)+1), probs=c(0.66))), color="lightgrey") +
  geom_point() +
  scale_color_manual(values=c("#e8e8e8", "#b0d5df", "#64acbe", "#e4acac", "#ad9ea5", "#627f8c", "#c85a5a", "#985356", "#574249")) +
  # geom_text(aes(label=NOM_CANT_1), hjust=0, vjust=0) +
  labs(x="", y="Log normalized increase in sediment retention with reforestation (tons/yr/km)", title="") +
  theme_classic() +
  theme(legend.position = "none") 

n2 <- ggplot(data = canton_stats, aes(x = log10((Ocupantes_rio_km)+1), y=log10((N_diff_sum_perkm*-1)+1), color=Nxwater)) +
  geom_hline(yintercept = unname(quantile(log10((canton_stats$N_diff_sum_perkm*-1)+1), probs=c(0.66))), color="lightgrey") +
  geom_vline(xintercept = unname(quantile(log10((canton_stats$Ocupantes_rio_km)+1), probs=c(0.66))), color="lightgrey") +
  geom_point() +
  scale_color_manual(values=c("#e8e8e8", "#b0d5df", "#64acbe", "#e4acac", "#ad9ea5", "#627f8c", "#c85a5a", "#985356", "#574249")) +
  # geom_text(aes(label=NOM_CANT_1), hjust=0, vjust=0) +
  labs(x="", y="Log normalized increase in N retention with reforestation (kg/yr/km)", title="") +
  theme_classic() +
  theme(legend.position = "none") 

p2 <- ggplot(data = canton_stats, aes(x = log10((Ocupantes_rio_km)+1), y=log10((P_diff_sum_perkm*-1)+1), color=Pxwater)) +
  geom_hline(yintercept = unname(quantile(log10((canton_stats$P_diff_sum_perkm*-1)+1), probs=c(0.66))), color="lightgrey") +
  geom_vline(xintercept = unname(quantile(log10((canton_stats$Ocupantes_rio_km)+1), probs=c(0.66))), color="lightgrey") +
  geom_point() +
  scale_color_manual(values=c("#e8e8e8", "#b0d5df", "#64acbe", "#e4acac", "#ad9ea5", "#627f8c", "#c85a5a", "#985356", "#574249")) +
  # geom_text(aes(label=NOM_CANT_1), hjust=0, vjust=0) +
  labs(x="Log of river-dependent population per area (pop/km)", y="Log normalized increase in P retention with reforestation (kg/yr/km)") +
  theme_classic() +
  theme(legend.position = "none") 

# Clean multiplot
pdf("./Figures/River_dep_vs_export_bivariate_quantiles_log.pdf", height = 18, width = 6)
multiplot(s2, n2, p2, cols=1)
dev.off()

##########################################
# Equity and priority cantons
#########################################

# Reformat dataframe for easier plotting
# Get rid of unneeded columns
canton_stats.med <- canton_stats[,c("NOM_CANT_1", "Sedxwater", "Nxwater", "Pxwater", 
                                    "Povper_meddev", "Ind_meddev", "womper_meddev")]
canton_stats.vul <- canton_stats[,c("NOM_CANT_1", "Sedxwater", "Nxwater", "Pxwater", 
                                    "Hogares_pobres_per", "Porcentaje_indigena", "Percent_womenled")]
dim(canton_stats.med)
head(canton_stats.med)

# Melt
med_melt <- melt(canton_stats.med, id.vars = c("NOM_CANT_1", "Sedxwater", "Nxwater", "Pxwater"))
vul_melt <- melt(canton_stats.vul, id.vars = c("NOM_CANT_1", "Sedxwater", "Nxwater", "Pxwater"))
head(med_melt)
head(vul_melt)

# Create new column for each sed and n/p with C3 or other designation
vul_melt$Sed_priority <- ifelse(vul_melt$Sedxwater=="C3", "Priority", "Other")
vul_melt$NP_priority <- ifelse(vul_melt$Nxwater=="C3", "Priority", "Other")
head(vul_melt)

### Raincloud plots! 
# link to reference: https://neuroconscience.wordpress.com/2018/03/15/introducing-raincloud-plots/

# set theme:
raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

# plot all servicexpopulation combinations, with other points coded by category as option

# Sediment
s.pov <- ggplot(data = subset(vul_melt, vul_melt$variable=="Hogares_pobres_per"), aes(x=Sed_priority, y=value, fill=Sed_priority)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = value, x=Sed_priority, color=Sedxwater), 
             position = position_jitter(width = .15),
             size = .5, alpha = 0.8) +
  expand_limits(x = 2.75) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  # scale_color_manual(values=c("#e8e8e8", "#b0d5df", "#64acbe", "#e4acac", "#ad9ea5", "#627f8c", "#c85a5a", "#985356", "#574249")) +
  scale_color_manual(values=c("lightgrey", "lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey", "#68555b")) +
  scale_fill_manual(values=c("lightgrey", "#68555b")) +
  theme_bw() +
  raincloud_theme +
  labs(x="", y="% Poor Households") +
  #labs(x="", y="Percent", title = "Poor Households") +
  theme(legend.position = "none", 
        plot.title=element_text(hjust=0.5, vjust=6, size=14), axis.title.y=element_text(vjust=3, color="darkgrey", size=10),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,0.5,0,0.5), "cm")) 
  

s.ind <- ggplot(data = subset(vul_melt, vul_melt$variable=="Porcentaje_indigena"), aes(x=Sed_priority, y=log10(value+1), fill=Sed_priority)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = log10(value+1), x=Sed_priority, color=Sedxwater), 
             position = position_jitter(width = .15),
             size = .5, alpha = 0.8) +
  expand_limits(x = 2.75) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  # scale_color_manual(values=c("#e8e8e8", "#b0d5df", "#64acbe", "#e4acac", "#ad9ea5", "#627f8c", "#c85a5a", "#985356", "#574249")) +
  scale_color_manual(values=c("lightgrey", "lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey", "#68555b")) +
  scale_fill_manual(values=c("lightgrey", "#68555b")) +
  theme_bw() +
  raincloud_theme +
  labs(x="", y="Log % Indigenous Population") +
  # labs(x="", y="Log percent", title = "Indigenous Population") +
  theme(legend.position = "none", 
        plot.title=element_text(hjust=0.5, vjust=6, size=14), axis.title.y=element_text(vjust=3, color="darkgrey", size=10),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,0.5,0,0.5), "cm")) 

s.wom <- ggplot(data = subset(vul_melt, vul_melt$variable=="Percent_womenled"), aes(x=Sed_priority, y=log10(value+1), fill=Sed_priority)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = log10(value+1), x=Sed_priority, color=Sedxwater), 
             position = position_jitter(width = .15),
             size = .5, alpha = 0.8) +
  expand_limits(x = 2.75) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  # scale_color_manual(values=c("#e8e8e8", "#b0d5df", "#64acbe", "#e4acac", "#ad9ea5", "#627f8c", "#c85a5a", "#985356", "#574249")) +
  scale_color_manual(values=c("lightgrey", "lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey", "#68555b")) +
  scale_fill_manual(values=c("lightgrey", "#68555b")) +
  theme_bw() +
  raincloud_theme +
  # labs(x="", y="Log percent", title = "Women-led Households") +
  labs(x="", y="Log % Women-led Households") +
  theme(legend.position = "none", 
        plot.title=element_text(hjust=0.5, vjust=6, size=14), axis.title.y=element_text(vjust=3, color="darkgrey", size=10),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,0.5,0,0.5), "cm")) 


# N & P
n.pov <- ggplot(data = subset(vul_melt, vul_melt$variable=="Hogares_pobres_per"), aes(x=NP_priority, y=value, fill=NP_priority)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = value, x=NP_priority, color=Pxwater), 
             position = position_jitter(width = .15),
             size = .5, alpha = 0.8) +
  expand_limits(x = 2.75) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  # scale_color_manual(values=c("#e8e8e8", "#b0d5df", "#64acbe", "#e4acac", "#ad9ea5", "#627f8c", "#c85a5a", "#985356", "#574249")) +
  scale_color_manual(values=c("lightgrey", "lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey", "#68555b")) +
  scale_fill_manual(values=c("lightgrey", "#68555b")) +
  theme_bw() +
  raincloud_theme +
  # labs(x="", y="Percent") +
  labs(x="", y="% Poor Households") +
  theme(legend.position = "none", 
        plot.title=element_text(hjust=0.5, vjust=4), axis.title.y=element_text(vjust=3, color="darkgrey", size=10),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,0.5,0,0.5), "cm")) 

n.ind <- ggplot(data = subset(vul_melt, vul_melt$variable=="Porcentaje_indigena"), aes(x=NP_priority, y=log10(value+1), fill=NP_priority)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = log10(value+1), x=NP_priority, color=Pxwater), 
             position = position_jitter(width = .15),
             size = .5, alpha = 0.8) +
  expand_limits(x = 2.75) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  # scale_color_manual(values=c("#e8e8e8", "#b0d5df", "#64acbe", "#e4acac", "#ad9ea5", "#627f8c", "#c85a5a", "#985356", "#574249")) +
  scale_color_manual(values=c("lightgrey", "lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey", "#68555b")) +
  scale_fill_manual(values=c("lightgrey", "#68555b")) +
  theme_bw() +
  raincloud_theme +
  # labs(x="", y="Log percent") +
  labs(x="", y="Log % Indigenous Population") +
  theme(legend.position = "none", 
        plot.title=element_text(hjust=0.5, vjust=4), axis.title.y=element_text(vjust=3, color="darkgrey", size=10),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,0.5,0,0.5), "cm")) 

n.wom <- ggplot(data = subset(vul_melt, vul_melt$variable=="Percent_womenled"), aes(x=NP_priority, y=log10(value+1), fill=NP_priority)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = log10(value+1), x=NP_priority, color=Pxwater), 
             position = position_jitter(width = .15),
             size = .5, alpha = 0.8) +
  expand_limits(x = 2.75) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  # scale_color_manual(values=c("#e8e8e8", "#b0d5df", "#64acbe", "#e4acac", "#ad9ea5", "#627f8c", "#c85a5a", "#985356", "#574249")) +
  scale_color_manual(values=c("lightgrey", "lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey", "#68555b")) +
  scale_fill_manual(values=c("lightgrey", "#68555b")) +
  theme_bw() +
  raincloud_theme +
  # labs(x="", y="Log percent") +
  labs(x="", y="Log % Women-led Households") +
  theme(legend.position = "none", 
        plot.title=element_text(hjust=0.5, vjust=4), axis.title.y=element_text(vjust=3, color="darkgrey", size=10),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,0.5,0,0.5), "cm")) 

pdf("./Figures/C3cantons_vuln_raindrop_v2.pdf", height = 6, width = 9)
multiplot(s.pov, n.pov,
          s.ind, n.ind,
          s.wom, n.wom, cols=3)
dev.off()
# Add legend in editor

tiff("./Figures/C3cantons_vuln_raindrop_v2.tiff", height = 6, width = 9, res=300, units="in")
multiplot(s.pov, n.pov,
          s.ind, n.ind,
          s.wom, n.wom, cols=3)
dev.off()

############################
# Indigenous territories
#############################

# One territory, RIGDO, had 0 forest cover added, which is leading to Inf values when divided by forest cover
# territory does have some non-0 increase values, because of cross-boundary service provision presumably
subset(ind_mer, ind_mer$abb=="RIGDO")
is.na(ind_mer)<-sapply(ind_mer, is.infinite)
ind_mer[is.na(ind_mer)]<-0
head(ind_mer)

# Calculate service and load across all of Costa Rica, not just non-Indigenous lands, for comparison
CR_Seddiffkm <- sum(ind_mer$Sed_diff_sum)/sum(ind_mer$area_km)
CR_Seddiffkmfc <- sum(ind_mer$Sed_diff_sum)/sum(ind_mer$fc_diff_km2)
CR_Sedloadkm <- sum(ind_mer$Sed_load_sum)/sum(ind_mer$area_km)
CR_Ndiffkm <- sum(ind_mer$N_diff_sum)/sum(ind_mer$area_km)
CR_Ndiffkmfc <- sum(ind_mer$N_diff_sum)/sum(ind_mer$fc_diff_km2)
CR_Nloadkm <- sum(ind_mer$N_load_sum)/sum(ind_mer$area_km)
CR_Pdiffkm <- sum(ind_mer$P_diff_sum)/sum(ind_mer$area_km)
CR_Pdiffkmfc <- sum(ind_mer$P_diff_sum)/sum(ind_mer$fc_diff_km2)
CR_Ploadkm <- sum(ind_mer$P_load_sum)/sum(ind_mer$area_km)
CR_Carbon_bacdiffkm <- sum(ind_mer$Carbon_bac_diff_sum)/sum(ind_mer$area_km)
CR_Carbon_bacdiffkmfc <- sum(ind_mer$Carbon_bac_diff_sum)/sum(ind_mer$fc_diff_km2)
CR_fckm <- sum(ind_mer$fc_diff_km2)
CR_areakm <- sum(ind_mer$area_km)

# Add to main table
ind_plot <- ind_mer[,c("abb", "indig_land_bin", "area_km", "fc_diff_km2",
                        "Sed_diff_sum_perkm", "Sed_diff_sum_perkmfc", "Sed_load_sum_perkm",
                       "N_diff_sum_perkm", "N_diff_sum_perkmfc", "N_load_sum_perkm",
                       "P_diff_sum_perkm", "P_diff_sum_perkmfc", "P_load_sum_perkm",
                       "Carbon_bac_diff_sum_perkm", "Carbon_bac_diff_sum_perkmfc"
                       )]
head(ind_plot)
ind_plot <- subset(ind_plot, !(ind_plot$abb=="Nn-nl"))
ind_plot2 <- rbind(c("CR", "n", CR_areakm, CR_fckm, 
                     CR_Seddiffkm, CR_Seddiffkmfc, CR_Sedloadkm,
                     CR_Ndiffkm, CR_Ndiffkmfc, CR_Nloadkm,
                     CR_Pdiffkm, CR_Pdiffkmfc, CR_Ploadkm,
                     CR_Carbon_bacdiffkm, CR_Carbon_bacdiffkmfc), 
                   ind_plot)
head(ind_plot2)
ind_plot2$abb

### Increase in service per area

s1 <- ggplot(data = ind_plot2, aes(x=factor(abb, levels=abb), y=(as.numeric(Sed_diff_sum_perkm)*-1), fill=indig_land_bin)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("lightgray", "#877266")) +
  # geom_text(aes(label=NOM_CANT_1), hjust=0, vjust=0) +
  labs(x="", y="Increase in sediment retention per area (tons/yr/km2)") +
  # labs(y="Sediment (tons/yr)", x="", title="Decrease in export with reforestation") +
  theme_classic() +
  theme(axis.title.x = element_text(vjust=-1), axis.title.y = element_text(vjust = 3), plot.title = element_text(vjust = 4, hjust=0.5, size=10)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(1,0.5,0,0.5), "cm"))

n1 <- ggplot(data = ind_plot2, aes(x=factor(abb, levels=abb), y=(as.numeric(N_diff_sum_perkm)*-1), fill=indig_land_bin)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("lightgray", "#d3765b")) +
  # geom_text(aes(label=NOM_CANT_1), hjust=0, vjust=0) +
  labs(x="", y="Increase in N retention per area (kg/yr/km2)") +
  # labs(y="Sediment (tons/yr)", x="", title="Decrease in export with reforestation") +
  theme_classic() +
  theme(axis.title.x = element_text(vjust=-1), axis.title.y = element_text(vjust = 3), plot.title = element_text(vjust = 4, hjust=0.5, size=10)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(1,0.5,0,0.5), "cm"))

p1 <- ggplot(data = ind_plot2, aes(x=factor(abb, levels=abb), y=(as.numeric(P_diff_sum_perkm)*-1), fill=indig_land_bin)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("lightgray", "#a16ca0")) +
  # geom_text(aes(label=NOM_CANT_1), hjust=0, vjust=0) +
  labs(x="", y="Increase in P retention per area (kg/yr/km2)") +
  # labs(y="Sediment (tons/yr)", x="", title="Decrease in export with reforestation") +
  theme_classic() +
  theme(axis.title.x = element_text(vjust=-1), axis.title.y = element_text(vjust = 3), plot.title = element_text(vjust = 4, hjust=0.5, size=10)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(1,0.25,0,0.75), "cm"))

c1 <- ggplot(data = ind_plot2, aes(x=factor(abb, levels=abb), y=(as.numeric(Carbon_bac_diff_sum_perkm)), fill=indig_land_bin)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("lightgray", "aquamarine3")) +
  # geom_text(aes(label=NOM_CANT_1), hjust=0, vjust=0) +
  labs(x="", y="Increase in carbon stock per area (Mg/yr/km2)") +
  # labs(y="Sediment (tons/yr)", x="", title="Decrease in export with reforestation") +
  theme_classic() +
  theme(axis.title.x = element_text(vjust=-1), axis.title.y = element_text(vjust = 3), plot.title = element_text(vjust = 4, hjust=0.5, size=10)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(1,0.25,0,0.75), "cm"))


pdf("./Figures/Indigland_services.pdf", width = 15, height = 10)
multiplot(s1, n1, p1, c1, cols=2)
dev.off()


#### Increase in service per FC added

s1 <- ggplot(data = ind_plot2, aes(x=factor(abb, levels=abb), y=(as.numeric(Sed_diff_sum_perkmfc)*-1), fill=indig_land_bin)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("lightgray", "#877266")) +
  # geom_text(aes(label=NOM_CANT_1), hjust=0, vjust=0) +
  labs(x="", y="Increase in sediment retention per forest added (tons/yr/km2)") +
  # labs(y="Sediment (tons/yr)", x="", title="Decrease in export with reforestation") +
  theme_classic() +
  theme(axis.title.x = element_text(vjust=-1), axis.title.y = element_text(vjust = 3), plot.title = element_text(vjust = 4, hjust=0.5, size=10)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(1,0.5,0,0.5), "cm"))

n1 <- ggplot(data = ind_plot2, aes(x=factor(abb, levels=abb), y=(as.numeric(N_diff_sum_perkmfc)*-1), fill=indig_land_bin)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("lightgray", "#d3765b")) +
  # geom_text(aes(label=NOM_CANT_1), hjust=0, vjust=0) +
  labs(x="", y="Increase in N retention per forest added (kg/yr/km2)") +
  # labs(y="Sediment (tons/yr)", x="", title="Decrease in export with reforestation") +
  theme_classic() +
  theme(axis.title.x = element_text(vjust=-1), axis.title.y = element_text(vjust = 3), plot.title = element_text(vjust = 4, hjust=0.5, size=10)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(1,0.5,0,0.5), "cm"))

p1 <- ggplot(data = ind_plot2, aes(x=factor(abb, levels=abb), y=(as.numeric(P_diff_sum_perkmfc)*-1), fill=indig_land_bin)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("lightgray", "#a16ca0")) +
  # geom_text(aes(label=NOM_CANT_1), hjust=0, vjust=0) +
  labs(x="", y="Increase in P retention per forest added (kg/yr/km2)") +
  # labs(y="Sediment (tons/yr)", x="", title="Decrease in export with reforestation") +
  theme_classic() +
  theme(axis.title.x = element_text(vjust=-1), axis.title.y = element_text(vjust = 3), plot.title = element_text(vjust = 4, hjust=0.5, size=10)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(1,0.25,0,0.75), "cm"))

c1 <- ggplot(data = ind_plot2, aes(x=factor(abb, levels=abb), y=(as.numeric(Carbon_bac_diff_sum_perkmfc)), fill=indig_land_bin)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("lightgray", "aquamarine3")) +
  # geom_text(aes(label=NOM_CANT_1), hjust=0, vjust=0) +
  labs(x="", y="Increase in carbon stock per forest added (Mg/yr/km2)") +
  # labs(y="Sediment (tons/yr)", x="", title="Decrease in export with reforestation") +
  theme_classic() +
  theme(axis.title.x = element_text(vjust=-1), axis.title.y = element_text(vjust = 3), plot.title = element_text(vjust = 4, hjust=0.5, size=10)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(1,0.25,0,0.75), "cm"))


pdf("./Figures/Indigland_services_perfc.pdf", width = 15, height = 10)
multiplot(s1, n1, p1, c1, cols=2)
dev.off()



### Forest cover raw data and load 

f1 <- ggplot(data = ind_plot2, aes(x=factor(abb, levels=abb), y=(as.numeric(fc_diff_km2)/as.numeric(area_km)), fill=indig_land_bin)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("lightgray", "lightgreen")) +
  # geom_text(aes(label=NOM_CANT_1), hjust=0, vjust=0) +
  labs(x="", y="Increase in forest per area (km2/km2)") +
  # labs(y="Sediment (tons/yr)", x="", title="Decrease in export with reforestation") +
  theme_classic() +
  theme(axis.title.x = element_text(vjust=-1), axis.title.y = element_text(vjust = 3), plot.title = element_text(vjust = 4, hjust=0.5, size=10)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(1,0.25,0,0.75), "cm"))

s1 <- ggplot(data = ind_plot2, aes(x=factor(abb, levels=abb), y=(as.numeric(Sed_load_sum_perkm)), fill=indig_land_bin)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("lightgray", "#877266")) +
  # geom_text(aes(label=NOM_CANT_1), hjust=0, vjust=0) +
  labs(x="", y="Sediment load per area (tons/yr/km2)") +
  # labs(y="Sediment (tons/yr)", x="", title="Decrease in export with reforestation") +
  theme_classic() +
  theme(axis.title.x = element_text(vjust=-1), axis.title.y = element_text(vjust = 3), plot.title = element_text(vjust = 4, hjust=0.5, size=10)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(1,0.5,0,0.5), "cm"))

n1 <- ggplot(data = ind_plot2, aes(x=factor(abb, levels=abb), y=(as.numeric(N_load_sum_perkm)), fill=indig_land_bin)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("lightgray", "#d3765b")) +
  # geom_text(aes(label=NOM_CANT_1), hjust=0, vjust=0) +
  labs(x="", y="N load per area (kg/yr/km2)") +
  # labs(y="Sediment (tons/yr)", x="", title="Decrease in export with reforestation") +
  theme_classic() +
  theme(axis.title.x = element_text(vjust=-1), axis.title.y = element_text(vjust = 3), plot.title = element_text(vjust = 4, hjust=0.5, size=10)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(1,0.5,0,0.5), "cm"))

p1 <- ggplot(data = ind_plot2, aes(x=factor(abb, levels=abb), y=(as.numeric(P_load_sum_perkm)), fill=indig_land_bin)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("lightgray", "#a16ca0")) +
  # geom_text(aes(label=NOM_CANT_1), hjust=0, vjust=0) +
  labs(x="", y="P load per area (kg/yr/km2)") +
  # labs(y="Sediment (tons/yr)", x="", title="Decrease in export with reforestation") +
  theme_classic() +
  theme(axis.title.x = element_text(vjust=-1), axis.title.y = element_text(vjust = 3), plot.title = element_text(vjust = 4, hjust=0.5, size=10)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(1,0.25,0,0.75), "cm"))


pdf("./Figures/Indigland_load.pdf", width = 15, height = 10)
multiplot(s1, n1, p1, f1, cols=2)
dev.off()

###############################################
# Make % change figure
########################################

### If recalculating, comment this in###
totals <- CR_totals.df

# ### Read in if skipping calculations ###
# totals <- read.csv( "./Outputs/CR_service_totals_14June21.csv")
# row.names(totals) <- totals$Names
# percchange_fc <- totals["Percent difference scenario medium", "Forest.Cover"]
# percchange_tot_carbon <- totals["Percent difference scenario medium", "Carbon"]
# percchange_med_tot_sed_exp <- totals["Percent difference scenario medium", "Sediment"]
# percchange_med_tot_n_exp <- totals["Percent difference scenario medium", "Nitrogen"]
# percchange_med_tot_p_exp <- totals["Percent difference scenario medium", "Phosphorus"]
# sed_lo <- totals["Percent difference scenario high", "Sediment"]
# sed_hi <- totals["Percent difference scenario low", "Sediment"]
# n_lo <- totals["Percent difference scenario low", "Nitrogen"]
# n_hi <- totals["Percent difference scenario high", "Nitrogen"]
# p_lo <- totals["Percent difference scenario low", "Phosphorus"]
# p_hi <- totals["Percent difference scenario high", "Phosphorus"]

# Get data on percent change into a single df
df <- data.frame(matrix(ncol = 2, nrow = 5))
x <- c("Service", "Percent.change")
colnames(df) <- x
df$Service <- c("Forest cover", "Carbon stock", "Sediment export", "Nitrogen export", "Phosphorus export")
df$Percent.change <- c(percchange_fc, percchange_tot_carbon, (percchange_med_tot_sed_exp), (percchange_med_tot_n_exp), (percchange_med_tot_p_exp))
df



p <- ggplot(df, aes(x=c(1,3,4,5,6), y=Percent.change, fill=Service)) +
  geom_bar(stat="identity", width = 0.85) +
  scale_x_discrete(limits=c("Forest cover", "Carbon stock", "Sediment export", "Nitrogen export", "Phosphorus export")) + 
  scale_fill_manual(values=c("Forest cover" = "#8ecc70", 
                             "Carbon stock" = "#38a18a",
                             "Sediment export" = "#877266",
                             "Nitrogen export" = "#d3765b",
                             "Phosphorus export" ="#a16ca0")) +
  theme(legend.position="none") + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) +
  scale_y_continuous(limits = c(0,90), expand = c(0, 0)) +
  # Getting rid of axis labels
  theme(axis.text.y = element_text(size=15, color = "grey40"), # resize text!
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_line(color = "grey"))

p  

pdf("./Figures/Service_percchange_bargraph.pdf")
p
dev.off()
# Do axis re-labeling, bar labeling, line drawing in editor

p2 <- ggplot(df, aes(x=c(1,2,3,4,5), y=Percent.change, fill=Service)) +
  geom_bar(stat="identity", width = 0.85) +
  scale_x_discrete(limits=c("Forest cover", "Carbon stock", "Sediment export", "Nitrogen export", "Phosphorus export")) + 
  scale_fill_manual(values=c("Forest cover" = "#8ecc70", 
                             "Carbon stock" = "#38a18a",
                             "Sediment export" = "#877266",
                             "Nitrogen export" = "#d3765b",
                             "Phosphorus export" ="#a16ca0")) +
  geom_errorbar(aes(ymin = c(NA,NA,sed_lo, n_lo, p_lo), 
                    ymax = c(NA,NA, sed_hi, n_hi, p_hi)), 
                width=.2, 
                colour = "grey20"
                # position=position_dodge(.9)
                )+
  theme(legend.position="none") + 
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  # Getting rid of axis labels
  theme(axis.text.y = element_text(size=15, color = "grey20"), # resize text!
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_line(color = "grey"))

pdf("./Figures/Service_percchange_bargraph_errorbars.pdf")
p2
dev.off()


