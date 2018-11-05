### General remarks -------------------------------------------------

## Longer computations are marked with @duration and an estimate
## (referring to a slightly outdated quadcore i5 laptop w/ 16GB RAM)

## The total duration for a full run without any cached intermediates
## is around 5 hours on abovementioned machine.

## Figure numbers corresponding to manuscript and SI are marked with
## @figure; table data is marked with @table

### Setup workspace -------------------------------------------------
## Load required packages and extra functions. Set global variables
## (all in all-caps).
source("lib/init.R")

## Save session info to file for later reference in case of
## incompatibilities.
source("lib/save_session_info.R")

### Prepare/read in data --------------------------------------------

## Create or read in tidified temperature data from three different
## netCDF data sources.

## This is conditional on having done this before, since creating tidy
## data from netCDF takes long; we check if the tidy data has been
## cached, and read from binary files if appropriate.
source("munge/data.R")

### Identification of ENSO influence --------------------------------

## Correlation test of MEI time series with gridded temp products;
## results are cached to mei_[dataset]
## @duration 5'
## TODO for the revision, this procedure has to change!
## TODO we need to account for spatial correlation bias (@José)
## TODO (really? we are not correlation *two* spatial patterns here!
## MEI is a single time series, only the climate is spatial data)
## Added FDR correction for multile testing

## TODO we need to adapt the method for interpolation:
## try an even more continuous approach w/o significance threshold;
## and only use modelled values for 97/98
source("analysis/elnino.R")

## Plot correlation with MEI (not part of the manuscript)
source("analysis/mei_cor_maps.R")

### Compute slopes and significances --------------------------------

## This will cache the results according to the scheme
## "slopes_[dataset]_[period]", where [period] is one of the hiatus
## periods referred to by PERIODS.
## @duration ~10h
if (!request("slopes_gistemp_R")) {
  ## take this cached file as proxy for the existence of all others
  source("analysis/slopes.R")
} else {
  request_all("slopes_(gistemp|hadcrut|noaa)")
}


## Do the same for the after removing El Niño influence for gridcells
## w/ significant correlation with MEI. This will cache the results
## according to the scheme "slopes_meiresid_[dataset]_[period]", where
## [period] is one of the hiatus periods referred to by PERIODS.
## @duration ~10h
if (!request("slopes_meiresid_gistemp_R")) {
  ## take this cached file as proxy for the existence of all others
  source("analysis/slopes_sans_elnino.R")
} else {
  request_all("slopes_meiresid")
}

# the following takes all slopes_ created before and 
# adjusts p-values for multiple testing using fdr procedure
# also assumes existence of all slope_ objects created above 
# @duration 30'
source("analysis/fdr_slopes.R")

## Compute percentages of (sign.) slope changes
## @table 1
source("analysis/slope_perc.R")

## Combine with information about significant correlation with MEI
source("analysis/mei_percent.R")

## Plot density of slope changes for different reference periods
## @figure 4
source("analysis/slope_difference_density.R")

### Draw maps -------------------------------------------------------

## 1.) Draw maps of trend changes for the three datasets w/
## significances according to both tests, as well as a consensus map;
## this is done for all periods, only the first period ("R") is shown
## in the main text, the rest goes to SI.
##
## 2.) Do the same after the interpolation of ENSO effects (not shown
## anywhere currently).
##
## 3.) Draw consensus maps of how regions with significant slowdown
## agree before and after interpolation.
##
## Details (labels, legend, etc.) to be done in Inkscape
## @figure 1 3 S1 S2 S3
source("analysis/maps.R")

### Show trends for example regions ---------------------------------

## @figure 2 S4 S5 S6
source("analysis/example_regions.R")

### Compute IPO and correlation with temperature fields -------------

## Compute IPO according to Dong and Dai 2015
## @duration 5'
source("analysis/ipo.R")

## Correlate IPO with all gridcells, similar to ENSO, but detrend
## temperature before correlation.
## @duration 10'
source("analysis/correlate_ipo.R")

## Draw maps of IPO correlations
## @figure S7
source("analysis/ipo_cor_maps.R")

### Contribution of IPO cells to global trend -----------------------

## This will compute the data for
## @table 2
## and create
## @figure 5 S8 S9 S10
## and spit out the test results for the contingency table tests that
## are cited in the text of the main manuscript
## @duration 20'
source("analysis/contribution.R")
