### Packages --------------------------------------------------------

library(ncdf4)
library(fridge)
library(reshape2)
library(raster)
library(gstat)
library(sp)
library(tmap)
library(strucchange)
library(lubridate)
library(purrr)
library(tidyverse)

### Package data ----------------------------------------------------

data(World)

### Global variables ------------------------------------------------

## warming/hiatus periods for testing
PERIODS <- list(
  R = list(wp = 1972:2000,
           hp = 2001:2014),
  p2 = list(wp = 1972:1999,
            hp = 2000:2014),
  p3 = list(wp = 1972:1998,
            hp = 1999:2013),
  p4 = list(wp = 1972:1997,
            hp = 1999:2012)
)

## should stuff be computed in parallel
PARALLEL <- TRUE

## set number of cores for parallel stuff and load parallel package
if (PARALLEL) {
  library(parallel)
  NCORES <- detectCores() - 1
}

## p-threshold for significant correlations of temp with MEI
P_MEI <- 0.05

## URL from which to retrieve NetCDF for HadISST_SST data
ISST_URL <-
  "https://www.metoffice.gov.uk/hadobs/hadisst/data/HadISST_sst.nc.gz"

## number of MC simulations
N_MC <- 5000

## which method to use to remove ENSO effects:
## en9798: remove ENSO only for El Niño years 97/98
## full  : remove ENSO effects for entire time span
REMOVE_ENSO <- "en9798"

## should ENSO effects only be removed for gridcells with significant
## ENSO effects, or for all grid cells?
SIG_ENSO_ONLY <- TRUE

### RNG -------------------------------------------------------------

set.seed(42)

### Library functions -----------------------------------------------

source("lib/slope_fn.R")
source("lib/detrend_df.R")
source("lib/mei_residuals.R")
source("lib/reraster.R")
source("lib/map_layer.R")
source("lib/fdr_fn.R")
source("lib/phipson_smith_test.R")
source('lib/correction_factor_fn.R')
