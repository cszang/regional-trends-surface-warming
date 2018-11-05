## read in and tidy climate data sets (if necessary)

data_path <- "cache/temps.rda"

data_present <- file.exists(data_path)

## guard against expensive re-reading
missing <- list("temp_gistemp", "temp_noaa", "temp_hadcrut") %>% 
  vapply(exists, FUN.VALUE = logical(1)) %>% 
  `!` %>% 
  any()

if (data_present & missing) {
  cat("Reading data from cache.\n")
  load(data_path)
}

if (data_present & !missing) {
  cat("Nothing to be done.\n")
  ## all is fine, nothing to be done
}

if (!data_present) {

  cat("Reading data from netCDF files and creating cache.\n")
  
  ## conditional caching

  ## 1. NASA GISTEMP
  gistemp <- nc_open("data/gistemp1200_ERSSTv5.nc")
  temp_gistemp %<c-% ncvar_get(gistemp, "tempanomaly")
  dimnames(temp_gistemp) <- list(lon = gistemp$dim$lon$vals,
                                lat = gistemp$dim$lat$vals,
                                t = gistemp$dim$time$vals)
  temp_gistemp <- as_tibble(melt(temp_gistemp, value.name = "temp"))
  temp_gistemp$t <- as.Date(temp_gistemp$t, origin = "1800-01-01")
  rm(gistemp)
  
  ## 2. NOAA Global Surface Temperature Dataset (NOAAGlobalTemp)
  noaa <- nc_open("data/air.mon.anom.nc")
  temp_noaa %<c-% ncvar_get(noaa, "air")
  dimnames(temp_noaa) <- list(lon = noaa$dim$lon$vals,
                             lat = noaa$dim$lat$vals,
                             t = noaa$dim$time$vals)
  temp_noaa <- as_tibble(melt(temp_noaa, value.name = "temp"))
  temp_noaa$t <- as.Date(temp_noaa$t, origin = "1800-01-01")
  rm(noaa)

  ## 3. HadCRUT 4.5.0.0
  hadcrut <- nc_open("data/HadCRUT.4.5.0.0.median.nc")
  temp_hadcrut %<c-% ncvar_get(hadcrut, "temperature_anomaly")
  dimnames(temp_hadcrut) <- list(lon = hadcrut$dim$lon$vals,
                                lat = hadcrut$dim$lat$vals,
                                t = hadcrut$dim$time$vals)
  temp_hadcrut <- as_tibble(melt(temp_hadcrut, value.name = "temp"))
  temp_hadcrut$t <- as.Date(temp_hadcrut$t, origin = "1850-01-01")
  rm(hadcrut)
  
  ## truncate to recent time span > 1949
  temp_gistemp <- filter(temp_gistemp, year(t) > 1950)
  temp_noaa <- filter(temp_noaa, year(t) > 1950)
  temp_hadcrut <- filter(temp_hadcrut, year(t) > 1950)
  
  ## make yearly averages, filter out all-na cells, and nest
  make_yearly <- function(x) {
    x %>% group_by(lon, lat, year = year(t)) %>%
      summarise(temp = mean(temp, na.rm = TRUE)) %>%
      group_by(lon, lat) %>%
      filter(sum(is.na(temp)) == 0) %>% 
      nest()
  }

  temp_gistemp <- make_yearly(temp_gistemp)
  temp_noaa <- make_yearly(temp_noaa)
  temp_hadcrut <- make_yearly(temp_hadcrut)
  
  save(temp_gistemp, temp_noaa, temp_hadcrut, file = data_path)
}
