## compute IPO following Don and Dai 2015 from HadISST SST.

if (!request("ipo_9")) {

  isst_file <- "downloads/HadISST_sst.nc"
  isst_gz_file <- paste0(isst_file, ".gz")

  
  if (!file.exists(isst_file)) {
    dir.create("downloads", showWarnings = FALSE)
    cat("Downloading the Hadley Centre Sea Ice and Sea Surface Temperature data set (large file, stay tuned).\n")
    download.file(ISST_URL, isst_gz_file)
    R.utils::gunzip(isst_gz_file)
  }

  hadisst <- nc_open(isst_file)
  temp_hadisst %<c-% ncvar_get(hadisst, "sst")
  dimnames(temp_hadisst) <- list(lon = hadisst$dim$lon$vals,
                                 lat = hadisst$dim$lat$vals,
                                 t = hadisst$dim$time$vals)
  temp_hadisst <- as_tibble(melt(temp_hadisst, value.name = "temp"))
  temp_hadisst$t <- as.Date(temp_hadisst$t, origin = "1870-01-01")
  rm(hadisst)

  temp_hadisst <- temp_hadisst %>%
    mutate(temp = ifelse(temp == -1000, NA, temp))

  ## yearly mean; selecting a longer time span because of multiple
  ## smoothing from historic end

  temp_hadisst %<f-% {
    temp_hadisst %>%
      group_by(lon, lat, year = year(t)) %>%
      summarise(temp = mean(temp, na.rm = TRUE)) %>%
      filter(year > 1928, lat < 60, lat > -60) %>% 
      group_by(lon, lat) %>%
      filter(!all(is.na(temp))) %>%
      nest()
  }

  ## rolling n yr mean
  rolln <- function(df, n = 3) {
    tibble(
      year = df$year,
      temp = rollmean(df$temp, k = n, align = "right", na.pad = TRUE)
    )
  }
  
  hadisst_r3 <- temp_hadisst %>%
    mutate(roll3 = purrr::map(data, rolln))
  
  ## transform to 2D matrix gridcellid ~ year for PCA (= EOF)
  
  hadisst_spread <- hadisst_r3 %>%
    select(lon, lat, roll3) %>%
    mutate(cell = paste(lon, lat, sep = "+")) %>%
    unnest(roll3) %>%
    select(cell, year, temp) %>%
    spread(year, temp)
  
  cells <- hadisst_spread$cell
  
  hadisst_matrix <- as.matrix(hadisst_spread %>%
                                select(-cell))
  rownames(hadisst_matrix) <- cells
  
  ## remove columns with NAs
  hadisst_matrix <- hadisst_matrix[, -which(is.na(colSums(hadisst_matrix)))]

  eof <- princomp(hadisst_matrix)

  ## extract second EOF
  ipo_raw <- eof$loadings[, 2]

  ## check correctness of sign; first value must be positive, if not
  ## flip signs!
  if (ipo_raw[1] < 0) {
    ipo_raw <- -ipo_raw
  }
  
  ## apply 9 yr smoother twice
  ipo_9 <- rollmean(rollmean(ipo_raw, k = 9, align = "right"), k = 9,
                    align = "right")

  ipo_9 %<f-% tibble(year = 1947:2017,
                     ipo = ipo_9)

  ## check IPO on map (for comparison w/ Dai paper)
  ipo_score <- eof$scores[, 2]
  ## unpack cell-ids
  split <- strsplit(names(ipo_score), "\\+")
  ipo_lon <- sapply(split, "[[", 1) %>% as.numeric
  ipo_lat <- sapply(split, "[[", 2) %>% as.numeric
  ipo_df <- tibble(
    x = ipo_lon,
    y = ipo_lat,
    ipo = ipo_score
  ) %>% reraster

  tm_shape(ipo_df) +
    tm_raster("ipo", palette = hpalette,
              legend.show = FALSE, auto.palette.mapping = FALSE,
              colorNA = "#e3e3e3") +
  map_layer

  ## looks perfect!
}
