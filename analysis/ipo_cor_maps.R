## draw maps with mei/sst-correlations

request("ipo_gistemp_fdr")
request("ipo_noaa_fdr")
request("ipo_hadcrut_fdr")

## x is a ipo_X object
ipo_ref <- function(x) {
  x <- x %>%
    unnest(cipo) %>%
    select(x = lon, y = lat, cor, p) %>%
    mutate(sig = ifelse(p < 0.05, TRUE, FALSE),
           pos = ifelse(cor > 0, TRUE, FALSE)) %>%
    mutate(mode = ifelse(
      pos & sig, 1,                     # sig +
      ifelse(
        pos & !sig, 2,                  # +
        ifelse(
          !pos & sig, 3,                # sig -
          4                             # -
        )
      )
    )) %>% 
  as.data.frame
  x
}

ipo_gistemp_R <- ipo_gistemp_fdr %>%
  ipo_ref %>%
  reraster

ipo_map_gistemp <- tm_shape(ipo_gistemp_R) +
  tm_raster("mode", palette = hpalette,
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
  map_layer

ipo_noaa_R <- ipo_noaa_fdr %>%
  ## longitudes range from 0 to 360 and need to be fixed prior to
  ## remapping
  mutate(lon = ifelse(lon < 180, lon, lon - 360)) %>% 
  ipo_ref %>%
  reraster

ipo_map_noaa <- tm_shape(ipo_noaa_R) +
  tm_raster("mode", palette = hpalette,
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
  map_layer

ipo_hadcrut_R <- ipo_hadcrut_fdr %>%
  ipo_ref %>%
  reraster

ipo_map_hadcrut <- tm_shape(ipo_hadcrut_R) +
  tm_raster("mode", palette = hpalette,
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
  map_layer

pdf("figures/ipo_correlation_maps_fdr.pdf", width = 8, height = 4)
tmap_arrange(ipo_map_gistemp, ipo_map_hadcrut, ipo_map_noaa,
             ncol = 3)
dev.off()
