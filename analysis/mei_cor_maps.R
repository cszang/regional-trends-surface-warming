## draw maps with mei/sst-correlations

request_all("mei_")

## explorative plots

## x is a mei_X object
mei_ref <- function(x) {
  x <- x %>%
    unnest(cmei) %>%
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

mei_gistemp_R <- mei_gistemp %>%
  mei_ref %>%
  reraster

mei_map_gistemp <- tm_shape(mei_gistemp_R) +
  tm_raster("mode", palette = hpalette,
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
  map_layer

mei_noaa_R <- mei_noaa %>%
  ## longitudes range from 0 to 360 and need to be fixed prior to
  ## remapping
  mutate(lon = ifelse(lon < 180, lon, lon - 360)) %>% 
  mei_ref %>%
  reraster

mei_map_noaa <- tm_shape(mei_noaa_R) +
  tm_raster("mode", palette = hpalette,
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
  map_layer

mei_hadcrut_R <- mei_hadcrut %>%
  mei_ref %>%
  reraster

mei_map_hadcrut <- tm_shape(mei_hadcrut_R) +
  tm_raster("mode", palette = hpalette,
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
  map_layer

pdf("figures/mei_correlation_maps_fdr.pdf", width = 8, height = 4)
tmap_arrange(mei_map_gistemp, mei_map_hadcrut, mei_map_noaa,
             ncol = 3)
dev.off()
