## adding map to x layer

## used for hiatus maps
hpalette <- c("#0000ff", "#9797d8", "#ff0000", "#e27878")
## used for consensus style maps
fpalette <- c("#0000ff", "#ffa500", "#00ff00", "#e3e3e3")

## create adaptive colour palette based on levels in rastered data
adaptive_palette <- function(.raster, .palette = hpalette) {
  .levels <- sort(na.omit(unique(values(.raster))))
  .palette[.levels]
}

map_layer <- tm_shape(World) + 
  tm_borders() + 
  tm_format_World(earth.boundary = TRUE, frame = FALSE) +
  tm_layout(between.margin = 0.1)
