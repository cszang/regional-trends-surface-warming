## detrend a single data frame

detrend_df <- function(df) {
  m <- lm(temp ~ year, data = df)
  tibble(
    year = df$year,
    temp = residuals(m)
  )
}
