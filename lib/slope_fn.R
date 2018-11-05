## the defaults for these functions stick to the hiatus definition in
## Rahmstorf et al. 2017

## do Chow test for single data frame
chow_test <- function(x, wp = 1972:2000, hp = 2001:2014,
                     time_var = "year", check_var = "temp") {
  cp <- hp[1]
  all_years <- c(wp, hp) %>% sort
  .x <- data.frame(
    .year = x[[time_var]],
    .var = x[[check_var]]
  )
  .x <- .x %>% filter(.year %in% all_years)
  .cp <- which(.x$.year == cp)
  .F <- tryCatch(sctest(.var ~ .year, data = .x, type = "Chow", point = .cp)$p.value,
                error = function(e) e)
  if (any(class(.F) == "error")) {
    NA
  } else {
    .F
  }
}


## Compare slopes for single data frame
## Significance test is done based on Monte-Carlo-Simulation: we
## generate n series with the same global trend (full WP + HP periods)
## and add the same white noise structure; then we check for the 95%
## of possible trends based on the distribution of slope changes
## between HP and WP.

## see below for functions and global vars in lieue of real status bar

slope_diff <- function(x, wp = 1972:2000, hp = 2001:2014,
                      check_sig = TRUE, n = 1000,
                      use_vars = c("year", "temp"), 
                      parallel = TRUE) {
  if (!PARALLEL) {
    SCOUNTER <<- SCOUNTER + 1
    cat(round(SCOUNTER/NOBS * 100, 2), "%\n")
  }
  years <- x[[use_vars[1]]]
  the_var <- x[[use_vars[2]]]
  hp_subset <- years %in% hp
  wp_subset <- years %in% wp
  total_set <- hp_subset | wp_subset
  all_years <- years[total_set]
  hp_period <- all_years %in% hp
  wp_period <- all_years %in% wp
  pt <- the_var[total_set]
  hpt <- the_var[hp_subset]
  wpt <- the_var[wp_subset]
  HP <- cbind(1, hp)
  WP <- cbind(1, wp)
  slope_hp <- qr.solve(HP, hpt)[2]
  slope_wp <- qr.solve(WP, wpt)[2]
  slope_delta <- slope_hp - slope_wp
  if (!check_sig) {
    data.frame(slope_delta)
  } else {
    AP <- cbind(1, all_years)
    global_qr <- qr.solve(AP, pt)
    global_slope <- global_qr[2]
    global_intercept <- global_qr[1]
    global_prediction <- global_slope * all_years + global_intercept
    global_sd <- sd(pt)
    global_var <- global_sd^2
    trend_sd <- sd(global_prediction)
    trend_var <- trend_sd^2
    mc_sd <- sqrt(global_var - trend_var)
    m <- length(global_prediction)
    mc_series <- sapply(1:n, function(v) {
      global_prediction + rnorm(m, sd = mc_sd)
    })
    mchpt <- mc_series[hp_period,]
    mcwpt <- mc_series[wp_period,]
    mc_wpslopes <- sapply(1:n, function(v) {
      .series <- mcwpt[,v]
      qr.solve(WP, .series)[2]
    })
    mc_hpslopes <- sapply(1:n, function(v) {
      .series <- mchpt[,v]
      qr.solve(HP, .series)[2]
    })
    mc_slopes_delta <- mc_hpslopes - mc_wpslopes
    delta_sig <- phipson_smith_test(slope_delta, mc_slopes_delta)
    data.frame(slope_delta, delta_sig)
  }
}

# global var for counter
SCOUNTER <- 0
# global for % complete (refers to n(obs))
NOBS <- NA

reset_counter <- function() {
  SCOUNTER <<- 0
}

set_nobs <- function(x) {
  NOBS <<- x
}
