# calculates correction factor (lambda) for chi-square testswith spatial data. 
# Modified Tests of Independence in 2 x 2 Tables with Spatial Data Andrea Cerioli 
# Istituto di Statistica, Universita di Parma, Via Kennedy 6, 43100 Parma, Italy
# Biometrics

correction_factor<- function(data, var1, var2){
  # data must have the following columns 
  # lon: longitude
  # lat: latitude
  # var1: name of spatial (binary) process (i.e., significant/not significant: 1/0)
  # var2: name of second spatial (binary) process
  # name should be desired column name 

  #have to separate data points into distance classes/bins
  get_bins<- function(data){
    sp_data<- data
    coordinates(sp_data)<- ~lon+lat
    
    v_x<- variogram(eval(as.name(var1))~1,sp_data)
    v_x_fit<- fit.variogram(v_x, vgm("Mat")) # matern tends to perform well
    max_lag_x<- v_x_fit$range[2]
    
    v_y<- variogram(eval(as.name(var2))~1,sp_data)
    v_y_fit<- fit.variogram(v_y, vgm("Mat")) # matern tends to perform well
    max_lag_y<- v_y_fit$range[2]
    
    D<- min(max_lag_x, max_lag_y) 
    
    bins<- c(0,v_x$dist[1:min(which(v_x$dist>D))])
    return(bins)
  }
  
  # calculate distance between data points
  dists<- data %>% ungroup %>%  dplyr::select(lat, lon) %>% dist %>% as.matrix
  
  #gets neigbhors within the specified bins, at distance lag 1:(length(bins)-1)
  get_neighbors<- function(dists, bins, d){
    neighbors<- which(dists <= bins[d+1] & dists > bins[d], arr.ind=T)
    neighbors<- neighbors[neighbors[,1]!=neighbors[,2],]
    neighbors<- data_frame(row1 = neighbors[,1], row2 = neighbors[,2])
    # row1,row2 = row2,row1 so we get rid of these duplicates
    # this should reduce points by a half 
    neighbors<- neighbors %>% arrange(row1) %>% filter(row1<row2) 
    return(neighbors)
  }
  
  # sample proportions, as in paper... 
  # 1 indicates significance, 0 indicates no significance
  # so pi_11 are the proportion of grid cells with significance in 
  # both X and Y
  sig_x<- data[var1]
  sig_y<- data[var2]
  n<- nrow(data)
  pi_hat<- c(pi_11 = sum(sig_x & sig_y), 
             pi_10 = sum(sig_x & !sig_y), 
             pi_01 = sum(!sig_x & sig_y),
             pi_00 = sum(!sig_x & !sig_y))/n
  
  bins<- suppressWarnings(get_bins(data))
  # using greater circle distances is an alternative
  # for this purposes it should not matter much...
  pi_x<- sum(pi_hat[1:2])
  pi_y<- sum(pi_hat[c(1,3)])
  
  gamma_fn<- function(data,neighbors,var1, var2,pi_x, pi_y){
    #x_or_y is object given by neighbours function
    xs<- data[neighbors$row1,][var1]
    xt<- data[neighbors$row2,][var1]
    out_x<- sum((xs-pi_x) * (xt - pi_x))/nrow(neighbors)
    ys<- data[neighbors$row1,][var2]
    yt<- data[neighbors$row2,][var2]
    out_y<- sum((xs-pi_y) * (xt - pi_y))/nrow(neighbors)
    return(list(gamma_x = out_x, gamma_y = out_y))
  }
  
  lambda_hat<- function(data, neighbors, var1, var2, pi_x, pi_y, bins, dists){
    constant<- (2/(n * pi_hat['pi_11'] * pi_hat['pi_00']))
    sum_d<- 0
    for (d in 1:(length(bins)-1)){
      neighbors<- get_neighbors(dists, bins,d)
      tmp_g<- gamma_fn(data, neighbors, var1, var2, pi_x, pi_y)
      sum_d<- sum_d + tmp_g[[1]]*tmp_g[[2]]*nrow(neighbors)
    }
    out<- constant * sum_d
    return(out)
  }
  lambda<- lambda_hat(data, neighbors, var1, var2, pi_x, pi_y, bins, dists)
  return(unname(lambda))
}
