generateGraphics = function(results_df){
  #results_df = results_df
  plot.ts(results_df$Target, lwd=2)
  for(i in 2:length(results_df)){
    lines(results_df[[2]], col=i, lwd=2)
  }
  legend("topleft", c(names(results_df)), lwd=2, col=c(1:length(results_df))
         , inset = 0.01)
}

treatingOutliers = function(time_series){
  time_series = incDia[[2]]# incDia_ts[[2]]
  out = boxplot.stats(time_series, coef = 1.5)$out
  out_ind = which(time_series %in% c(out))
  for(i in 1:length(out_ind)){
    time_series[out_ind[i]] = time_series[out_ind[i]-1]
  }#plot.ts(time_series)
  return(time_series)
}

getNormalizeTS = function(array, min, max){#, lim_inf=0, lim_sup=1){
  #Normalize to [0, 1]
  range = max - min
  norm1 = (array - min) / range
  
  #Then scale to [x,y]
  #range2 = lim_sup - lim_inf
  #normalized = (norm1*range2) + lim_inf (conferir)
  return(norm1)
}

delete_zero = function(time_series){
  #time_series=incDia_ts
  for(i in 1:length(time_series[[2]])){
    if(time_series[i, 2] < 0){
      time_series[i, 2] = 0
    }
  }
  return(time_series)
}

getSlideWindowMatrix = function(time_series, lag){
  
  df = data.frame(matrix(ncol=(lag+1)))
  names_lag = NULL
  for(i in 1:lag){names_lag[i] = paste("x_", i, sep="")}
  colnames(df) = c(names_lag, 'target')
  
  for(i in 1:(length(time_series)-lag)){
    df[i, ] = time_series[(i):(i+lag)]
  }
  return(df)
}

