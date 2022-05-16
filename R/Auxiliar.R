generateGraphics = function(results_df, n=30){
  #results_df = results_df; n=70
  len = length(results_df$Target)
  max_ = max(results_df); min_ = min(results_df)
  plot.ts(results_df$Target[(len-n):len], lwd=3, ylim=c(min_, max_)
          , ylab=country, xlab="Index (test set)")
  for(i in 2:length(results_df)){#i=5
    lines(results_df[[i]][(len-n):len], col=i, lwd=3)
    points(results_df[[i]][(len-n):len], col=i, lwd=2, pch=i)
  }
  legend("top", c(names(results_df)), lwd=2, col=c(1:length(results_df))
         , inset = 0.01, horiz = T, pch=c(NA, 2:length(results_df))
         , box.col = "white", cex = 0.9)
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

getNormalizedTS = function(array, min, max, lim_inf=0, lim_sup=1){
  #array = 1:10; min = min(array); max=max(array); lim_inf=-1;lim_sup=1
  #Normalize to [0, 1]
  range = max - min
  norm1 = (array - min) / range
  #Then scale to [x,y]
  range2 = lim_sup - lim_inf
  normalized = (norm1*range2) + lim_inf #(conferir)
  return(normalized)
}

getDenormalizedTS = function(array_norm, min, max, lim_inf=0, lim_sup=1){
  #array = 1:10; min = min(array); max=max(array); lim_inf=-1;  lim_sup=1
  #array_norm = seq(-1,1,0.222222222); plot.ts(array_norm)
  #Normalize to [0, 1]
  range = max - min
  range2 = lim_sup - lim_inf
  array = (array_norm-lim_inf)*range/range2+min
  return(array)
}

delete_zero = function(time_series){
  #time_series = incDia_ts[[2]]
  for(i in 1:length(time_series)){
    if(time_series[i] < 0){
      time_series[i] = 0
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
