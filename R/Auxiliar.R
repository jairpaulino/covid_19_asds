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

