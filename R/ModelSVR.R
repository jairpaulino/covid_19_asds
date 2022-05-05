getSVRParameters_GridSearch = function(train, valid, grid){
  # train=normTrain; valid=normValid
  grid = list(lag = c(2, 5, 10, 15, 20)
              , kernel = c("radial")
              , gamma = c(1, 0.1, 0.01, 0.001)
              , cost = c(0.1, 1, 100, 1000, 10000)
              , epsilon = c(0.1, 0.01, 0.001)
              , tolerance = c( 0.01, 0.001, 0.0001)
  )
  sink(file = paste("Results/", country, "/" ,"models_", country, "_svrParameters.txt", sep="")
       , append = FALSE, type = "output", split = FALSE); 
  begin = proc.time()
  MSE_min = 1e10; nModels = 0
  for(i in 1:length(grid$lag)){
    for (j in 1:length(grid$kernel)){
      for (k in 1:length(grid$gamma)){
        for (l in 1:length(grid$cost)){
          for(m in 1:length(grid$epsilon)){
            for(n in 1:length(grid$tolerance)){
              #i=1; j=1; k=1; l=1; m=1; n=1
              
              nModels = nModels + 1
              
              svrParameters = list()
              svrParameters$lag = grid$lag[i]
              svrParameters$kernel = grid$kernel[j] 
              svrParameters$gamma = grid$gamma[k] 
              svrParameters$cost = grid$cost[l]
              svrParameters$epsilon = grid$epsilon[m] 
              svrParameters$tolerance = grid$tolerance[n]
              
              train_valid_df = getSlideWindowMatrix(c(train, valid), svrParameters$lag)
              len = length(train_valid_df$target)
              valid_len = length(valid)
              trainNorm_ts = train_valid_df[1:(len-valid_len),]
              validNorm_ts = train_valid_df[(len-valid_len+1):len,]
              #length(valid); length(validNorm_ts$target)
              #plot.ts(valid); lines(validNorm_ts$target, col=2)
              #length(train); length(trainNorm_ts$target)
              
              modelSVR = svm(formula = target ~ .
                             , type = "eps-regression"
                             , x = trainNorm_ts
                             , y = trainNorm_ts$target
                             , kernel = svrParameters$kernel
                             , gamma = svrParameters$gamma
                             , cost = svrParameters$cost
                             , epsilon = svrParameters$epsilon
                             , tolerance = svrParameters$tolerance)
                             
              
              predSVR = predict(modelSVR, validNorm_ts)
              MSE = getMSE(validNorm_ts$target, predSVR)
              
              print(paste0("Iter.: ", nModels ," | lag: ", svrParameters$lag
                           , " | kernel: ", svrParameters$kernel
                           , " | gamma: ",  svrParameters$gamma
                           , " | cost: ", svrParameters$cost
                           , " | epsilon: ", svrParameters$epsilon
                           , " | tolerance: ", svrParameters$tolerance
                           , " | MSE: ", MSE));
              if(is.na(MSE)){
                print(paste0("NA value in ", nModels, " iteration."))
              }else{
                if(MSE < MSE_min){
                  print(paste0(round(MSE_min, 10), " --> ", round(MSE, 10), " - it: ", nModels))
                  MSE_min = MSE
                  bestPar = svrParameters
                }
              }
            }
          }
        }
      }
    }
  }
  end = proc.time()[[3]] - begin[[3]]
  print(paste0("Best parameters: ", list(bestPar)))
  print(paste0("Processing time: ", end))
  
  sink()
  
  rtr = list()
  rtr$bestPar = bestPar
  rtr$MSE_min = MSE_min
  rtr$proc_time = end
  return(rtr)
}  
