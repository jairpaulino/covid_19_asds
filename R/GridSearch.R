epochs = 200

getSVRParameters_GridSearch = function(train, valid, grid){
  # train=train_ts; valid=valid_ts
  #grid = list(lag = c(2, 5, 10, 15, 20)
  #            , kernel = c("radial")
  #            , gamma = c(1, 0.1, 0.01, 0.001)
  #            , cost = c(0.1, 1, 100, 1000, 10000)
  #            , epsilon = c(0.1, 0.01, 0.001)
  #            , tolerance = c( 0.01, 0.001, 0.0001)
  #)
  sink(file = paste("Results/", country, "/" ,"gridSearch_", country, "_SVR_Parameters.txt", sep="")
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
              #train_valid_df[,1:svrParameters$lag] = apply(train_valid_df[,1:svrParameters$lag]
              #                                             , MARGIN = 2, FUN = getNormalizedTS
              #                                             , max = max(train), min = min(train))
              len = length(train_valid_df$target)
              valid_len = length(valid)
              trainNorm_ts = train_valid_df[1:(len-valid_len),]
              validNorm_ts = train_valid_df[(len-valid_len+1):len,]
              #length(valid); length(validNorm_ts$target)
              #plot.ts(valid); lines(validNorm_ts$target, col=2)
              #length(train); length(trainNorm_ts$target)
              
              modelSVR = svm(formula = target ~ .
                             , type = "eps-regression"
                             , x = trainNorm_ts[,1:svrParameters$lag]
                             , y = trainNorm_ts$target
                             , kernel = svrParameters$kernel
                             , gamma = svrParameters$gamma
                             , cost = svrParameters$cost
                             , epsilon = svrParameters$epsilon
                             , tolerance = svrParameters$tolerance)
              
              predSVR = predict(modelSVR, validNorm_ts[,1:svrParameters$lag])
              MSE = getMSE(validNorm_ts$target, predSVR)
              #plot.ts(validNorm_ts$target); lines(predSVR, col=2)
              
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

getELMParameters_GridSearch = function(train, valid, grid){
  # train=train_ts; valid=valid_ts
  # grid = list(lag = c(2, 5)#, 10, 15, 20)
  #             , actfun = c("tansig")# , "sigmoid", "radbas", "relu")
  #             , nhid = c(2, 5, 10, 15, 20)
  # )
  sink(file = paste("Results/", country, "/" ,"gridSearch_", country, "_ELM_Parameters.txt", sep="")
       , append = FALSE, type = "output", split = FALSE); 
  
  begin = proc.time()
  
  MSE_min = 1e10; nModels = 0
  for(i in 1:length(grid$lag)){
    for (j in 1:length(grid$actfun)){
      for (k in 1:length(grid$nhid)){
        #i=1; j=1; k=1; l=1; m=1; n=1
        
        nModels = nModels + 1
        elmParameters = list()
        elmParameters$lag = grid$lag[i]
        elmParameters$actfun = grid$actfun[j]
        elmParameters$nhid = grid$nhid[k]
        
        train_valid_df = getSlideWindowMatrix(c(train, valid), elmParameters$lag)
        #train_valid_df[,1:elmParameters$lag] = apply(train_valid_df[,1:elmParameters$lag]
        #                                             , MARGIN = 2, FUN = getNormalizedTS
        #                                             , max = max(train), min = min(train))
        
        #View(train_valid_df)
        len = length(train_valid_df$target)
        valid_len = length(valid)
        trainNorm_ts = train_valid_df[1:(len-valid_len),]
        validNorm_ts = train_valid_df[(len-valid_len+1):len,]
        #length(valid); length(validNorm_ts$target)
        #plot.ts(valid); lines(validNorm_ts$target, col=2)
        #length(train); length(trainNorm_ts$target)
        
        modelELM = elm(target ~ .
                       , data = trainNorm_ts
                       , nhid = elmParameters$nhid
                       , actfun = elmParameters$actfun)
        
        predELM = predict(modelELM, validNorm_ts)
        MSE = getMSE(validNorm_ts$target, predELM)
        #plot.ts(validNorm_ts$target); lines(predELM, col=2)
        
        print(paste0("Iter.: ", nModels ," | lag: ", elmParameters$lag
                     , " | actfun: ", elmParameters$actfun
                     , " | nhid: ",  elmParameters$nhid
                     , " | MSE: ", round(MSE, 2)));
        if(is.na(MSE)){
          print(paste0("NA value in ", nModels, " iteration."))
        }else{
          if(MSE < MSE_min){
            print(paste0(round(MSE_min, 2), " --> ", round(MSE, 2), " - it: ", nModels))
            MSE_min = MSE
            bestPar = elmParameters
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

getLSTMParameters_GridSearch = function(train, valid, grid){
  # train=train_ts; valid=valid_ts
  # grid = list(lag = c(2, 10, 15, 20)
  #             , optim = c("Adam")
  #             , actfun = c("linear")
  #             , nhid = c(2, 5, 10, 15, 20)
  # )
  
  sink(file = paste("Results/", country, "/" ,"gridSearch_", country, "_LSTM_Parameters.txt", sep="")
       , append = FALSE, type = "output", split = FALSE); 
  
  begin = proc.time()
  
  MSE_min = 1e10; nModels = 0
  for(i in 1:length(grid$lag)){
    for (j in 1:length(grid$optim)){
      for (k in 1:length(grid$actfun)){
        for (l in 1:length(grid$nhid)){
          #i=1; j=1; k=1; l=1
          
          nModels = nModels + 1
          lstmParameters = list()
          lstmParameters$lag = grid$lag[i]
          lstmParameters$optim = grid$optim[j]
          lstmParameters$actfun = grid$actfun[k]
          lstmParameters$nhid = grid$nhid[l]
          
          train_valid_df = getSlideWindowMatrix(c(train, valid), lstmParameters$lag)
          #train_valid_df[,1:(lstmParameters$lag+1)] = apply(train_valid_df[,1:(lstmParameters$lag+1)]
          #                                             , MARGIN = 2, FUN = getNormalizedTS
          #                                             , max = max(train), min = min(train))
          #View(train_valid_df)
          len = length(train_valid_df$target)
          valid_len = length(valid)
          trainNorm_ts = train_valid_df[1:(len-valid_len),]
          validNorm_ts = train_valid_df[(len-valid_len+1):len,]
          #length(valid); length(validNorm_ts$target)
          #plot.ts(valid); lines(validNorm_ts$target, col=2)
          #length(train); length(trainNorm_ts$target)
          
          covariates_train = as.matrix(trainNorm_ts[,1:lstmParameters$lag])
          dim(covariates_train) = c(dim(covariates_train), 1)
          target_train = as.matrix(trainNorm_ts$target)
          covariates_valid = as.matrix(validNorm_ts[,1:lstmParameters$lag])
          dim(covariates_valid) = c(dim(covariates_valid), 1)
          target_valid = as.matrix(validNorm_ts$target)
          
          modelLSTM = keras_model_sequential()
          modelLSTM %>%
            layer_lstm(units = lstmParameters$nhid
                       , input_shape = dim(covariates_train)[-1]
                       , return_sequences = FALSE) %>%
            layer_dense(units = 1, lstmParameters$actfun)
          
          modelLSTM %>% 
            compile(loss = 'mse'
                    , optimizer = lstmParameters$optim
                    , metrics = 'mse')
          
          early_stopping = callback_early_stopping(monitor = 'val_loss', patience = 5)
          
          # early_stopping = callback_reduce_lr_on_plateau(
          #   monitor = "val_loss",
          #   factor = 0.1,
          #   patience = 5,
          #   verbose = 0,
          #   mode = c("auto", "min", "max"),
          #   min_delta = 1e-04,
          #   cooldown = 0,
          #   min_lr = 0
          # )
          # 
          
          history = modelLSTM %>% 
            fit(x = covariates_train
                , y = target_train
                , callbacks = c(early_stopping)
                , epochs = epochs
                #, shuffle = FALSE
                , batch_size = 1
                , validation_split = 0.25
                #, validation_data = list(covariates_valid, target_valid)
                , verbose = TRUE)
          
          predLSTM = predict(modelLSTM, covariates_valid, rep=10)
          MSE = getMSE(target_valid, predLSTM)
          #plot.ts(target_valid); lines(predLSTM, col=2)
          #plot.ts(predLSTM)
          
          print(paste0("Iter.: ", nModels ," | lag: ", lstmParameters$lag
                       , " | optim: ", lstmParameters$optim
                       , " | actfun: ",  lstmParameters$actfun
                       , " | nhid: ",  lstmParameters$nhid
                       , " | MSE: ", round(MSE, 5)));
          if(is.na(MSE)){
            print(paste0("NA value in ", nModels, " iteration."))
          }else{
            if(MSE < MSE_min){
              print(paste0(round(MSE_min, 5), " --> ", round(MSE, 5), " - it: ", nModels))
              MSE_min = MSE
              bestPar = lstmParameters
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

getMLPParameters_GridSearch = function(train, valid, grid){
  # train= normTrain; valid=normValid
  # grid = list(lag = c(14)#2, 10, 15, 20)
  #             , actfun = c("relu", "sigmoid")
  #             , nhid = c(10, 25, 50, 100)
  #             , optim = c('adam'))
  
  sink(file = paste("Results/", country, "/" ,"gridSearch_", country, "_MLP_Parameters.txt", sep="")
       , append = FALSE, type = "output", split = FALSE); 
  
  begin = proc.time()
  
  MSE_min = 1e10; nModels = 0
  for(i in 1:length(grid$lag)){
    for (j in 1:length(grid$optim)){
      for (k in 1:length(grid$actfun)){
        for (l in 1:length(grid$nhid)){
          #i=1; j=1; k=1; l=1
          
          nModels = nModels + 1
          mlpParameters = list()
          mlpParameters$lag = grid$lag[i]
          mlpParameters$optim = grid$optim[j]
          mlpParameters$actfun = grid$actfun[k]
          mlpParameters$nhid = grid$nhid[l]
          
          train_valid_df = getSlideWindowMatrix(c(train, valid), mlpParameters$lag)
          #train_valid_df[,1:(lstmParameters$lag+1)] = apply(train_valid_df[,1:(lstmParameters$lag+1)]
          #                                                  , MARGIN = 2, FUN = getNormalizedTS
          #                                                 , max = max(train), min = min(train))
          
          #View(train_valid_df)
          len = length(train_valid_df$target)
          valid_len = length(valid)
          trainNorm_ts = train_valid_df[1:(len-valid_len),]
          validNorm_ts = train_valid_df[(len-valid_len+1):len,]
          #length(valid); length(validNorm_ts$target)
          #plot.ts(valid); lines(validNorm_ts$target, col=2)
          #length(train); length(trainNorm_ts$target)
          
          covariates_train = as.matrix(trainNorm_ts[,1:mlpParameters$lag])
          train_target = trainNorm_ts$target
          covariates_valid = as.matrix(validNorm_ts[,1:mlpParameters$lag])
          valid_target = validNorm_ts$target
          
          modelMLP = keras_model_sequential()
          modelMLP %>%
            layer_dense(units = mlpParameters$nhid
                        , input_shape = c(mlpParameters$lag) 
                        , activation = mlpParameters$actfun) %>%
            layer_dense(units = 1, activation = 'linear')
          
          modelMLP %>% 
            compile(loss = 'mse'
                    , optimizer = mlpParameters$optim
                    , metrics = 'mse')
          
          early_stopping = callback_early_stopping(monitor = 'val_loss'
                                                   , patience = 5)
          
          history = modelMLP %>% 
            fit(x = covariates_train
                , y = train_target
                , callbacks = c(early_stopping)
                , epochs = epochs
                , shuffle = FALSE
                , batch_size = 1
                , validation_split = 0.25
                , verbose = TRUE)
             
          predMLP = predict(modelMLP, covariates_valid, rep=10)
          MSE = getMSE(valid_target, predMLP)
          
          #plot.ts(target_valid); lines(predMLP, col=2)

          print(paste0("Iter.: ", nModels ," | lag: ", mlpParameters$lag
                       , " | optim: ", mlpParameters$optim
                       , " | actfun: ",  mlpParameters$actfun
                       , " | nhid: ",  mlpParameters$nhid
                       , " | MSE: ", round(MSE, 5)));
          if(is.na(MSE)){
            print(paste0("NA value in ", nModels, " iteration."))
          }else{
            if(MSE < MSE_min){
              print(paste0(round(MSE_min, 5), " --> ", round(MSE, 5), " - it: ", nModels))
              MSE_min = MSE
              bestPar = mlpParameters
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
