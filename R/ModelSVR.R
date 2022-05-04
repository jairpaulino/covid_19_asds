getSVRParameters_GridSearch = function(train, valid, grid){
  #train=train_ts; valid=valid_ts; test=test_ts
  # grid = list(gamma = c(0.001, 1)
  #             , cost = c(0.1, 1, 100)
  #             , tolerance = c(0.001, 0.01, 0.1)
  #             , kernel = c("radial"))#, "linear"))
  
  getSlideWindowMatrix(1:100, 7)
  
  
  MSE_min = 1e10; bestPar = list(); nModels = 0
  for(a in 1:length(grid$gamma)){
    for (i in 1:length(grid$cost)){
      for (j in 1:length(grid$tolerance)){
        for (k in 1:length(grid$kernel)){#i=1; j=1; k=1
          
          nModels = nModels + 1
          
          svrParameters = list()
          svrParameters$gamma = gamma[a]
          svrParameters$cost = cost[i] 
          svrParameters$tolerance = tolerance[j] 
          svrParameters$kernel = kernel[k] 
          
          modelSVM = svm(formula = nStepAhead ~ .
                         , type = "eps-regression"
                         , x = dataTrain_fit_model
                         , y = dataTrain_fit_model$nStepAhead
                         , gamma = as.numeric(svmParameters[1])
                         , cost = as.numeric(svmParameters[2])
                         , epsilon = as.numeric(svmParameters[3])
                         , kernel = svmParameters[4])
          
          predSVM_train = predict(modelSVM, dataTrain_fit_model)
          
          dataValid_fit_model = dataValid_fit[,c(1:w,w+6)]
          predSVM_valid = predict(modelSVM, dataValid_fit_model)
          MSE = getMSE(dataValid_fit_model$nStepAhead
                       , predSVM_valid)
          
          print(paste0("----------------- It. ", nModels ," -----------------"));
          print(svmParameters)
          print(paste0("MSE: ", MSE))
          
          if(is.na(MSE)){
            print(paste0("NA value in ", nModels, " iteration."))
          }else{
            if(MSE < MSE_min){
              print(paste0(round(MSE_min, 6), " --> ", round(MSE, 6), " - it: ", nModels))
              MSE_min = MSE
              bestPar$gamma = as.numeric(svmParameters[1])
              bestPar$cost = as.numeric(svmParameters[2])
              bestPar$epsilon = as.numeric(svmParameters[3])
              bestPar$kernel = svmParameters[4]
              bestModelSVM = modelSVM
            }
          }
        }
      }
    }
  }
  
  
  
  

}