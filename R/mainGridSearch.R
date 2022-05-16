#Title: SVR, ELM and LSTM GridSearch to COVID-19 forecasting
#Date: Mai/2022

# Setup ----
# Cleaning R environment
rm(list=ls()); graphics.off() 
#sourceMode <<- getSourceMode(isToDebug)

# Libraries
if(!require("e1071")) install.packages("e1071")
if(!require("elmNNRcpp")) install.packages("elmNNRcpp")
if(!require("keras")) install.packages("keras")
if(!require("tensorflow")) install.packages("tensorflow")
if(!require("outliers")) install.packages("outliers")

# Importing functions
source("R/Auxiliar.R")
source("R/GridSearch.R")
source("R/performanceMetrics.R")
source("R/CreateModels.R")

countries= c("Argentina", "Australia", "Brazil", "Canada", "France"
             , "Germany", "India", "Iran", "Israel", "Italy")
#Argentina, Australia, Brazil, Canada, France, Germany, India, Iran, Israel, Italy,
models2Run = list(SVR = TRUE, ELM = TRUE, LSTM = TRUE, MLP = TRUE)
toGrid = list(SVR = T, ELM = T, LSTM = T, MLP = T)

begin_all = proc.time()
for(i in 1:length(countries)){
  
  country = countries[i] #i=1
  
  if(!dir.exists(file.path(getwd(), paste0("Results/", country)))){
    dir.create(file.path(getwd(), paste0("Results/", country)))
  }
  incDia = read.csv(paste0("Data/", country, "_daily.csv", sep=""), sep=",")
  #plot.ts(incDia[[2]])
  cut_pos = which(grepl("11/30/21", incDia$X))
  incDia_ts = incDia[1:cut_pos,]
  incDia_ts[[2]] = delete_zero(incDia_ts[[2]])
  #incDia_ts = treatingOutliers(incDia_ts[[2]])
  #plot.ts(incDia_ts[[2]])
  #acf(incDia_ts[[2]]); pacf(incDia_ts[[2]])
  
  # Split data into train_valid - test
  incDia_ts = incDia_ts[[2]]; plot.ts(incDia_ts)
  len = length(incDia_ts)
  train_perc = round(len*0.7, 0) 
  valid_perc = train_perc + round(len*0.2, 0)  
  test_perc = valid_perc + round(len*0.1, 0)  
  train_ts = incDia_ts[1:train_perc]
  #train_ts = diff(train_ts_normal); plot.ts(train_ts)
  valid_ts =  incDia_ts[(train_perc+1):valid_perc]
  #valid_ts = diff(valid_ts_normal); plot.ts(valid_ts)
  test_ts = incDia_ts[(valid_perc+1):len]
  #test_ts = diff(test_ts_normal); plot.ts(test_ts)
  
  #len==length(train_ts)+length(valid_ts)+length(test_ts)
  
  # MinMax Normalisation
  max_train = max(train_ts); min_train = min(train_ts); 
  normTrain = getNormalizedTS(train_ts, min=min_train, max=max_train, lim_inf=(-1), lim_sup=(1))
  normValid = getNormalizedTS(valid_ts, min=min_train, max=max_train, lim_inf=(-1), lim_sup=(1))
  normTest = getNormalizedTS(test_ts, min=min_train, max=max_train, lim_inf=(-1), lim_sup=(1))

  plot.ts(c(normTrain, normValid, normTest), col=2, lty=2)#, ylim=c(-1,1))
  #abline(v=length(normTrain), col=2, lwd=2)
  #abline(v=(length(normValid)+length(normTrain)), col=4, lwd=3)
  
  if(models2Run$SVR == TRUE){
    if(toGrid$SVR == TRUE){
      grid = list(lag = c(14)#2, 5, 10, 15, 20)
                  , kernel = c("linear", "radial")
                  , gamma = c(1, 0.1, 0.01)#, 0.001)
                  , cost = c(0.1, 1, 100)#, 1000)#, 10000)
                  , epsilon = c(0.1, 0.01, 0.001)
                  , tolerance = c(0.01, 0.001, 0.0001))
      SVR_parameters = getSVRParameters_GridSearch(normTrain, normValid, grid)
      write.csv(SVR_parameters, file = paste0("Results/", country, "/", country
                                                          , "_SVR_Parameters.csv"), row.names = F)
    }
    SVR_parameters = read.csv(paste0("Results/", country, "/", country, "_SVR_Parameters.csv"))
    results_SVR = SVR_predict(train = c(normTrain, normValid), test = normTest
                              , parameters = SVR_parameters)
  }
  
  if(models2Run$ELM == TRUE){
    if(toGrid$ELM == TRUE){
      grid = list(lag = c(14)#2, 10, 15, 20)
                  , actfun = c("relu", "tansig", "sig")#, "sigmoid", "radbas")
                  , nhid = c(10, 20, 50, 100, 150, 200)
    )
    ELM_parameters = getELMParameters_GridSearch(normTrain, normValid, grid)
    write.csv(ELM_parameters, file = paste0("Results/", country, "/", country, "_ELM_Parameters.csv"), row.names = F)
    }
    ELM_parameters = read.csv(paste0("Results/", country, "/", country
                                       , "_ELM_Parameters.csv"))
    results_ELM = ELM_predict(train = c(normTrain, normValid), test = normTest
                              , parameters = ELM_parameters)
  }
  
  if(models2Run$LSTM == TRUE){
    if(toGrid$LSTM == TRUE){
      grid = list(lag = c(14)#2, 10, 15, 20)
                  , optim = c("adam", "rmsprop")
                  , actfun = c("linear", "relu", "sigmoid")
                  , nhid = c(10, 30, 60, 100, 200)#2, 5, 10, 15, 20)
      )
      LSTM_parameters = getLSTMParameters_GridSearch(normTrain, normValid, grid)
      write.csv(LSTM_parameters, file = paste0("Results/", country, "/", country, "_LSTM_Parameters.csv"), row.names = F)
    }
    LSTM_parameters = read.csv(paste0("Results/", country, "/", country
                                       , "_LSTM_Parameters.csv"))
    results_LSTM = LSTM_predict(train = c(normTrain, normValid), test = normTest
                                , parameters = LSTM_parameters)
  }
  
  if(models2Run$MLP == TRUE){
    if(toGrid$MLP == TRUE){
      grid = list(lag = c(14)#2, 10, 15, 20)
                  , actfun = c("linear", "relu", "sigmoid")
                  , nhid = c(10, 30, 60, 100, 200)
                  , optim = c('adam', 'rmsprop'))
      MLP_parameters = getMLPParameters_GridSearch(normTrain, normValid, grid)
      write.csv(MLP_parameters, file = paste0("Results/", country, "/", country, "_MLP_Parameters.csv"), row.names = F)
    }
    MLP_parameters = read.csv(paste0("Results/", country, "/", country, "_MLP_Parameters.csv"))
    results_MLP = MLP_predict(train = c(normTrain, normValid)
                              , test = normTest
                              , parameters = MLP_parameters)
  }

  results_df = data.frame(matrix(ncol=1, nrow = length(normTest)))
  colnames(results_df) = c("Target")
  results_df$Target = normTest
  if(models2Run$SVR){
    results_df$SVR = results_SVR$predict
  }
  if(models2Run$ELM){
    results_df$ELM = results_ELM$predict
  }
  if(models2Run$LSTM){
    results_df$LSTM = results_LSTM$predict
  }
  if(models2Run$MLP){
    results_df$MLP = results_MLP$predict
  }
  #head(results_df)
  #results_df = data.frame(apply(results_df, MARGIN = 2, FUN = delete_zero))
  ## DESNORMALIZAR
  results_df = data.frame(apply(results_df, MARGIN = 2, FUN = getDenormalizedTS,
                                min=min_train, max=max_train, lim_inf=(-1), lim_sup=(1)))
  #results_df = data.frame(apply(results_df, MARGIN = 2, FUN = diffinv, xi = test_ts_normal[1]))
  #
  png(filename = paste0("Results/", country, "/", country, "_Test.png")
      , width = 800, height = 600, res = 100)
  generateGraphics(results_df, length(results_df$Target))    
  dev.off()
  
  metrics = calculateMetrics(results_df)
  write.csv(metrics, file = paste0("Results/", country, "/", country, "_Metrics.csv"), row.names = T)

  options(scipen=999)
  write.csv(results_df, file = paste0("Results/", country, "/", country, "_Results.csv"), row.names = F)
  
}
end_all = proc.time() - begin_all

#
#c = "Canada"
#df = read.csv(paste0("Results/", c, "/", c, "_Metrics.csv")); df
# a = AirPassengers
# b = diff(a); plot.ts(b)
# c = diffinv(b, 1, xi=AirPassengers[1]); plot.ts(c); lines(AirPassengers, col=2, lty=2)
# length(c)
# 
# plot.ts(normTest)
# lines(results_df$Target, col=2)
# plot.ts(diffinv(normTest, xi=normTest[1]))
# plot.ts(test_ts)
