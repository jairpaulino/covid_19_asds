#Title: SVR GridSearch to COVID-19 forecasting
#Date: Mai/2022

# Setup ----
# Cleaning R environment
rm(list=ls()); graphics.off() 
#sourceMode <<- getSourceMode(isToDebug)

# Libraries
if(!require("e1071")) install.packages("e1071")
if(!require("elmNNRcpp")) install.packages("elmNNRcpp")

# Importing functions
source("R/Auxiliar.R")
source("R/GridSearch.R")
source("R/performanceMetrics.R")

countries= c("Brazil")#"Argentina", "Australia", "Brazil", "Canada", "China")
models2Run = list(SVR = TRUE, ELM = TRUE)

begin_all = proc.time()
for(i in 1:length(countries)){
  
  country = countries[i] #i=1
  
  if(!dir.exists(file.path(getwd(), paste0("Results/", country)))){
    dir.create(file.path(getwd(), paste0("Results/", country)))
  }
  
  incDia = read.csv(paste0("Data/", country, "_daily.csv", sep=""), sep=",")
  #plot.ts(incDia[country])
  cut_pos = which(grepl("12/31/21", incDia$X))
  incDia_ts = delete_zero(incDia[1:cut_pos,])
  plot.ts(incDia_ts[country])
  
  # Split data into train_valid - test
  len = length(incDia_ts[[country]])
  train_perc = round(len*0.7, 0) 
  valid_perc = train_perc + round(len*0.2, 0)  
  test_perc = valid_perc + round(len*0.1, 0)  
  train_ts =  incDia_ts[[country]][1:train_perc]
  valid_ts =  incDia_ts[[country]][(train_perc+1):valid_perc]
  test_ts =  incDia_ts[[country]][(valid_perc+1):len]
  #len==length(train_ts)+length(valid_ts)+length(test_ts)
  
  # MinMax Normalisation
  min_train = min(train_ts); max_train = max(train_ts)
  normTrain = getNormalizeTS(train_ts, min=min_train, max=max_train)
  normValid= getNormalizeTS(valid_ts, min=min_train, max=max_train)
  normTest = getNormalizeTS(test_ts, min=min_train, max=max_train)
  plot.ts(c(normTrain, normValid, normTest))
  abline(v=length(normTrain), col=2, lwd=2)
  abline(v=(length(normValid)+length(normTrain)), col=4, lwd=3)
  
  if(models2Run$SVR==TRUE){
    grid = list(lag = c(14)#2, 5, 10, 15, 20)
                , kernel = c("radial", "linear")
                , gamma = c(1, 0.1, 0.01, 0.001)
                , cost = c(0.1, 1, 100, 1000, 10000)
                , epsilon = c(0.1, 0.01, 0.001)
                , tolerance = c( 0.01, 0.001, 0.0001)
                )
    SVR_parameters = getSVRParameters_GridSearch(normTrain, normValid, grid)
    write.csv(SVR_parameters, file = paste0("Results/", country, "/", country, "_SVR_Parameters.csv"), row.names = F)
  }
  
  if(models2Run$ELM==TRUE){
    grid = list(lag = c(14)#, 10, 15, 20)
                , actfun = c("tansig", "relu")#, "sigmoid", "radbas")
                , nhid = c(2, 5, 10, 15, 20)
                )
    ELM_parameters = getELMParameters_GridSearch(normTrain, normValid, grid)
    write.csv(ELM_parameters, file = paste0("Results/", country, "/", country, "_ELM_Parameters.csv"), row.names = F)
  }
  
  
}
end_all = proc.time() - begin_all
