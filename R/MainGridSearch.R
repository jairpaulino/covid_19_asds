#Title: SVR GridSearch to COVID-19 forecasting
#Date: Mai/2022

# Setup ----
# Cleaning R environment
rm(list=ls()); graphics.off() 
#sourceMode <<- getSourceMode(isToDebug)

# Libraries
if(!require("e1071")) install.packages("e1071")
# Importing functions
source("R/Auxiliar.R")
source("R/ModelSVR.R")


countries= c("Brazil")
models2Run = list(SVR = TRUE)

begin_all = proc.time()
for(i in 1:length(countries)){
  
  country = countries[i] #i=1
  
  if(!dir.exists(file.path(getwd(), paste0("Results/", country)))){
    dir.create(file.path(getwd(), paste0("Results/", country)))
  }
  
  incDia = read.csv(paste0("Data/", country, "_daily.csv"), sep=",")
  plot.ts(incDia[country])
  cut_pos = which(grepl("12/31/21", incDia$X))
  incDia_ts = delete_zero(incDia[1:cut_pos,])
  #plot.ts(incDia_ts[country])
  
  # Split data into train_valid - test
  len = length(incDia_ts[[country]])
  train_perc = round(len*0.7, 0) 
  valid_perc = train_perc + round(len*0.2, 0)  
  test_perc = valid_perc + round(len*0.1, 0)  
  train_ts =  incDia_ts[[country]][1:train_perc]
  valid_ts =  incDia_ts[[country]][(train_perc+1):valid_perc]
  test_ts =  incDia_ts[[country]][(valid_perc+1):len]
  #len==length(train_ts)+length(valid_ts)+length(test_ts)
  
  if(models2Run$SVR==TRUE){
    
    grid = list(gamma = c(0.001, 1)
                , cost = c(0.1, 1, 100)
                , tolerance = c(0.001, 0.01, 0.1)
                , kernel = c("radial"))#, "linear"))
    
    SVR_parameters = getSVRParameters_GridSearch()
  }
  
}
end_all = proc.time() - begin_all
