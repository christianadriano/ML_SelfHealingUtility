## Control code that can run different machine learning methods

install.packages("xgboost")
install.packages("devtools")
install_git("git://github.com/jpmml/r2pmml.git")

library(devtools)
library(xgboost)
library(r2pmml) #https://github.com/jpmml/r2pmml

# Initialization section ------------------------------------------------------

#Load utility functions
source("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//loadData.R");
source("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//models//xboostRegression.R");
#source("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//models//gbmRegression.R");

  
  #Data structure to keep results
  results.df <- data.frame(matrix(data=NA,nrow=3,ncol=12));
  colnames(results.df) <- c("Item","Utility_Type","Train_RMSE_MEAN","Train_RMSE_STD","Test_RMSE_MEAN",
                            "Test_RMSE_STD","RMSE","R_Squared", "MAPD","User_Time","Sys_Time","Elapsed_Time");
  #Folder with training data
  folder <- "C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//data//DataPoints_1K-3K-9K//";
  #folder <- "//DataPoints_1K-3K-9K//";
  
  # CONTROL CODE   ------------------------------------------------------------
  
  modelList <- c("Linear","Discontinuous","Saturating","ALL");
  modelName <- modelList[1];
  
  datasetSize <- c("1K","3K","9K");
  datasetName <- generateDataSetNames(modelName,datasetSize,0);
  #for(i in c(1:length(datasetName))){
    i <- 1;
    fileName <- paste0(folder,datasetName[i],".csv");
    dataf <- loadData(fileName);
    #data_all <- read.csv(fileName,header = TRUE,sep=",");
    
    featuresdf <- prepareFeatures(dataf,"Saturating");
    
    #Extract training ad validation sets 
    totalData = dim(featuresdf)[1];
    trainingSize = trunc(totalData * 0.7);
    startTestIndex = totalData - trainingSize;
    trainingData<- as.data.frame(featuresdf[1:trainingSize,]);
    validationData<-as.data.frame(featuresdf[startTestIndex:totalData,]);
    
    #Train model  
    outcomeList <- trainModel(trainingData);
    
    #Compute results
    results.df <- validatePredictions(outcomeList,results.df,validationData);
  #}
  
  
#print(results.df); #show on the console

message <- resultsToFile(results.df,modelName,"_70-30_FeatureSelection.csv"); #save to a .csv file
print(message);

generatePMML(outcomeList[[1]],featuresdf,datasetName[i]);#datasetName[length(datasetName)]);





#-----------------------------------------------------------------------------