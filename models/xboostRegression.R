#Train xBoostTree model to predict Utility Function
#Ten-fold cross-validation
#Trains model with different dataset sizes 1k 3K 9K
#
#Validate model with holdout dataset
#
#Convert xgboost to jpmml
#https://github.com/jpmml/jpmml-xgboost

#Automatic model selection
#https://stats.stackexchange.com/questions/20836/algorithms-for-automatic-model-selection/20856#20856
#https://stats.stackexchange.com/questions/218208/what-are-the-advantages-of-stepwise-regression

install.packages("xgboost")
install.packages("devtools")
install_git("git://github.com/jpmml/r2pmml.git")

library(devtools)
library(xgboost)
library(r2pmml) #https://github.com/jpmml/r2pmml

# Initialization section ------------------------------------------------------

#Load utility functions
source("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//loadData.R");

#Data structure to keep results
mcResultsf <- data.frame(matrix(data=NA,nrow=3,ncol=8));
colnames(mcResultsf) <- c("DataSet","Train_RMSE_MEAN","Train_RMSE_STD","Test_RMSE_MEAN",
                          "Test_RMSE_STD","RMSE","R_Squared", "MAPD");
#Folder with training data
folder <- "C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//data//DataPoints_1K-3K-9K//";
#folder <- "//DataPoints_1K-3K-9K//";

# CONTROL CODE   ------------------------------------------------------------

modelList <- c("Linear","Discontinuous","Saturating","ALL");
modelName <- modelList[3];

datasetSize <- c("1K","3K","9K");
datasetName <- generateDataSetNames(modelName,datasetSize,0);
for(i in c(1:length(datasetName))){
  #i <- 2;
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
  mcResultsf <- validatePredictions(outcomeList,mcResultsf,validationData);
}

#print(mcResultsf); #show on the console

resultsToFile(mcResultsf,modelName,"_70-30_FeatureSelection.csv"); #save to a .csv file


generatePMML(outcomeList[[1]],featuresdf,datasetName[i]);#datasetName[length(datasetName)]);

#-------------------------------------------------------------------------------------------------


# Generate the dataset names that will be trained -------------------------
generateDataSetNames <- function(modelName,datasetSize,s_idx){
  
  if(s_idx==0 & length(datasetSize)>0){#Generate for all sizes
    datasetName <- paste0(modelName,datasetSize[1]);
    for(i in c(2:length(datasetSize))){
      datasetName <- cbind(datasetName,paste0(modelName,datasetSize[i]));
    }
  }
  else{
    datasetName <- paste0(modelName,datasetSize[s_idx]);
  }
  return(datasetName);
}

# Save results to file ----------------------------------------------------
resultsToFile <- function(mcResults,modelName,extension){
  fileName <- paste0("mcResultsf_",modelName,extension);
  write.table(mcResults,fileName,sep=",",col.names = TRUE);
  print(paste0("file written:",fileName));
  mcResults
}

# Prepare features --------------------------------------------------------
prepareFeatures <- function(dataf,selectionType){
  
  #Do feature selection (or not)
  if(selectionType=="ALL")
    featuresdf<- select_ALL(dataf) 
  else
  if(selectionType=="Linear")
    featuresdf<- select_Linear(dataf) 
  else
    if(selectionType=="Discontinuous")
      featuresdf<- select_Discontinuous(dataf) 
    else
      if(selectionType=="Saturating")
        featuresdf<- select_Saturation(dataf) 
  
  #Remove zero utilities
  featuresdf <- featuresdf[featuresdf$UTILITY_INCREASE!=0,];

  # Scramble data 
  featuresdf <- scrambleData(datadf=featuresdf);
  
  return (featuresdf);
}


# Train function  ---------------------------------------------------------
trainModel <- function(featuresdf){
  
  inputFeatures <- dim(featuresdf)[2] - 1; #last column is the target variable
  
  xgb.train.data = xgb.DMatrix(data.matrix(trainingData[,1:inputFeatures]), 
                               label = trainingData[,"UTILITY_INCREASE"],
                               missing = NA)
  
  param <- list(objective = "reg:linear", base_score = 0.5)# booster="gbtree")
  xgboost.cv = xgb.cv(param=param, data = xgb.train.data, nfold = 10, nrounds = 2500, 
                      early_stopping_rounds = 500, metrics='rmse',verbose = FALSE)
  best_iteration <- xgboost.cv$best_iteration;
  xgboost.cv$evaluation_log[best_iteration]
  
  xgb.model <- xgboost(param =param,  data = xgb.train.data, nrounds=best_iteration)
 
  return(list(xgb.model,xgboost.cv));
  
}

# Validation -------------------------------------------------------------
validatePredictions <- function(modelList, mcResultsf,validationData){
  
  xgb.model <- modelList[[1]];
  xgboost.cv <- modelList[[2]];
  
  best_iteration <- xgboost.cv$best_iteration;

  y_pred <- predict(xgb.model, as.matrix(validationData));
  error <- y_pred - validationData$UTILITY_INCREASE;
  
  best_iteration <- xgboost.cv$best_iteration;

  mcResultsf$DataSet[i]<-datasetName[i];
  mcResultsf$Train_RMSE_MEAN[i]<-xgboost.cv$evaluation_log[best_iteration]$train_rmse_mean;
  mcResultsf$Train_RMSE_STD[i]<-xgboost.cv$evaluation_log[best_iteration]$train_rmse_std;
  mcResultsf$Test_RMSE_MEAN[i]<-xgboost.cv$evaluation_log[best_iteration]$test_rmse_mean;
  mcResultsf$Test_RMSE_STD[i]<-xgboost.cv$evaluation_log[best_iteration]$test_rmse_std;
  
  mcResultsf$RMSE[i] <- rmse(error);
  mcResultsf$R_Squared[i] <- r_squared(y_pred,validationData$UTILITY_INCREASE);
  mcResultsf$MAPD[i] <- mapd(y_pred,validationData$UTILITY_INCREASE);
  
  return(mcResultsf);    
}


# Generate PMML file ------------------------------------------------------

generatePMML <- function(xgb.model, featuresdf,modelName){  

  inputFeatures <- dim(featuresdf)[2] - 1; #last column is the target variable
  
    # Generate feature map
  xgboost.fmap = r2pmml::genFMap(featuresdf[1:inputFeatures])
  r2pmml::writeFMap(xgboost.fmap, "xgboost.fmap")
  
  # Save the model in XGBoost proprietary binary format
  xgb.save(xgb.model, "xgboost.model")
  
  # Dump the model in text format
  #  xgb.dump(xgb.model, "xgboost.model.txt", fmap = "xgboost.fmap");
  
  pmmlFileName <- paste0(".//pmml///",modelName,"-xgb.pmml");
  
  r2pmml(xgb.model, pmmlFileName, fmap = xgboost.fmap, response_name = "UTILITY_INCREASE", 
         missing = NULL, ntreelimit = 25, compact = TRUE)
}


# Plot Predicted vs Actual ------------------------------------------------
#https://stackoverflow.com/questions/2564258/plot-two-graphs-in-same-plot-in-r

events = paste("events =",dim(dataf)[1]);
plot(y_pred, type="p",col="red", pch=4, xlab=events, ylab = "Utility Increase") 
points(validationData$UTILITY_INCREASE)
#title = paste("Pred (red cross) x Actual,", affectedComponent[i],", ",name,", MAPD =", mcResultsf$MAPD[i],"%");
title = paste("Pred (red cross) x Actual,", "All Components",", MAPD =", mcResultsf$MAPD[i],"%");

title(title);


# Compute Averages --------------------------------------------------------

resultsf$TRAIN_RMSE_MEAN[index] <-averageRMSE(mcResultsf$Train_RMSE_MEAN,trainingSize);

testingSize = totalData - trainingSize; 
resultsf$TEST_RMSE_MEAN[index] <-averageRMSE(mcResultsf$Test_RMSE_MEAN,testingSize);

resultsf$R_Squared[index] <- r_squared(y_pred,validationData$UTILITY_INCREASE);
resultsf$MAPD[index] <- mapd(y_pred,validationData$UTILITY_INCREASE);




#Plot Train RMSE
proportion <- "70/30"
proportionStr <- toString(proportion);
meanRMSE_Train <- toString(round(averageRMSE(resultsf$Train_RMSE_MEAN,trainingSize),2))
title <- paste("Training RMSE, training proportion", proportionStr,"mean=",meanRMSE_Train)
plot(resultsf$Train_RMSE_MEAN, main=title);

# Plot Validation RMSE
proportionStr <- toString(1-proportion);
validationSize <- length(validationData$Utility_Increase)
meanRMSE_validation <- toString(round(averageRMSE(resultsf$RMSE,validationSize),2))
title <- paste("Validation RMSE, data proportion", proportionStr,"mean=",meanRMSE_validation)
plot(resultsf$RMSE, main=title);

#Plot MAPD
meanMAPD_validation <- round(mean(mcResultsf$MAPD),4);
title <- paste("Validation MAPD, data proportion", proportionStr,"mean=",meanMAPD_validation)
plot(resultsf$MAPD, main=title);


#Plot Validation R_Squared
maxRSquared <- max(resultsf$R_Squared);
minRSquared <- min(resultsf$R_Squared);
title <- paste("Validation R_Squared, data proportion", proportionStr,"max=",maxRSquared,"min=",minRSquared);
plot(resultsf$R_Squared, main=title);
hist(resultsf$R_Squared)

hist(resultsf$RMSE)

hist(trainingData$UTILITY_INCREASE)

meanLinear <- mean(validationData$UTILITY_INCREASE)
rmseLinear <- 5.97283
rmseLinear/meanLinear *100

#Try modeling using linear regression and MARS for the probabilistic and the discontinous