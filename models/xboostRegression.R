#Train xBoostTree model to predict Utility Function
#Ten-fold cross-validation
#Trains model with different dataset sizes 1k 3K 9K
#
#Validate model with holdout dataset
#
#Convert xgboost to jpmml
#https://github.com/jpmml/jpmml-xgboost

#Automatic model selection
#Models are select by defining a metric, which in our case we choose RMSE - Root Mean Square Error)

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
for(i in c(1:length(datasetName))){
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
}

#print(results.df); #show on the console

message <- resultsToFile(results.df,modelName,"_70-30_FeatureSelection.csv"); #save to a .csv file
print(message);

generatePMML(outcomeList[[1]],featuresdf,datasetName[i]);#datasetName[length(datasetName)]);

#-------------------------------------------------------------------------------------------------


# Train function  ---------------------------------------------------------
trainModel <- function(featuresdf){
  
  inputFeatures <- dim(featuresdf)[2] - 1; #last column is the target variable
  
  xgb.train.data = xgb.DMatrix(data.matrix(trainingData[,1:inputFeatures]), 
                               label = trainingData[,"UTILITY_INCREASE"],
                               missing = NA)
  
  param <- list(objective = "reg:linear", base_score = 0.5)# booster="gbtree")
  
  #Discovers the best model
  time <- system.time(trained.model <-  xgb.cv(
                                      param=param, 
                                      data = xgb.train.data, 
                                      nfold = 10, 
                                      nrounds = 2500, 
                                      early_stopping_rounds = 500, 
                                      metrics='rmse',
                                      verbose = FALSE)
                      )
  
  best_iteration <- trained.model$best_iteration;
  #trained.model$evaluation_log[best_iteration]
  
  #Get the bes model
  best.model <- xgboost(param =param,  data = xgb.train.data, nrounds=best_iteration)
  #convertTimeToDataFrame(time)
  return(list(best.model,trained.model, convertTimeToDataFrame(time)));
  
}

# Validation -------------------------------------------------------------
validatePredictions <- function(modelList, results.df,validationData){
  
  best.model <- modelList[[1]];
  trained.model <- modelList[[2]];
  time.df <- modelList[3]
  
  best_iteration <- trained.model$best_iteration;

  y_pred <- predict(best.model, as.matrix(validationData));
  error <- y_pred - validationData$UTILITY_INCREASE;
  
  best_iteration <- trained.model$best_iteration;
  results.df$Item[i] = "";
  results.df$Utility_Type[i]<-datasetName[i];
  results.df$Train_RMSE_MEAN[i]<-trained.model$evaluation_log[best_iteration]$train_rmse_mean;
  results.df$Train_RMSE_STD[i]<-trained.model$evaluation_log[best_iteration]$train_rmse_std;
  results.df$Test_RMSE_MEAN[i]<-trained.model$evaluation_log[best_iteration]$test_rmse_mean;
  results.df$Test_RMSE_STD[i]<-trained.model$evaluation_log[best_iteration]$test_rmse_std;
  
  results.df$RMSE[i] <- rmse(error);
  results.df$R_Squared[i] <- r_squared(y_pred,validationData$UTILITY_INCREASE);
  results.df$MAPD[i] <- mapd(y_pred,validationData$UTILITY_INCREASE);
  
  results.df$User_Time[i] <- time.df$user.time;
  results.df$Sys_Time[i] <- time.df$sys.time;
  results.df$Elapsed_Time[i] <- time.df$elapsed.time;
  
  return(results.df);    
}



# Plot Predicted vs Actual ------------------------------------------------
#https://stackoverflow.com/questions/2564258/plot-two-graphs-in-same-plot-in-r

events = paste("events =",dim(dataf)[1]);
plot(y_pred, type="p",col="red", pch=4, xlab=events, ylab = "Utility Increase") 
points(validationData$UTILITY_INCREASE)
#title = paste("Pred (red cross) x Actual,", affectedComponent[i],", ",name,", MAPD =", results.df$MAPD[i],"%");
title = paste("Pred (red cross) x Actual,", "All Components",", MAPD =", results.df$MAPD[i],"%");

title(title);


# Compute Averages --------------------------------------------------------

resultsf$TRAIN_RMSE_MEAN[index] <-averageRMSE(results.df$Train_RMSE_MEAN,trainingSize);

testingSize = totalData - trainingSize; 
resultsf$TEST_RMSE_MEAN[index] <-averageRMSE(results.df$Test_RMSE_MEAN,testingSize);

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
meanMAPD_validation <- round(mean(results.df$MAPD),4);
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