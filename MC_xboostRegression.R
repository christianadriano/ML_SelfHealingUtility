#Montecarlo simulation over xBoostTrees
#average the outcome of R2 and RMSE for different samplings
#of the same training/testing/validation split
#Create a loop to compute the outcomes for differnt splits (measure the time of execution)
#Run the simulation for different file sizes 100, 1000, 10000

#Imports
library(xgboost)
library(pROC)

# load data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");
dataf_l<-loadData(fileName="data//Linear.csv");
dataf_p<-loadData(fileName="data//Probabilistic.csv");

resultsf <- data.frame(matrix(data=NA,nrow=100,ncol=6));
colnames(resultsf) <- c("Train_RMSE_MEAN","Train_RMSE_STD","Test_RMSE_MEAN", "Test_RMSE_STD","RMSE","R_Squared");

# Select feature columns --------------------------------------------------
featuresdf<- data.frame(dataf$CRITICALITY,dataf$CONNECTIVITY,dataf$RELIABILITY, dataf$IMPORTANCE, 
                        dataf$PROVIDED_INTERFACE, dataf$REQUIRED_INTERFACE, dataf$ADT, dataf$UTILITY.INCREASE); 

colnames(featuresdf) <- c("Criticality","Connectivity","Reliability","Importance","Provided.Interface", 
                          "Required.Interface","ADT","Utility.Increase");

proportion <- 0.7

for(i in c(1:100)){
  
  
  # Scramble data -----------------------------------------------------------
  featuresdf <- scrambleData(dataf=featuresdf);
  
  # Extract training ad validation sets -------------------------------------
  #Training = used to create a model
  #Validation = used to compute prediction error (Bias)
  totalData = dim(featuresdf)[1];
  trainingSize = trunc(totalData * proportion);
  startTestIndex = totalData - trainingSize;
  
  trainingData<- as.data.frame(featuresdf[1:trainingSize,]);
  validationData<-as.data.frame(featuresdf[startTestIndex:totalData,]);
  
  
  # Build model -------------------------------------------------------------
  
  xgb.train.data = xgb.DMatrix(data.matrix(trainingData[,1:7]), 
                               label = trainingData[,"Utility.Increase"],
                               missing = NA)
  
  param <- list(objective = "reg:linear", base_score = 0.5)
  xgboost.cv = xgb.cv(param=param, data = xgb.train.data, nfold = 10, nrounds = 1500, 
                      early_stopping_rounds = 100, metrics='rmse')
  best_iteration <- xgboost.cv$best_iteration;
  xgboost.cv$evaluation_log[best_iteration]
  
  xgb.model <- xgboost(param =param,  data = xgb.train.data, nrounds=best_iteration)
  
  # Validation -------------------------------------------------------------
  
  y_pred <- predict(xgb.model, as.matrix(validationData));
  error <- y_pred - validationData$Utility.Increase;
  
  resultsf$Train_RMSE_MEAN[i]<-xgboost.cv$evaluation_log[best_iteration]$train_rmse_mean;
  resultsf$Train_RMSE_STD[i]<-xgboost.cv$evaluation_log[best_iteration]$train_rmse_std;
  resultsf$Test_RMSE_MEAN[i]<-xgboost.cv$evaluation_log[best_iteration]$test_rmse_mean;
  resultsf$Test_RMSE_STD[i]<-xgboost.cv$evaluation_log[best_iteration]$test_rmse_std;
  
  resultsf$RMSE[i] <- rmse(error);
  resultsf$R_Squared[i] <- r_squared(y_pred,validationData$Utility.Increase);
  
}

plot(resultsf$Train_RMSE_MEAN, main="Training RMSE, 70/30, mean= 16.42466");
mean(resultsf$Train_RMSE_MEAN)
plot(resultsf$Test_RMSE_MEAN, main="Testing RMSE, 70/30, mean= 120.5871");
mean(resultsf$Test_RMSE_MEAN)
plot(resultsf$RMSE, main="Validation RMSE, 70/30, mean=80.52817");
mean(resultsf$RMSE)
plot(resultsf$R_Squared, main="Validation R_Squared, 70/30, mean=0.9905962");
mean(resultsf$R_Squared)

hist(resultsf$RMSE)

meanLinear <- mean(validationData$Utility.Increase)
rmseLinear <- 5.97283
rmseLinear/meanLinear *100

#Try modeling using linear regression and MARS for the probabilistic and the discontinous