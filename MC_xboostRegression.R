#Montecarlo simulation over xBoostTrees
#average the outcome of R2 and RMSE for different samplings
#of the same training/testing/validation split
#Create a loop to compute the outcomes for differnt splits (measure the time of execution)
#Run the simulation for different file sizes 100, 1000, 10000

#Imports
#https://stackoverflow.com/questions/10216014/simple-program-to-call-r-from-java-using-eclipse-and-rserve
library(Rserve)
Rserve()

library(xgboost)
library(pROC)

# load data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");
# dataf_l<-loadData(fileName="data//Linear.csv");
# dataf_p<-loadData(fileName="data//Probabilistic.csv");
# dataf_d <- loadData(fileName="data//discontinous.csv");
dataf_s <- loadData(fileName="data//Saturating.csv");
dataf <- dataf_s;
#summary(dataf_s)

resultsf <- data.frame(matrix(data=NA,nrow=100,ncol=7));
colnames(resultsf) <- c("Train_RMSE_MEAN","Train_RMSE_STD","Test_RMSE_MEAN",
                        "Test_RMSE_STD","RMSE","R_Squared", "MAPD");

# Select feature columns --------------------------------------------------
featuresdf<- data.frame(dataf$CRITICALITY,dataf$CONNECTIVITY,dataf$RELIABILITY, dataf$IMPORTANCE, 
                        dataf$PROVIDED_INTERFACE, dataf$REQUIRED_INTERFACE, dataf$ADT,
                        dataf$PMax,dataf$alpha,dataf$REQUEST,
                        dataf$UTILITY.INCREASE); 

colnames(featuresdf) <- c("Criticality","Connectivity","Reliability","Importance",
                          "Provided.Interface", 
                          "Required.Interface","ADT",
                          "PMax","alpha","REQUEST",
                          "Utility.Increase");
proportion <- 0.7
featuresdf <- featuresdf[featuresdf$Utility.Increase!=0,];

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
  
  xgb.train.data = xgb.DMatrix(data.matrix(trainingData[,1:10]), 
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
  resultsf$MAPD[i] <- mapd(y_pred,validationData$Utility.Increase);
}

#Plot Train RMSE
proportionStr <- toString(proportion);
meanRMSE_Train <- toString(round(averageRMSE(resultsf$Train_RMSE_MEAN,trainingSize),2))
title <- paste("Training RMSE, training proportion", proportionStr,"mean=",meanRMSE_Train)
plot(resultsf$Train_RMSE_MEAN, main=title);

# Plot Validation RMSE
proportionStr <- toString(1-proportion);
validationSize <- length(validationData$Utility.Increase)
meanRMSE_validation <- toString(round(averageRMSE(resultsf$RMSE,validationSize),2))
title <- paste("Validation RMSE, data proportion", proportionStr,"mean=",meanRMSE_validation)
plot(resultsf$RMSE, main=title);

#Plot MAPD
meanMAPD_validation <- round(mean(resultsf$MAPD),4);
title <- paste("Validation MAPD, data proportion", proportionStr,"mean=",meanMAPD_validation)
plot(resultsf$MAPD, main=title);


#Plot Validation R_Squared
maxRSquared <- max(resultsf$R_Squared);
minRSquared <- min(resultsf$R_Squared);
title <- paste("Validation R_Squared, data proportion", proportionStr,"max=",maxRSquared,"min=",minRSquared);
plot(resultsf$R_Squared, main=title);
hist(resultsf$R_Squared)

hist(resultsf$RMSE)

meanLinear <- mean(validationData$Utility.Increase)
rmseLinear <- 5.97283
rmseLinear/meanLinear *100

#Try modeling using linear regression and MARS for the probabilistic and the discontinous