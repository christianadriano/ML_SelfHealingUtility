#Montecarlo simulation over xBoostTrees
#average the outcome of R2 and RMSE for different samplings
#of the same training/testing/validation split
#Create a loop to compute the outcomes for differnt splits (measure the time of execution)
#Run the simulation for different file sizes 100, 1000, 10000

#Imports
#Older implementation with Rserver. Not using it anymore.
#https://stackoverflow.com/questions/10216014/simple-program-to-call-r-from-java-using-eclipse-and-rserve
# install.packages("Rserve")
# library(Rserve)
# Rserve()

library(xgboost)
# load data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");

index=0;


datasetName <- c("linear10K","Probabilistic10K","Discontinous10K","Saturation10K","ALL10K","ALL_but_Random_10K");



dataf <-loadData(fileName="data//10000//Linear10K.csv"); 
dataf <-loadData(fileName="data//10000//Probabilistic10K.csv"); 
dataf <- loadData(fileName="data//10000//Discontinous10K.csv");
dataf <- loadData(fileName="data//10000//Saturating10K.csv");
dataf <- loadData(fileName="data//10000//ALL10K.csv");
dataf <- loadData(fileName="data//10000//ALL_but_Random10K.csv");
summary(dataf)

index=index+1;



# Select feature columns --------------------------------------------------
featuresdf<- select_Linear(dataf) #
featuresdf<- select_Probabilistic(dataf) #
featuresdf<- select_Discontinous(dataf) #
featuresdf<- select_Saturation(dataf) #
featuresdf<- select_ALL(dataf) #

inputFeatures <- dim(featuresdf)[2] - 1;

#RUN ALL DATA SETS WITH ALL FEATURES TO CHECK IF THE MODEL IS ABLE TO GET RID OF
#USELESS FEATURES.
averageResultsf <- data.frame(matrix(data=NA,nrow=6,ncol=8));
colnames(resultsf) <- c("DataSet","Train_RMSE_MEAN","Train_RMSE_STD","Test_RMSE_MEAN",
                        "Test_RMSE_STD","RMSE","R_Squared", "MAPD");



proportion <- 0.9
featuresdf <- featuresdf[featuresdf$UTILITY_INCREASE!=0,];

#DATA STRUCTURE TO KEEP THE INTERMEDIATE MODELS 
mcResultsf <- data.frame(matrix(data=NA,nrow=100,ncol=8));
colnames(resultsf) <- c("DataSet","Train_RMSE_MEAN","Train_RMSE_STD","Test_RMSE_MEAN",
                        "Test_RMSE_STD","RMSE","R_Squared", "MAPD");

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
  
  xgb.train.data = xgb.DMatrix(data.matrix(trainingData[,1:inputFeatures]), 
                               label = trainingData[,"UTILITY_INCREASE"],
                               missing = NA)
  
  param <- list(objective = "reg:linear", base_score = 0.5)
  xgboost.cv = xgb.cv(param=param, data = xgb.train.data, nfold = 10, nrounds = 1500, 
                      early_stopping_rounds = 100, metrics='rmse')
  best_iteration <- xgboost.cv$best_iteration;
  xgboost.cv$evaluation_log[best_iteration]
  
  xgb.model <- xgboost(param =param,  data = xgb.train.data, nrounds=best_iteration)
  
  # Validation -------------------------------------------------------------
  
  y_pred <- predict(xgb.model, as.matrix(validationData));
  error <- y_pred - validationData$UTILITY_INCREASE;
  
  mcResultsf$DataSet[i]<-datasetName[i];
  mcResultsf$Train_RMSE_MEAN[i]<-xgboost.cv$evaluation_log[best_iteration]$train_rmse_mean;
  mcResultsf$Train_RMSE_STD[i]<-xgboost.cv$evaluation_log[best_iteration]$train_rmse_std;
  mcResultsf$Test_RMSE_MEAN[i]<-xgboost.cv$evaluation_log[best_iteration]$test_rmse_mean;
  mcResultsf$Test_RMSE_STD[i]<-xgboost.cv$evaluation_log[best_iteration]$test_rmse_std;
  
  mcResultsf$RMSE[i] <- rmse(error);
  mcResultsf$R_Squared[i] <- r_squared(y_pred,validationData$UTILITY_INCREASE);
  mcResultsf$MAPD[i] <- mapd(y_pred,validationData$UTILITY_INCREASE);

}

fileName <- paste0("mcResultsf_",datasetName[index],".csv");
write.table(mcresultsf,fileName,sep=",",col.names = TRUE);


# Compute Averages --------------------------------------------------------

 


resultsf$TRAIN_RMSE_MEAN[index] <-averageRMSE(mcResultsf$Train_RMSE_MEAN,trainingSize);

testingSize = totalData - trainingSize; 
resultsf$TEST_RMSE_MEAN[index] <-averageRMSE(mcResultsf$Test_RMSE_MEAN,testingSize);

resultsf$R_Squared[index] <- r_squared(y_pred,validationData$UTILITY_INCREASE);
resultsf$MAPD[index] <- mapd(y_pred,validationData$UTILITY_INCREASE);




#Plot Train RMSE
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

hist(trainingData$UTILITY_INCREASE)

meanLinear <- mean(validationData$UTILITY_INCREASE)
rmseLinear <- 5.97283
rmseLinear/meanLinear *100

#Try modeling using linear regression and MARS for the probabilistic and the discontinous