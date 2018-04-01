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



#-------------------------------------------------------------------------------------------------


# Train function  ---------------------------------------------------------
trainModel <- function(featuresdf){
  
  inputFeatures <- dim(featuresdf)[2] - 1; #last column is the target variable
  
  xgb.train.data = xgb.DMatrix(data.matrix(trainingData[,1:inputFeatures]), 
                               label = trainingData[,"UTILITY_INCREASE"],
                               missing = NA);
  
  param <- list(objective = "reg:linear", base_score = 0.5);# booster="gbtree")
  
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
  best.model <- xgboost(param =param,  data = xgb.train.data, nrounds=best_iteration);
  #convertTimeToDataFrame(time)
  return(list(best.model,trained.model, convertTimeToDataFrame(time)));
  
}

# Validation -------------------------------------------------------------
validatePredictions <- function(modelList,validationData,i){
  
  results.df <- data.frame(matrix(data=NA,nrow=3,ncol=12));
  colnames(results.df) <- c("Item","Utility_Type","Train_RMSE_MEAN","Train_RMSE_STD","Test_RMSE_MEAN",
                            "Test_RMSE_STD","RMSE","R_Squared", "MAPD","User_Time","Sys_Time","Elapsed_Time");
  
  best.model <- modelList[[1]];
  trained.model <- modelList[[2]];
  time.df <- modelList[[3]]
  
  best_iteration <- trained.model$best_iteration;

  y_pred <- predict(best.model, as.matrix(validationData));
  error <- y_pred - validationData$UTILITY_INCREASE;
  
  best_iteration <- trained.model$best_iteration;
  results.df$Item[i] <- i;
  results.df$Utility_Type[i]<-gsub(" ","",datasetName[i],fixed = TRUE);
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




