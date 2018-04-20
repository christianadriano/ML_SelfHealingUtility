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

#Interesting articles about XGBoost
#https://medium.com/@Synced/tree-boosting-with-xgboost-why-does-xgboost-win-every-machine-learning-competition-ca8034c0b283
#


#-------------------------------------------------------------------------------------------------


# Train function  ---------------------------------------------------------
train_XGBoost <- function(training.df,numberOfTrees,kfolds=10){
  
  last.column.explanatory <- dim(training.df)[2] - 1; #last column is the target variable
  
  xgb.train.data = xgb.DMatrix(data.matrix(training.df[,1:last.column.explanatory]), 
                               label = training.df[,"UTILITY_INCREASE"],
                               missing = NA);
  
  param <- list(objective = "reg:linear", base_score = 0.5);# booster="gbtree")
  
  #Discovers the best model
  time <- system.time(trained.model <-  xgb.cv(
                                      param=param, 
                                      data = xgb.train.data, 
                                      nfold = kfolds, 
                                      nrounds = numberOfTrees, 
                                      early_stopping_rounds = 500, 
                                      metrics='rmse',
                                      verbose = TRUE)
                      );
  
  best.iteration <- trained.model$best_iteration;
  trained.model$evaluation_log[best.iteration];
  #browser();
  # Get feature importance
  summary(trained.model, n.trees = best.iteration);
  
  #Get the bes model
  best.model <- xgboost(param =param,  data = xgb.train.data, nrounds=best.iteration);

  return(list(best.model, convertTimeToDataFrame(time),best.iteration));
}

# Validation -------------------------------------------------------------
validate_XGBoost <- function(outcome.list,validation.df,dataset.name,i,results.df){
  
  trained.model <- outcome.list[[1]];
  time.df <- outcome.list[[2]];
  
  best.iteration <- outcome.list[[3]];;

  y_pred <- predict(trained.model, as.matrix(validation.df));
  error <- y_pred - validation.df$UTILITY_INCREASE;
  
  results.df$Item[i] <- i;
  results.df$Utility_Type[i]<-gsub(" ","",dataset.name,fixed = TRUE);
  # results.df$Train_RMSE_MEAN[i]<-trained.model$evaluation_log[best.iteration]$train_rmse_mean;
  # results.df$Train_RMSE_STD[i]<-trained.model$evaluation_log[best.iteration]$train_rmse_std;
  # results.df$Test_RMSE_MEAN[i]<-trained.model$evaluation_log[best.iteration]$test_rmse_mean;
  # results.df$Test_RMSE_STD[i]<-trained.model$evaluation_log[best.iteration]$test_rmse_std;
  
  results.df$RMSE[i] <- rmse(error);
  results.df$R_Squared[i] <- r_squared(y_pred,validation.df$UTILITY_INCREASE);
  results.df$MADP[i] <- madp(y_pred,validation.df$UTILITY_INCREASE);
  
  #results.df$User_Time[i] <- time.df$user.time;
  #results.df$Sys_Time[i] <- time.df$sys.time;
  results.df$Elapsed_Time[i] <- time.df$elapsed.time;
  results.df$Number_of_Trees[i] <- best.iteration;
  
  return(results.df);    
}


# Plot trees --------------------------------------------------------------

plot_trees <- function(feature.names,model){
  xgb.plot.tree(feature.names,model); 
}





