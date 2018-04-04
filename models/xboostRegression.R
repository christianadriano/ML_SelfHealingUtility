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
trainXGBoost <- function(training.df,numberOfTrees,kfolds=10){
  
  last.column.explanatory <- dim(training.df)[2] - 1; #last column is the target variable
  
  xgb.train.data = xgb.DMatrix(data.matrix(training.df[,1:last.column.explanatory]), 
                               label = training.df[,"UTILITY_INCREASE"],
                               missing = NA);
  
  param <- list(objective = "reg:linear", base_score = 0.5);# booster="gbtree")
  
  #Discovers the best model
  time <- system.time(trained.model <-  xgb.cv(
                                      param=param, 
                                      data = xgb.train.data, 
                                      nfold = kfold, 
                                      nrounds = numberOfTrees, 
                                      early_stopping_rounds = 500, 
                                      metrics='rmse',
                                      verbose = FALSE)
                      )
  
  best.iteration <- trained.model$best_iteration;
  #trained.model$evaluation_log[best_iteration]
  
  # Get feature importance
  summary(trained.model, n.trees = best.iteration)
  
  #Get the bes model
  #best.model <- xgboost(param =param,  data = xgb.train.data, nrounds=best.iteration);

  return(list(trained.model, convertTimeToDataFrame(time)));
  
}

# Validation -------------------------------------------------------------
validateXGBoost <- function(outcome.list,validation.df,dataset.name.list,i,results.df){
  
  trained.model <- outcome.list[[1]];
  time.df <- outcome.list[[2]];
  
  best.iteration <- trained.model$best_iteration;

  y_pred <- predict(trained.model, as.matrix(validation.df));
  error <- y_pred - validation.df$UTILITY_INCREASE;
  
  results.df$Item[i] <- i;
  results.df$Utility_Type[i]<-gsub(" ","",dataset.name.list[i],fixed = TRUE);
  # results.df$Train_RMSE_MEAN[i]<-trained.model$evaluation_log[best.iteration]$train_rmse_mean;
  # results.df$Train_RMSE_STD[i]<-trained.model$evaluation_log[best.iteration]$train_rmse_std;
  # results.df$Test_RMSE_MEAN[i]<-trained.model$evaluation_log[best.iteration]$test_rmse_mean;
  # results.df$Test_RMSE_STD[i]<-trained.model$evaluation_log[best.iteration]$test_rmse_std;
  
  results.df$RMSE[i] <- rmse(error);
  results.df$R_Squared[i] <- r_squared(y_pred,validation.df$UTILITY_INCREASE);
  results.df$MAPD[i] <- mapd(y_pred,validation.df$UTILITY_INCREASE);
  
  results.df$User_Time[i] <- time.df$user.time;
  results.df$Sys_Time[i] <- time.df$sys.time;
  results.df$Elapsed_Time[i] <- time.df$elapsed.time;
  
  return(results.df);    
}


# Plot trees --------------------------------------------------------------

plot_trees <- function(feature.names,model){
  xgb.plot.tree(feature.names,model); 
}





