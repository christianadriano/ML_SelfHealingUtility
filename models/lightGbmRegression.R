#Train LightGBM Light General Boosting Model
#Ten-fold cross-validation
#Trains model with different dataset sizes 1k 3K 9K
#
#Validate model with holdout dataset
#
#Convert model to pmml
##https://github.com/jpmml/r2pmml

# https://www.kaggle.com/nschneider/gbm-vs-xgboost-vs-lightgbm
# https://www.kaggle.com/andrewmvd/lightgbm-in-r


#Hyper-paramaters that made a big difference:
# See documentation here -https://sites.google.com/view/lauraepp/parameters
#max_depth - Typical: 6, usually [3, 12].
#early_stopping_round - typical 50
#num_leaves - On LightGBM, the maximum leaves must be tuned with the maximum depth together. To get xgboost behavior, set the maximum leaves to 2^depth - 1. 
#n.minobsinnode - number of items allowed in the final nodes (leaves). It was initially 100. I reduced to 10, 5, 1. The best result was with 10. 
#shrinkage - the learning step, allows the tree to grow slower, hence be more precise
#bag.fraction - fraction of the trees that can be used to generate the new trees. Reducing the bagging from 1 to 0.7 cause a slight reduction in validation error, but caused an increase in processing time from 35% to 45%. in error

# Train function  ---------------------------------------------------------
trainLightGBM <- function(training.df,interaction,numberOfTrees=2500,kfolds=10){
  
  max.depth <- 12ç
  num.leaves <- max.detph^2-1ç
  
  params.lgb = list(
    objective = "regression"
    , metric = "l2"
    , min_data_in_leaf = 1
    , min_sum_hessian_in_leaf = 100
    , feature_fraction = 1
    , bagging_fraction = 1
    , bagging_freq = 0
    , early_stopping_round=50
    , max_depth=max.depth
    , num_leaves = num.leaves
    , learning_rate=0.1
    , nfold = kfolds
  )
  
  lgb.train.data <- lgb.Dataset(training.df$data, label=training.df$UTILITY_INCREASE);
  
  #system.time gets the time to train the model
  time <- system.time(
        trained.model <- lgb.cv(params, lgb.train.data))
  
  best.iteration = gbm.perf(trained.model, method = "cv")
  
  #trained.model$evaluation_log[best_iteration]
  
  # Get feature importance
  # gbm.feature.imp = summary(trained.model, n.trees = best.iteration)
  
  return(list(trained.model, best.iteration, convertTimeToDataFrame(time)));
}

# Validation -------------------------------------------------------------
validateLightGBM <- function(outcome.list,validation.df,dataset.name.list,i,results.df,nodes){
  
  trained.model <- outcome.list[[1]];
  best.iteration <- outcome.list[[2]];
  time.df <- outcome.list[[3]]
  
  y_pred <- predict(trained.model, validation.df);
  error <- y_pred - validation.df$UTILITY_INCREASE;
  
  results.df$Item[i] <- i;
  results.df$Number_of_Trees[i] <- best.iteration;
  results.df$Utility_Type[i]<-gsub(" ","",dataset.name.list[3],fixed = TRUE);
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
  results.df$Number_of_Trees[i] <- nodes;
  
  return(results.df); 
}

