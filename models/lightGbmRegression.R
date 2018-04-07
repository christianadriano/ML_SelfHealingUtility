#Train LightGBM Light General Boosting Model
#Ten-fold cross-validation
#Trains model with different dataset sizes 1k 3K 9K
#
#Validate model with holdout dataset
#
#Convert model to pmml
##https://github.com/jpmml/r2pmml

#https://www.kaggle.com/nschneider/gbm-vs-xgboost-vs-lightgbm
#https://www.kaggle.com/andrewmvd/lightgbm-in-r
#https://www.analyticsvidhya.com/blog/2017/06/which-algorithm-takes-the-crown-light-gbm-vs-xgboost/

#Installing LGBM
#https://github.com/Microsoft/LightGBM/issues/912#issuecomment-329496254
#https://cmake.org/download/
#https://www.analyticsvidhya.com/blog/2017/06/which-algorithm-takes-the-crown-light-gbm-vs-xgboost/
#https://raw.githubusercontent.com/Microsoft/LightGBM/master/R-package/README.md


#Hyper-paramaters that made a big difference:
# See documentation here -https://sites.google.com/view/lauraepp/parameters
#max_depth - Typical: 6, usually [3, 12].
#early_stopping_round - typical 50
#num_leaves - On LightGBM, the maximum leaves must be tuned with the maximum depth together. To get xgboost behavior, set the maximum leaves to 2^depth - 1. 
#n.minobsinnode - number of items allowed in the final nodes (leaves). It was initially 100. I reduced to 10, 5, 1. The best result was with 10. 
#shrinkage - the learning step, allows the tree to grow slower, hence be more precise
#bag.fraction - fraction of the trees that can be used to generate the new trees. Reducing the bagging from 1 to 0.7 cause a slight reduction in validation error, but caused an increase in processing time from 35% to 45%. in error

# Train function  ---------------------------------------------------------
train_LightGBM <- function(train_df,test_df,numberOfTrees,kfolds=10){
  #default of cross-validation folds is 10
  browser();
  
  max.depth <- 12;
  num.leaves <- max.depth^2-1;

  params.lgb = list(
    objective = "regression"
    , metric = "l2"
    , min_data_in_leaf = 1
    , min_sum_hessian_in_leaf = 100
    , feature_fraction = 1
    , bagging_fraction = 1
    , bagging_freq = 0
    , early_stopping_rounds=50
    , max_depth=max.depth
    , num_leaves = num.leaves
    , learning_rate=0.1
    , boosting = "dart"
  )
  
  
  matrix.training.df <- matrix(as.numeric(unlist(train_df)),nrow=nrow(train_df));
  matrix.testing.df <- matrix(as.numeric(unlist(test_df)),nrow=nrow(train_df));
  
  
  lgb.train.data <- lgb.Dataset(matrix.training.df, label=train_df$UTILITY_INCREASE);
  lgb.test.data <- lgb.Dataset(matrix.testing.df, label=test_df$UTILITY_INCREASE);
  
  #system.time gets the time to train the model
  time <- system.time(
                    trained.models <- lgb.cv(params.lgb, lgb.train.data, 
                                             nfold = kfolds, nrounds=numberOfTrees)
        );
  
  #training final model
  final.lgb.model = lgb.train(params=params.lgb, data=lgb.train.data, valids=list(lgb.test.data), 
                              nfold = kfolds, nrounds=trained.models$best_iter);
                        
  # Get feature importance
  #lgb.feature.imp = lgb.importance(trained.model, percentage = TRUE);

  return(list(final.lgb.model, trained.models$best_iter, convertTimeToDataFrame(time)));
}

# Validation -------------------------------------------------------------
validate_LightGBM <- function(outcome.list,validation.df,dataset.name.list,i,results.df){
  
  trained.model <- outcome.list[[1]];
  best.iteration <- outcome.list[[2]];
  time.df <- outcome.list[[3]];
  
  y_pred <- predict(trained.model, validation.df);
  error <- y_pred - validation.df$UTILITY_INCREASE;
  
  results.df$Item[i] <- i;
  results.df$Number_of_Trees[i] <- best.iteration;
  results.df$Utility_Type[i]<-gsub(" ","",dataset.name.list[i],fixed = TRUE);

  results.df$RMSE[i] <- rmse(error);
  results.df$R_Squared[i] <- r_squared(y_pred,validation.df$UTILITY_INCREASE);
  results.df$MAPD[i] <- mapd(y_pred,validation.df$UTILITY_INCREASE);
  
  results.df$User_Time[i] <- time.df$user.time;
  results.df$Sys_Time[i] <- time.df$sys.time;
  results.df$Elapsed_Time[i] <- time.df$elapsed.time;

  return(results.df); 
}

# Partition data in training and testing ----------------------------------
extractTrainingTesting <- function(dataset.df){
  browser();
  totalData.size <- dim(dataset.df)[1];
  train.size <- trunc(totalData.size * 0.9);

  train.df <- as.data.frame(dataset.df[1:train.size-1,]);
  test.df <- as.data.frame(dataset.df[train.size:totalData.size,]);
 
  train_test.list <- list(train.df,test.df);
  return (train_test.list);
}
