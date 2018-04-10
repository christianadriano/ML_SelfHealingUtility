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

#max.detph - there were no differences across 6, 12, and 24. Kept 6. Typical is from 3 to 12.
#learning rate - evaluated from 0.1 to 0.001. Best results was 0.1.
#split training/testing - evaluated from 70/30 to 90/10. Best was 90/10.
#number of trees - there is no effect on MAPD (varied from 1000 to 15000)

#Ideas for tunning
#https://medium.com/@pushkarmandot/https-medium-com-pushkarmandot-what-is-lightgbm-how-to-implement-it-how-to-fine-tune-the-parameters-60347819b7fc

# Train function  ---------------------------------------------------------
train_LightGBM <- function(train_df,test_df,numberOfTrees, kfolds, max.detph,
                           learning.rate,min.data.in.leaf,bagging.fraction){
  #browser();
  num.leaves <- max.depth^2-1;

  
  
  params.lgb = list(
    objective = "regression"
    , metric = "mape"
    , min_data_in_leaf = min.data.in.leaf
    , min_sum_hessian_in_leaf = min.data.in.leaf
    , feature_fraction = bagging.fraction
    , bagging_fraction = bagging.fraction
    , bagging_freq = 0
    , early_stopping_rounds=50
    , max_depth=max.depth
    , num_leaves = num.leaves
    , learning_rate=learning.rate
    , boosting = "dart"
  );
  
  

  lgb.train.dataset = lgb.Dataset(as.matrix(train_df[, colnames(train_df) != "UTILITY_INCREASE"]), 
                          label=train_df$UTILITY_INCREASE);
  
  lgb.test.dataset = lgb.Dataset(as.matrix(test_df[, colnames(test_df) != "UTILITY_INCREASE"]), 
                                 label=test_df$UTILITY_INCREASE);
   
  ##Discovers the best model
  time <- system.time(
    trained.models <- lgb.cv(params.lgb
                             ,lgb.train.dataset
                             ,nfold = kfolds
                             ,nrounds=numberOfTrees
                             ,verbose = 1
    )
  );
  
  
  #Train final model
  final.lgb.model = lgb.train(params=params.lgb 
                              ,data=lgb.train.dataset
                              ,valids=list(test=lgb.test.dataset));
  #                             ,nrounds=trained.models$best_iter
  # );
                        
  # Get feature importance
  #lgb.feature.imp = lgb.importance(trained.model, percentage = TRUE);

  return(list(final.lgb.model, trained.models$best_iter, convertTimeToDataFrame(time)));
}

# Validation -------------------------------------------------------------
validate_LightGBM <- function(outcome.list,validation.df,dataset.name,index,results.df,
                              numberOfTrees,learning.rate,max.depth,train.split,
                              min.data.in_leaf,bagging.fraction){
  #browser();
  trained.model <- outcome.list[[1]];
  best.iteration <- outcome.list[[2]];
  time.df <- outcome.list[[3]];
  
  #convert dataframe to lightGBM format
  lgb.validation.matrix =  as.matrix(validation.df[, colnames(validation.df) != "UTILITY_INCREASE"]);
  
  y_pred <- predict(trained.model, lgb.validation.matrix, n = trained.model$best_iter);
  error <- y_pred - validation.df$UTILITY_INCREASE;
  
  results.df$Item[index] <- index;
  results.df$Number_of_Trees[index] <- trained.model$best_iter;
  results.df$Utility_Type[index]<-gsub(" ","", dataset.name, fixed = TRUE);

  results.df$RMSE[index] <- rmse(error);
  results.df$R_Squared[index] <- r_squared(y_pred,validation.df$UTILITY_INCREASE);
  results.df$MAPD[index] <- mapd(y_pred,validation.df$UTILITY_INCREASE);
  
  results.df$User_Time[index] <- time.df$user.time;
  results.df$Sys_Time[index] <- time.df$sys.time;
  results.df$Elapsed_Time[index] <- time.df$elapsed.time;
  
  results.df$Number_of_Trees[index] <- numberOfTrees;
  results.df$Learning_Rate[index] <- learning.rate;
  results.df$Max_Depth[index] <- max.depth;
  results.df$Train_Split[index] <- train.split;
  results.df$Min_Data_In_Leaf[index] <- min.data.in_leaf;
  results.df$Bagging_Fraction[index] <- bagging.fraction;
  
  
  return(results.df); 
}

# Partition data in training and testing ----------------------------------
extractTrainingTesting <- function(dataset.df,train_split){
  #browser();
  totalData.size <- dim(dataset.df)[1];
  train.size <- trunc(totalData.size * train_split);

  train.df <- as.data.frame(dataset.df[1:train.size-1,]);
  test.df <- as.data.frame(dataset.df[train.size:totalData.size,]);
 
  train_test.list <- list(train.df,test.df);
  return (train_test.list);
}
