#Train GBM General Boosting Model
#Ten-fold cross-validation
#Trains model with different dataset sizes 1k 3K 9K
#
#Validate model with holdout dataset
#
#Convert model to pmml
##https://github.com/jpmml/r2pmml

# https://www.kaggle.com/nschneider/gbm-vs-xgboost-vs-lightgbm
# https://www.kaggle.com/andrewmvd/lightgbm-in-r
# http://ftp.auckland.ac.nz/software/CRAN/doc/vignettes/gbm/gbm.pdf
# https://stats.stackexchange.com/questions/242105/generating-predictions-on-training-data-in-gbm-regression
# https://stats.stackexchange.com/questions/242105/generating-predictions-on-training-data-in-gbm-regression
# http://allstate-university-hackathons.github.io/PredictionChallenge2016/GBM
# https://stats.stackexchange.com/questions/229356/gbm-package-vs-caret-using-gbm
# https://www.r-bloggers.com/an-introduction-to-xgboost-r-package/


# Train function  ---------------------------------------------------------
trainGBM <- function(training.df,numberOfTrees=2500,kfolds=10){
 
  #lightGBM with caret?
  #https://github.com/bwilbertz/RLightGBM

  #system.time gets the time to train the model
  time <- system.time(
    trained.model <- gbm(UTILITY_INCREASE~.,
                         data = training.df,
                         distribution = "gaussian",
                         n.trees = 2500,
                         n.minobsinnode = 100,
                         shrinkage = 0.01,
                         bag.fraction = 0.5,
                         cv.folds = 10)
  )
  
  best.iteration = gbm.perf(trained.model, method = "cv")
  
  #trained.model$evaluation_log[best_iteration]
  
  # Get feature importance
  gbm.feature.imp = summary(trained.model, n.trees = best.iteration)
  
  return(list(trained.model, convertTimeToDataFrame(time)));
}

# Validation -------------------------------------------------------------
validateGBM <- function(outcome.list,validation.data,i){
  
  results.df <- data.frame(matrix(data=NA,nrow=3,ncol=12));
  colnames(results.df) <- c("Item","Utility_Type","Train_RMSE_MEAN","Train_RMSE_STD","Test_RMSE_MEAN",
                            "Test_RMSE_STD","RMSE","R_Squared", "MAPD","User_Time","Sys_Time","Elapsed_Time");

  trained.model <- outcome.list[[1]];
  time.df <- outcome.list[[2]]
  
  best.iteration <- trained.model$best_iteration;
  
  y_pred <- predict(trained.model, validation.data, n.trees = best.iteration);
  error <- y_pred - validation.data$UTILITY_INCREASE;
  
  results.df$Item[i] <- i;
  results.df$Utility_Type[i]<-gsub(" ","",datasetName[i],fixed = TRUE);
  results.df$Train_RMSE_MEAN[i]<-trained.model$evaluation_log[best.iteration]$train_rmse_mean;
  results.df$Train_RMSE_STD[i]<-trained.model$evaluation_log[best.iteration]$train_rmse_std;
  results.df$Test_RMSE_MEAN[i]<-trained.model$evaluation_log[best.iteration]$test_rmse_mean;
  results.df$Test_RMSE_STD[i]<-trained.model$evaluation_log[best.iteration]$test_rmse_std;
  
  results.df$RMSE[i] <- rmse(error);
  results.df$R_Squared[i] <- r_squared(y_pred,validation.data$UTILITY_INCREASE);
  results.df$MAPD[i] <- mapd(y_pred,validation.data$UTILITY_INCREASE);
  
  results.df$User_Time[i] <- time.df$user.time;
  results.df$Sys_Time[i] <- time.df$sys.time;
  results.df$Elapsed_Time[i] <- time.df$elapsed.time;
  
  return(results.df); 
}

