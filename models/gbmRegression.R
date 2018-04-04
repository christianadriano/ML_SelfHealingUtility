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


#Hyper-paramaters that made a big difference:
#Number of trees - I tested with 5K trees and results were 5 times worse
#interation.depth - consists of the number of feature interactions allowed to be explored. After 15 interactions the MAPD started to saturate
#n.minobsinnode - number of items allowed in the final nodes (leaves). It was initially 100. I reduced to 10, 5, 1. The best result was with 10. 
#shrinkage - the learning step, allows the tree to grow slower, hence be more precise. However, the best rersult came from
#bag.fraction - fraction of the trees that can be used to generate the new trees. Reducing the bagging from 1 to 0.7 cause a slight reduction in validation error, but caused an increase in processing time from 35% to 45%. in error

# Train function  ---------------------------------------------------------
trainGBM <- function(training.df,numberOfTrees,kfolds=10){
 
  #lightGBM with caret?
  #https://github.com/bwilbertz/RLightGBM

  #system.time gets the time to train the model
  time <- system.time(
    trained.model <- gbm(UTILITY_INCREASE~.,
                         data = training.df,
                         distribution = "gaussian",
                         interaction.depth=10,
                         n.trees = numberOfTrees,
                         n.minobsinnode = 10,
                         shrinkage = 0.1,
                         bag.fraction = 1,
                         cv.folds = kfolds)
  )
  
  
  best.iteration = gbm.perf(trained.model, method = "cv")
  
  #trained.model$evaluation_log[best_iteration]
  
  # Get feature importance
 # gbm.feature.imp = summary(trained.model, n.trees = best.iteration)
  
  return(list(trained.model, best.iteration, convertTimeToDataFrame(time)));
}

# Validation -------------------------------------------------------------
validateGBM <- function(outcome.list,validation.df,dataset.name.list,i,results.df){
  
  trained.model <- outcome.list[[1]];
  best.iteration <- outcome.list[[2]];
  time.df <- outcome.list[[3]]
  
  y_pred <- predict(trained.model, validation.df);
  error <- y_pred - validation.df$UTILITY_INCREASE;
  
  results.df$Item[i] <- i;
  results.df$Number_of_Trees[i] <- best.iteration;
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

