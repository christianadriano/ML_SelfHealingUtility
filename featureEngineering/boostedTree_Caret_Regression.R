#Looking for pptimal hyperparameters for boosted trees
#Using CARET package

library(caret) # for model-building

# load data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");
dataf_l<-loadData(fileName="data//Linear.csv");
dataf_p<-loadData(fileName="data//Probabilistic.csv");
dataf_d <- loadData(fileName="data//discontinous.csv");
dataf <- dataf_l;

resultsf <- data.frame(matrix(data=NA,nrow=6,ncol=3));
colnames(resultsf) <- c("ProportionTraining","RMSE","R_Squared");

# Select feature columns --------------------------------------------------
featuresdf<- data.frame(dataf$CRITICALITY,dataf$CONNECTIVITY,dataf$RELIABILITY, dataf$IMPORTANCE, 
                        dataf$PROVIDED_INTERFACE, dataf$REQUIRED_INTERFACE, dataf$ADT, dataf$UTILITY.INCREASE); 

colnames(featuresdf) <- c("Criticality","Connectivity","Reliability","Importance","Provided.Interface", 
                          "Required.Interface","ADT","Utility.Increase");

proportionList <- c(0.7,0.75,0.8,0.85,0.9,0.95);

for(i in c(1:6)){

  proportion <- proportionList[i];
  
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
  
  folds <- 10;
  # Create custom indices: myFolds
  #Guarantees that we are going to use the exact same datasets for all models
  myFolds <- createFolds(trainingData, k = folds); 
  
  #larger K implies less bias (but more overfitting). However, larger K implies larger variance (overfitting), 
  #i.e., modeling noise, which makes the prediction to presentlarge variation. 
  #The reason for this is that larger K makes each training data large and very similar, while at the same
  #time makes the testing data very dissimilar.
  #nice explanation here: https://stats.stackexchange.com/questions/27730/choice-of-k-in-k-fold-cross-validation
  
  # Create reusable trainControl object: myControl
  kFoldControl <- trainControl(
    index = myFolds, #Train with k folds and validate with one
    classProbs = FALSE, # IMPORTANT!
    verboseIter = TRUE, #
    savePredictions = TRUE
  );
  
  xgbTree.model <- train(Utility.Increase ~ .,
                         trainingData, 
                         method="xgbTree", 
                         verbose= TRUE,
                         trControl=kFoldControl, 
                         metric="RMSE");
  
  predicted <- predict(xgbTree.model,newdata =validationData );
  
  error <- predicted - validationData$Utility.Increase;
  
  # resultsf$Train_RMSE_MEAN[i]<-xgboost.cv$evaluation_log[best_iteration]$train_rmse_mean;
  # resultsf$Train_RMSE_STD[i]<-xgboost.cv$evaluation_log[best_iteration]$train_rmse_std;
  # resultsf$Test_RMSE_MEAN[i]<-xgboost.cv$evaluation_log[best_iteration]$test_rmse_mean;
  # resultsf$Test_RMSE_STD[i]<-xgboost.cv$evaluation_log[best_iteration]$test_rmse_std;
  
  resultsf$ProportionTraining[i] <- proportion;
  resultsf$RMSE[i] <- rmse(error);
  resultsf$R_Squared[i] <- r_squared(predicted,validationData$Utility.Increase);
}

#----------------------------------------------------------------------------------------------

# registerDoMC(cores = 20)

# # Max shrinkage for gbm
# nl = nrow(training)
# max(0.01, 0.1*min(1, nl/10000))
# # Max Value for interaction.depth
# floor(sqrt(NCOL(training)))

# gbmGrid <-  expand.grid(interaction.depth = c(1, 3, 6, 9, 10),
#                         n.trees = (0:50)*50, 
#                         shrinkage = seq(.0005, .05,.0005),
#                         n.minobsinnode = 10) # you can also put something        like c(5, 10, 15, 20)

# fitControl <- trainControl(method = "repeatedcv",
#                            repeats = 5,
#                            preProcOptions = list(thresh = 0.95),
#                            ## Estimate class probabilities
#                            classProbs = TRUE,
#                            ## Evaluate performance using
#                            ## the following function
#                            summaryFunction = twoClassSummary)

# Method + Date + distribution

# set.seed(1)
# system.time(GBM0604ada <- train(Outcome ~ ., data = training,
#                                 distribution = "adaboost",
#                                 method = "gbm", bag.fraction = 0.5,
#                                 nTrain = round(nrow(training) *.75),
#                                 trControl = kFoldControl,
#                                 verbose = TRUE,
#                                 tuneGrid = gbmGrid,
#                                 ## Specify which metric to optimize
#                                 metric = "RMSE"))