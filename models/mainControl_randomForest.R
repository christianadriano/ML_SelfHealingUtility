## Control code to run Random Fores

#https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

library(randomForest)
library(caret)

library(devtools)
library(r2pmml) #https://github.com/jpmml/r2pmml

# Initialization section ------------------------------------------------------

#Load utility functions
source("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//models//loadData.R");

#Data structure to keep results

#Folder with training data
folder <- "C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//data//DataPoints_1K-3K-9K//";
#folder <- "//DataPoints_1K-3K-9K//";

# CONTROL CODE   ------------------------------------------------------------

model.name.list <- c("Linear","Discontinuous","Saturating","Combined");
model.name <- model.name.list[2];

method.name <- "RF";

dataset.name.list <- generateDataSetNames(model.name, c("1K","2K","9K"),0);

results.df <- data.frame(matrix(data=NA,nrow=1000,ncol=14));
colnames(results.df) <- c("Item","Utility_Type","RMSE","R_Squared", "MADP","User_Time","Sys_Time","Elapsed_Time",
                          "Number_of_Trees","Learning_Rate","Max_Depth","Train_Split","Min_Data_In_Leaf","Bagging_Fraction");

results_line <- 0;

for(model.name in model.name.list){
  dataset.name.list <- generateDataSetNames(model.name, c("1K","3K","9K"),0);
  
  for(i in c(1:length(dataset.name.list))){
    #i <- 1;
    results_line <- results_line+1;
    
    fileName <- paste0(folder,dataset.name.list[i],".csv");
    data.df <- loadData(fileName);
    
    features.df <- prepareFeatures(data.df,"Combined");
    
    #Extract training and validation sets 
    totalData.size <- dim(features.df)[1];
    training.size <- trunc(totalData.size * 0.7);
    #endValidationIndex <- totalData.size - training.size;
    
    training.df <- as.data.frame(features.df[1:training.size-1,]);
    validation.df <- as.data.frame(features.df[training.size:totalData.size,]);
    
    #Train model
    numberOfTrees <- 100
    control <- trainControl(method="repeatedcv", number=10, repeats=1, search="random")
    set.seed(7)
    bestmtry <- tuneRF(training.df, training.df$UTILITY_INCREASE, stepFactor=1.5, 
                       improve=1e-2)
    tune.grid <- expand.grid(.mtry=bestmtry)
     time <- system.time(
       trained.model <- train(UTILITY_INCREASE~., data=training.df, method="rf", metric="RMSE", 
                          tuneGrid=tune.grid, trControl=control,nodesize=5, ntree=numberOfTrees)
     );
     
    #Validate model
    results.df <- validate_RF(trained.model,convertTimeToDataFrame(time),validation.df,
                               dataset.name.list[i],results_line,results.df,
                               numberOfTrees);

    #pmmlFileName <- paste0(".//pmml///",dataset.name.list[i],"_RF_70-30_test",method.name,".pmml");
    #generatePMML(trained.model,training.df,pmmlFileName,numberOfTrees);
  }
}

message <- resultsToFile(results.df,model.name,method.name,"_RF_70_30_test.csv"); #save to a .csv file
print(message);

#-----------------------------------------------------------------------------

# Validation -------------------------------------------------------------
validate_RF <- function(trained.model,time.df,validation.df,dataset.name,i,results.df,numberOfTrees){
  
  y_pred <- predict(trained.model, validation.df);
  error <- y_pred - validation.df$UTILITY_INCREASE;
  
  results.df$Item[i] <- i;
  results.df$Number_of_Trees[i] <- numberOfTrees;
  results.df$Utility_Type[i]<-gsub(" ","",dataset.name,fixed = TRUE);
  # results.df$Train_RMSE_MEAN[i]<-trained.model$evaluation_log[best.iteration]$train_rmse_mean;
  # results.df$Train_RMSE_STD[i]<-trained.model$evaluation_log[best.iteration]$train_rmse_std;
  # results.df$Test_RMSE_MEAN[i]<-trained.model$evaluation_log[best.iteration]$test_rmse_mean;
  # results.df$Test_RMSE_STD[i]<-trained.model$evaluation_log[best.iteration]$test_rmse_std;
  
  results.df$RMSE[i] <- rmse(error);
  results.df$R_Squared[i] <- r_squared(y_pred,validation.df$UTILITY_INCREASE);
  results.df$MADP[i] <- madp(y_pred,validation.df$UTILITY_INCREASE);
  
  results.df$User_Time[i] <- time.df$user.time;
  results.df$Sys_Time[i] <- time.df$sys.time;
  results.df$Elapsed_Time[i] <- time.df$elapsed.time;

  return(results.df); 
}
