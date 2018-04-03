
#Several funtions to plot the results from training a prediction model
#I used some techniques as such: https://stackoverflow.com/questions/2564258/plot-two-graphs-in-same-plot-in-r
#My goal is to improve this file, so I can reuse it across projects.


# Plot Predicted vs Actual ------------------------------------------------
plotData <- function(dataf){
  events = paste("events =",dim(dataf)[1]);
  plot(y_pred, type="p",col="red", pch=4, xlab=events, ylab = "Utility Increase") 
  points(validationData$UTILITY_INCREASE)
  #title = paste("Pred (red cross) x Actual,", affectedComponent[i],", ",name,", MAPD =", results.df$MAPD[i],"%");
  title = paste("Pred (red cross) x Actual,", "All Components",", MAPD =", results.df$MAPD[i],"%");
  title(title);
}


# Plot AUC ----------------------------------------------------------------
plotAUC <- function(model, tes.data, best.iter){
  test = predict(model, newdata = test.data, n.trees = best.iter);
  auc.results = roc(test.data$UTILTY_CHANGE, gbm.test, plot = TRUE, col = "red");
  plot(auc.results)
  return(auc.results);
}

# Compute Averages --------------------------------------------------------
computeAverages <- function(results.df, index){
  results.df$TRAIN_RMSE_MEAN[index] <-averageRMSE(results.df$Train_RMSE_MEAN,trainingSize);
  
  testingSize = totalData - trainingSize; 
  results.df$TEST_RMSE_MEAN[index] <-averageRMSE(results.df$Test_RMSE_MEAN,testingSize);
  
  results.df$R_Squared[index] <- r_squared(y_pred,validationData$UTILITY_INCREASE);
  results.df$MAPD[index] <- mapd(y_pred,validationData$UTILITY_INCREASE);
  return(results.df)
}


# Plot Train RMSE ---------------------------------------------------------
plot_TrainRMSE <- function(resultsf, trainingSize){
  proportion <- "70/30"
  proportionStr <- toString(proportion);
  meanRMSE_Train <- toString(round(averageRMSE(resultsf$Train_RMSE_MEAN,trainingSize),2))
  title <- paste("Training RMSE, training proportion", proportionStr,"mean=",meanRMSE_Train)
  plot(resultsf$Train_RMSE_MEAN, main=title);
}


# Plot Validation RMSE ----------------------------------------------------
plot_ValidationRMSE(resultsf, validationData){
  proportionStr <- toString(1-proportion);
  validationSize <- length(validationData$Utility_Increase)
  meanRMSE_validation <- toString(round(averageRMSE(resultsf$RMSE,validationSize),2))
  title <- paste("Validation RMSE, data proportion", proportionStr,"mean=",meanRMSE_validation)
  plot(resultsf$RMSE, main=title);
}


# Plot MAPD ---------------------------------------------------------------
plot_MAPD <- function(results.df){
  meanMAPD_validation <- round(mean(results.df$MAPD),4);
  title <- paste("Validation MAPD, data proportion", proportionStr,"mean=",meanMAPD_validation)
  plot(resultsf$MAPD, main=title);
}


# Plot Validation R_Squared -----------------------------------------------
plot_Validation_R_Squared <- function(resultsf){
  maxRSquared <- max(resultsf$R_Squared);
  minRSquared <- min(resultsf$R_Squared);
  title <- paste("Validation R_Squared, data proportion", proportionStr,"max=",maxRSquared,"min=",minRSquared);
  plot(resultsf$R_Squared, main=title);
  hist(resultsf$R_Squared)
  
  hist(resultsf$RMSE)
  
  hist(trainingData$UTILITY_INCREASE)
  
  meanLinear <- mean(validationData$UTILITY_INCREASE)
  rmseLinear <- 5.97283
  rmseLinear/meanLinear *100
}