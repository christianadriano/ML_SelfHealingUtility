#Regression Trees to predict the utility drop based on the
#following features: criticality, connectivity, reliability, 
#fan-in, fan-out, average time to deploy (ATD)

#Imports
library(xgboost)
library(pROC)

# load data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");
dataf<-loadData(fileName="data//Random_with_different_Increase_for_each_rule.csv");

summary(dataf);



# Select feature columns --------------------------------------------------
featuresdf<- data.frame(dataf$CRITICALITY,dataf$CONNECTIVITY,dataf$RELIABILITY, dataf$IMPORTANCE, 
                        dataf$PROVIDED_INTERFACE, dataf$REQUIRED_INTERFACE, dataf$ADT, dataf$UTILITY.INCREASE); 
                          
colnames(featuresdf) <- c("Connectivity", "Criticality","Reliability","Importance","Provided.Interface", 
                          "Required.Interface","ADT","Utility.Increase");



#Centralize features (divide them by their mean)
centralize<- function(featureData){
  featureData <- featureData/mean(featureData);
  return(featureData);
}


# Centralize variables ----------------------------------------------------
featuresdf$Utility.Increase <- centralize(featuresdf$Utility.Increase);
featuresdf$Criticality <- centralize(featuresdf$Criticality);
featuresdf$Connectivity <- centralize(featuresdf$Connectivity);
featuresdf$Reliability <- centralize(featuresdf$Reliability);
featuresdf$Importance <- centralize(featuresdf$Importance);
featuresdf$Provided.Interface <- centralize(featuresdf$Provided.Interface);
featuresdf$Required.Interface <- centralize(featuresdf$Required.Interface);
featuresdf$ADT <- centralize(featuresdf$ADT);

# Scramble data -----------------------------------------------------------
#Scramble the dataset before extracting the training set.
set.seed(8850);
g<- runif((nrow(featuresdf))); #generates a random distribution
featuresdf <- featuresdf[order(g),];


# Extract training ad validation sets -------------------------------------
#Training = used to create a model
#Validation = used to compute prediction error (Bias)
totalData = dim(featuresdf)[1];
trainingSize = trunc(totalData * 0.9);
startTestIndex = totalData - trainingSize;
endTestIndex = totalData;

trainingData<- as.data.frame(featuresdf[1:trainingSize,]);
validationData<-as.data.frame(featuresdf[startTestIndex:endTestIndex,]);


# Build model -------------------------------------------------------------

xgb.train.data = xgb.DMatrix(data.matrix(trainingData[,1:7]), 
                             label = trainingData[,"Utility.Increase"],
                             missing = NA)

param <- list(objective = "reg:linear", base_score = 0.5)
xgboost.cv = xgb.cv(param=param, data = xgb.train.data, nfold = 10, nrounds = 1500, 
                    early_stopping_rounds = 100, metrics='auc')
best_iteration = xgboost.cv$best_iteration

xgb.model <- xgboost(param =param,  data = xgb.train.data, nrounds=best_iteration)

#Best training iteration = 17
# iter train_rmse_mean train_rmse_std test_rmse_mean test_rmse_std
# 17        110.4483        8.86739       225.6595      77.05662

xgb.test.data = xgb.DMatrix(data.matrix(validationData[,1:7]), missing = NA)
xgb.preds = predict(xgb.model, xgb.test.data)
xgb.roc_obj <- roc(validationData[,"Utility.Increase"], xgb.preds)

# Call:
#   roc.default(response = validationData[, "Utility.Increase"],     predictor = xgb.preds)
# 
# Data: xgb.preds in 399 controls (validationData[, "Utility.Increase"] 0) > 1 cases (validationData[, "Utility.Increase"] 13.3018695).
# Area under the curve: 0.8571



# Plot prediction ---------------------------------------------------------

y_pred <- predict(xgb.model, as.matrix(validationData));
mean(y_pred - validationData$Utility.Increase)
plot(y_pred)
plot(validationData$Utility.Increase)


# Feature Importance ------------------------------------------------------

col_names = attr(xgb.train.data, ".Dimnames")[[2]]
importance_matrix = xgb.importance(col_names, xgb.model)

xgb.plot.importance(importance_matrix, rel_to_first = TRUE, n_clusters=2, xlab = "Relative importance")

(gg <- xgb.ggplot.importance(importance_matrix, measure = "Gain", rel_to_first = TRUE))
gg + ggplot2::ylab("Information Gain (relative to top feature)")


# Explaining the model ----------------------------------------------------

library(xgboostExplainer)
explainer = buildExplainer(xgb.model,xgb.train.data, type="regression", 
                           base_score = 0.5, 
                           n_first_tree = xgb.model$best_ntreelimit - 1)

pred.breakdown = explainPredictions(xgb.model, explainer, xgb.test.data)
cat('Breakdown Complete','\n')
weights = rowSums(pred.breakdown)
pred.xgb = 1/(1+exp(-weights))
cat(max(xgb.preds-pred.xgb),'\n')
idx_to_get = as.integer(802)
validationData[idx_to_get,1:7]
showWaterfall(xgb.model, explainer, xgb.test.data, data.matrix(validationData[,1:7]) ,idx_to_get, type = "regression")


# Visualizing non-linearities ---------------------------------------------

plot(validationData[,"Connectivity"], t(pred.breakdown[,"Connectivity"]), cex=0.4, pch=16, 
     xlab = "Connectivity", ylab = "Connectivity impact on log-odds")

plot(validationData[,"Criticality"], t(pred.breakdown[,"Criticality"]), cex=0.4, pch=16, 
     xlab = "Criticality", ylab = "Criticality impact on log-odds")

#Not working below
# cr <- colorRamp(c("blue", "red"))
# 
# plot(validationData[,"Criticality"],  t(pred.breakdown[,"Criticality"]), 
#      col = rgb(cr(round(validationData[,"Criticality"])), max=255), cex=0.4, pch=16, 
#      xlab = "Criticality", ylab = "Criticality impact on log-odds")
