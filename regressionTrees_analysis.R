#Regression Trees to predict the utility drop based on the
#following features: criticality, connectivity, reliability, 
#fan-in, fan-out, average time to deploy (ATD)

#Imports
library(xgboost)
library(pROC)

# load data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");
dataf<-loadData(fileName="data//Random_10000Failures_without_inapplicable_rules.csv");
#dataf<-dataf[dataf$UTILITY.INCREASE!=0,] #Not removing anymore. We need some negative instances

#summary(dataf);

# Select feature columns --------------------------------------------------
featuresdf<- data.frame(dataf$CRITICALITY,dataf$CONNECTIVITY,dataf$RELIABILITY, dataf$IMPORTANCE, 
                        dataf$PROVIDED_INTERFACE, dataf$REQUIRED_INTERFACE, dataf$ADT, dataf$UTILITY.INCREASE); 
                          
colnames(featuresdf) <- c("Criticality","Connectivity","Reliability","Importance","Provided.Interface", 
                          "Required.Interface","ADT","Utility.Increase");

##TODO

#Create a Montecarlo Simulation loop to average the outcome of R2 and RMSE for different samplings
#of the same training/testing/validation split
#Create a loop to compute the outcomes for differnt splits (measure the time of execution)
#Run the simulation for different file sizes 100, 1000, 10000

##Nice to do - see if you can use vectorization to implment these loops (so you can even a blog post about it)

#PLOT function for each feature. Do that using a scatter plot with bloxplot

#Compute the RMSE and R2 for the case of probabilistic output (ask a question on Reddit or StackExchange?)
#Why R2 is not good to select models? How to use AUC, PROC, AIC, BIC instead

proportion <- 0.8

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


# Build model -------------------------------------------------------------

xgb.train.data = xgb.DMatrix(data.matrix(trainingData[,1:7]), 
                             label = trainingData[,"Utility.Increase"],
                             missing = NA)

param <- list(objective = "reg:linear", base_score = 0.5)
xgboost.cv = xgb.cv(param=param, data = xgb.train.data, nfold = 10, nrounds = 1500, 
                    early_stopping_rounds = 100, metrics='rmse')
best_iteration <- xgboost.cv$best_iteration;
xgboost.cv$evaluation_log[best_iteration]


# Compute AUC -------------------------------------------------------------

xgb.model <- xgboost(param =param,  data = xgb.train.data, nrounds=best_iteration)
xgb.model


xgb.test.data = xgb.DMatrix(data.matrix(validationData[,1:7]), missing = NA)
xgb.preds = predict(xgb.model, xgb.test.data)
xgb.roc_obj <- roc(validationData[,"Utility.Increase"], xgb.preds)
xgb.roc_obj
# Call:
#   roc.default(response = validationData[, "Utility.Increase"],     predictor = xgb.preds)
# 
# Data: xgb.preds in 399 controls (validationData[, "Utility.Increase"] 0) > 1 cases 
#(validationData[, "Utility.Increase"] 13.3018695).
# Area under the curve: 0.8571





# Metric functions --------------------------------------------------------

# Root mean square error
# https://en.wikipedia.org/wiki/Root-mean-square_deviation
rmse <- function(error){
  sqrt(mean(error^2))
}

# Coefficient of determination
# https://en.wikipedia.org/wiki/Coefficient_of_determination
r_squared <- function(prediction, actual){
  
  SS_ExplainedVariance <- sum((prediction - actual)^2);
  SS_TotalVariance <- sum((actual-mean(actual))^2);
  R2<- 1- SS_ExplainedVariance / SS_TotalVariance;
  return (R2);  
}

#-------------------------------------------------------------------------

# Validation -------------------------------------------------------------

y_pred <- predict(xgb.model, as.matrix(validationData));
error <- y_pred - validationData$Utility.Increase;



# Residual analysis -------------------------------------------------------

#Are the residuals (errors) ok? So we can accept the regression model as valid?
#Note however that regression trees usually do not perform well in terms of heteroscedacity [1]
errorFrame <- data.frame(validationData[,1:7]);
errorFrame$residuals<- error;
for(i in 1:7){
  plot(errorFrame[,i],error) #shows that the errors are randomly distributed for range of the predictions
}
qqnorm(error,main="Normal Q-Q Plot - error ") #shows that the residuals are fairly normally distributed
qqline(error)
hist(error)

#[1] Gelfand, S. J. (2015). Understanding the impact of heteroscedasticity on the predictive ability of modern regression methods.

# Percent error, RMSE, R2 -------------------------------------------------

meanError <- mean(y_pred - validationData$Utility.Increase)
percentMeanError <- meanError / mean(validationData$Utility.Increase)*100;
percentMeanError

rmse_value <- rmse(error);
rmse_value

prediction <- data.frame(y_pred);
actual <- data.frame(validationData$Utility.Increase);
R2 <- r_squared(prediction,as.numeric(validationData$Utility.Increase));
R2

# Plot prediction ---------------------------------------------------------

library(lattice)
xyplot(y_pred ~ Provided.Interface,validationData, grid=TRUE,
       type = c("p", "smooth"), col.line = "darkorange", lwd = 1, main="Predicted versus Actual");


xyplot(y_pred ~ ADT,validationData, grid=TRUE,
       type = c("p", "smooth"), col.line = "darkorange", lwd = 1, main="Predicted versus Actual");


xyplot(y_pred ~ Connectivity,validationData, grid=TRUE,
       type = c("p", "smooth"), col.line = "darkorange", lwd = 1, main="Predicted versus Actual");


#https://www.stat.ubc.ca/~jenny/STAT545A/block09_xyplotLattice.html
 
cor(y_pred,validationData$Provided.Interface)

xyplot(Provided.Interface ~ Criticality,validationData, grid=TRUE,
       type = c("p", "smooth"), col.line = "darkorange", lwd = 1, main="Predicted versus Actual");

cor(validationData$Provided.Interface,validationData$Connectivity)
cor(trainingData$Provided.Interface,trainingData$Connectivity)


plot(validationData$Provided.Interface,validationData$Criticality)

plot(trainingData$Provided.Interface,trainingData$Connectivity)



validationData$predicted<-y_pred

install.packages("lmtest")
library(lmtest)
library(gvlma)
model <- lm(y_pred ~ Provided.Interface, validationData)
bptest(model)
# studentized Breusch-Pagan test
# data:  model
# BP = 1194.9, df = 1, p-value < 2.2e-16
# p-value<0.05 means that we can reject the null-hypothesis
# Null-hypothesis is that the data is homoscedacity,which is not.
#There are many reasons to reject the null-hypothesis of homoscedacity, one is the omittion of predictors,
#which we really omitted.

gvlma(model, )
summary(model)

#90/10 percentMeanError -0.1447302
#80/20 percentMeanError  0.01095572
#70/30 percentMeanError -0.02022227


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
idx_to_get = as.integer(300)
validationData[idx_to_get,1:7]
showWaterfall(xgb.model, explainer, xgb.test.data, data.matrix(validationData[,1:7]),
              idx_to_get, type = "regression")


# Visualizing non-linearities ---------------------------------------------

plot(validationData[,"Connectivity"], t(pred.breakdown[,"Connectivity"]), cex=0.4, pch=16, 
     xlab = "Connectivity", ylab = "Connectivity impact on log-odds")

plot(validationData[,"Criticality"], t(pred.breakdown[,"Criticality"]), cex=0.4, pch=16, 
     xlab = "Criticality", ylab = "Criticality impact on log-odds")

plot(validationData[,"Reliability"], t(pred.breakdown[,"Reliability"]), cex=0.4, pch=16, 
     xlab = "Reliability", ylab = "Reliability impact on log-odds")

plot(validationData[,"ADT"], t(pred.breakdown[,"ADT"]), cex=0.4, pch=16, 
     xlab = "ADT", ylab = "ADT impact on log-odds")

plot(validationData[,"Importance"], t(pred.breakdown[,"Importance"]), cex=0.4, pch=16, 
     xlab = "Importance", ylab = "Importance impact on log-odds")

plot(validationData[,"Provided.Interface"], t(pred.breakdown[,"Provided.Interface"]), cex=0.4, pch=16, 
     xlab = "Provided.Interface", ylab = "Provided.Interface impact on log-odds")

plot(validationData[,"Required.Interface"], t(pred.breakdown[,"Required.Interface"]), cex=0.4, pch=16, 
     xlab = "Required.Interface", ylab = "Required.Interface impact on log-odds")


#Not working below
# cr <- colorRamp(c("blue", "red"))
# 
# plot(validationData[,"Criticality"],  t(pred.breakdown[,"Criticality"]), 
#      col = rgb(cr(round(validationData[,"Criticality"])), max=255), cex=0.4, pch=16, 
#      xlab = "Criticality", ylab = "Criticality impact on log-odds")
