#Predict Utility of Components

library(caret);
library(car);

# load xml and pmml library
library("devtools")
install_git("git://github.com/jpmml/r2pmml.git")
library(r2pmml)
library(XML)

# load data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");

datasetSize="10K";

linear = paste0("Linear_",datasetSize,".csv");
discontinous = paste0("Discontinous_",datasetSize,".csv");
saturating = paste0("Saturating_",datasetSize,".csv");
all = paste0("ALL_but_Random_",datasetSize,".csv");

datasetName <- c(linear,discontinous,saturating,all);

folder <- "data//New4Cases//";

# Load data section -------------------------------------------------------

dataf <-loadData(fileName=paste0(folder,linear)); 
dataf <- loadData(fileName=paste0(folder,discontinous));
dataf <- loadData(fileName=paste0(folder,saturating));
dataf <- loadData(fileName=paste0(folder,all));
#summary(dataf)

#Remove all reliability values equal to zero
dataf <- dataf[dataf$UTILITY.INCREASE!=0,];

# consider only the feature columns
featuresdf<-data.frame(dataf$CRITICALITY,
                      dataf$CONNECTIVITY,
                      dataf$RELIABILITY,
                      dataf$UTILITY_INCREASE);

colnames(featuresdf) <- c("CRITICALITY","CONNECTIVITY","RELIABILITY","UTILITY_INCREASE");

plot(featuresdf);
title("Training");

proportion <- 0.8;

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

resultsf <- data.frame(matrix(data=NA,nrow=8,ncol=4));
colnames(resultsf) <- c("Train_RMSE","Train_R_Squared","Validation_RMSE", "Validation_MAPD");

computeErrors <- function(training_error,predicted_values,actual_values){
  training_rmse <- rmse(training_error);
  training_rsquared <- r_squared(predicted_values,actual_values);
  validation_rmse <- rmse(predicted_values-actual_values);
  validation_mapd <- mapd(predicted_values,actual_values);
  rowResults <- c(training_rmse,training_rsquared,validation_rmse,validation_mapd);
}


# Build Models ------------------------------------------------------------

modelFit <- lm(log(UTILITY_INCREASE) ~ log(CRITICALITY) + log(CONNECTIVITY)  ,data=trainingData);
#summary(modelFit_1);
predicted_values <- predict(modelFit,log(validationData));
resultsf[1,]<- computeErrors(modelFit$residuals,predicted_values,log(validationData$UTILITY_INCREASE));

modelFit <- lm(UTILITY_INCREASE ~ CONNECTIVITY*CRITICALITY + RELIABILITY ,data=trainingData);
predicted_values <- predict(modelFit,validationData);
resultsf[2,]<- computeErrors(modelFit$residuals,predicted_values,validationData$UTILITY_INCREASE);
modelFit_CCplusR.lm <- modelFit; #Save it to export

modelFit <- lm(UTILITY_INCREASE ~ CRITICALITY ,data=trainingData);
predicted_values <- predict(modelFit,validationData);
resultsf[3,]<- computeErrors(modelFit$residuals,predicted_values,validationData$UTILITY_INCREASE);

modelFit <- lm(UTILITY_INCREASE ~ CONNECTIVITY ,data=trainingData);
predicted_values <- predict(modelFit,validationData);
resultsf[4,]<- computeErrors(modelFit$residuals,predicted_values,validationData$UTILITY_INCREASE);

modelFit<- lm(UTILITY_INCREASE ~ RELIABILITY ,data=trainingData);
predicted_values <- predict(modelFit,validationData);
resultsf[5,]<- computeErrors(modelFit$residuals,predicted_values,validationData$UTILITY_INCREASE);

modelFit <- lm(log(UTILITY_INCREASE) ~ log(CRITICALITY) + log(CONNECTIVITY) +log(RELIABILITY),data=trainingData);
predicted_values <- predict(modelFit,log(validationData));
resultsf[6,]<- computeErrors(modelFit$residuals, predicted_values,log(validationData$UTILITY_INCREASE));

modelFit <- lm(UTILITY_INCREASE ~ CRITICALITY*CONNECTIVITY*RELIABILITY ,data=trainingData);
predicted_values <- predict(modelFit,validationData);
resultsf[7,]<- computeErrors(modelFit$residuals,predicted_values,validationData$UTILITY_INCREASE);
modelFit_CCR.lm <- modelFit; #Save it to export

modelFit <- lm(UTILITY_INCREASE ~ . ,data=trainingData);
predicted_values <- predict(modelFit,validationData);
resultsf[8,]<- computeErrors(modelFit$residuals,predicted_values,validationData$UTILITY_INCREASE);

avPlots(modelFit)

# Export model to PMML format ---------------------------------------------

r2pmml(modelFit_CCplusR.lm, "models//CriticalityConnectivity_plus_Reliability_LM.pmml")
r2pmml(modelFit_CCR.lm, "models//CriticalityConnectivityReliability_LM.pmml")

# Validate Models ---------------------------------------------------------

lmPredicted <- predict(modelFit_1, validationData)

residual.modelFit<- resid(modelFit_1);
plot(x=validationData$CRITICALITY,y=residual.modelFit,
     ylab="Residuals", xlab="Criticality",
     main="Residual Plot Actual Utility minus Predicted Utility");
abline(0,0);

#There is a non-random pattern in the residuals, which suggest a non-linear relationship.

plot(x=featuresf$Criticality,y=featuresf$Utility);
points(featuresf$Criticality, lmPredicted, col = "red", pch=4, 
       abline(modelFit));
title("Linear Regression - actual (circles) vs predicted (crosses)");

plot(x=featuresf$Criticality,y=featuresf$Utility, 
     abline(modelFit),cex = 1.3, pch = 16);
title("Linear Regression - actual (circles) vs predicted (line)");

plot(x=featuresf$Connectivity,y=featuresf$Utility);
points(featuresf$Connectivity, lmPredicted, col = "red", pch=4);
title("Linear Regression - actual (circles) vs predicted (crosses)");

#validate models
lmPredictedValidation <- predict(modelFit_2, validationData);

plot(x=validationData$CONNECTIVITY,y=validationData$UTILITY_INCREASE);
points(validationData$CRITICALITY, lmPredictedValidation, col = "red", 
       pch=4,abline(modelFit));
title("Linear Regression - actual (circles) vs predicted (crosses)");

#error <- modelFit$residuals (residuals of the training, not interesting)
error<- validationData$UTILITY_INCREASE - lmPredictedValidation # same as data$Y - predictedY
predictionRMSE <- rmse(error)  
predictionRMSE

MADP <- mapd(lmPredictedValidation, validationData$UTILITY_INCREASE)
MADP

# 28.07377 (validation error)

# Linear Regression ALL FAILURES
# 
# 1098 samples
# 3 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 988, 988, 988, 988, 987, 990, ... 
# Resampling results:
#   
#   RMSE     Rsquared   MAE     
# 6.42462  0.9079988  2.478855

#---------------------------------------------------------
# 
# Linear Regression 
# 
# 54 samples
# 3 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 48, 48, 48, 50, 48, 48, ... 
# Resampling results:
#   
#   RMSE      Rsquared  MAE      
# 1.150587  0.995727  0.9715667


#Evaluate non-linearity
crPlots(modelFit)

install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(modelFit) 
summary(gvmodel)

prediction<- predict(modelFit, validationf);
plot(modelFit)

# ASSESSMENT OF THE LINEAR MODEL ASSUMPTIONS
# USING THE GLOBAL TEST ON 4 DEGREES-OF-FREEDOM:
#   Level of Significance =  0.05 
# 
# Call:
#   gvlma(x = modelFit) 
# 
#                     Value  p-value  Decision
# Global Stat        4.9304  0.2945 Assumptions acceptable.
# Skewness           0.6027  0.4375 Assumptions acceptable.
# Kurtosis           2.3508  0.1252 Assumptions acceptable.
# Link Function      1.1567  0.2821 Assumptions acceptable.
# Heteroscedasticity 0.8202  0.3651 Assumptions acceptable.

compareTable <- data.frame(validationf$Criticality,validationf$Connectivity,
                           log(validationf$Utility),
                           prediction);
colnames(compareTable) <- c("criticality","connectivity","actual_utility","predicted_utility");

compareTable
plot(prediction)
                     


#Extract the unique items from a column and return them sorted
# listUniqueItems<- function(column,columnName){
#   uniqueItems <- data.frame(unique(column));
#   colnames(uniqueItems) <- c(columnName);
#   uniqueItems <- uniqueItems[with(uniqueItems,order(columnName)),];
#   return(uniqueItems);
# }

# listUniqueItems(dataf$FAILURE.NAME,"FAILURE.NAME");

# Create custom indices: myFolds
#Guarantees that we are going to use the exact same datasets for all models
# myFolds <- createFolds(featuresf , k = 10); 

#larger K implies less bias (overfitting). However, larger K implies larger variance, i.e., 
#the prediction has large variation. The reason is that larger K makes each training data large and
#very similar.
#nice explanation here: https://stats.stackexchange.com/questions/27730/choice-of-k-in-k-fold-cross-validation

# Create reusable trainControl object: myControl
# kFoldControl <- trainControl(
#   index = myFolds, 
#   verboseIter = TRUE,
#   savePredictions = TRUE 
# );

# trControl <- trainControl(method="cv", number=10);
# 
# modelFit<- train(Utility ~ Criticality*Connectivity,featuresf, method="lm", 
#                  trControl=trControl);
