#Predict Utility of Components

library(caret);
library(car);

# load data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");
dataf<-loadData(fileName="Random_proper_comp_names.csv");
staticf<- loadData(fileName = "MLDATA2_STATIC.csv");
#summary(dataf);

#Remove all Failures that do not cause utility increase
#dataf<- dataf[dataf$FAILURE.NAME=="CF3",];
#validationf<- validationf[dataf$FAILURE.NAME=="CF3",];

#Select only the ws that have the Authentication component
#dataf<-dataf[grep("Auth", dataf$AFFECTED.COMPONENT), ];
#validationf<-validationf[grep("Auth", dataf$AFFECTED.COMPONENT), ];

#Remove all reliability values equal to zero
dataf<- dataf[dataf$RELIABILITY!=0,];
dataf <- dataf[dataf$UTILITY.INCREASE!=0,];
staticf<- staticf[staticf$RELIABILITY!=0,];
staticf<- staticf[staticf$UTILITY.INCREASE!=0,];

# consider only the feature columns
featuresf<-data.frame(dataf$CRITICALITY,
                      dataf$CONNECTIVITY,
                      dataf$RELIABILITY,
                      dataf$UTILITY.INCREASE);

validationf<-data.frame(staticf$CRITICALITY,
                        staticf$CONNECTIVITY,
                        staticf$RELIABILITY,
                        staticf$UTILITY.INCREASE);

colnames(featuresf) <- c("Criticality","Connectivity","Reliability","Utility");
colnames(validationf) <- c("Criticality","Connectivity","Reliability","Utility");

plot(featuresf);
title("Training");

plot(validationf);
title("Validation");


##Converted the non-linear into a linear
modelFit<- lm(log(Utility) ~ log(Criticality) + log(Connectivity)  ,data=featuresf);

modelFit<- lm(Utility ~ Connectivity*Connectivity + Reliability ,data=featuresf);

modelFit<- lm(Utility ~ Criticality ,data=featuresf);

modelFit<- lm(Utility ~ Connectivity ,data=featuresf);

modelFit<- lm(Utility ~ Reliability ,data=featuresf);


modelFit <- lm(log(Utility) ~ log(Criticality) + log(Connectivity) +log(Reliability),data=featuresf);

modelFit<- lm(Utility ~ Criticality*Connectivity ,data=featuresf);
summary(modelFit)
avPlots(modelFit)

#validate models
lmPredicted <- predict(modelFit, featuresf)

summary(modelFit)
lmPredicted

residual.modelFit<- resid(modelFit);
plot(x=featuresf$Criticality,y=residual.modelFit,
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
lmPredictedValidation <- predict(modelFit, validationf);

plot(x=validationf$Criticality,y=validationf$Utility);
points(validationf$Criticality, lmPredictedValidation, col = "red", 
       pch=4,abline(modelFit));
title("Linear Regression - actual (circles) vs predicted (crosses)");

#Compute Root Mean Square Error
rmse <- function(error)
{
  return (sqrt(mean(error^2)));
  
}

#error <- modelFit$residuals (residuals of the training, not interesting)
error<- validationf$Utility - lmPredictedValidation # same as data$Y - predictedY
predictionRMSE <- rmse(error)  
predictionRMSE
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
