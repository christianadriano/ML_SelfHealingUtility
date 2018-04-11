#Non-linear regression with SVM

#Predict Utility of Components

library(car);

# load data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");
dataf<-loadData(fileName="MLDATA2_data.csv");
staticf<- loadData(fileName = "MLDATA2_STATIC.csv");
#summary(dataf);

#Remove all Failures that do not cause utility increase
dataf<- dataf[dataf$FAILURE.NAME=="CF3",];
#validationf<- validationf[dataf$FAILURE.NAME=="CF3",];

#Select only the rows that have the Authentication component
#dataf<-dataf[grep("Auth", dataf$AFFECTED.COMPONENT), ];
#validationf<-validationf[grep("Auth", dataf$AFFECTED.COMPONENT), ];

#Remove all reliability values equal to zero
dataf<- dataf[dataf$RELIABILITY!=0,];
staticf<- staticf[staticf$RELIABILITY!=0,];

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



library(e1071) 
svmModelFit <- svm(Utility ~ Criticality*Connectivity*Reliability,featuresf);

#validate and compare models
svmPredicted <- predict(svmModelFit, validationf)

#Compare with Linear regression model
modelFit<- lm(log(Utility) ~ log(Criticality) + log(Connectivity)+ log(Reliability),data=featuresf);
lmPredicted <- predict(modelFit,validationf)

plot(x=validationf$Criticality,y=validationf$Utility);
points(validationf$Criticality, svmPredicted, col = "red", pch=4);
points(validationf$Criticality, exp(lmPredicted), col = "green", pch=3);
title("Regression, actual (circle), non-linear (plus)");
title("Regression, actual (circle), linear (cross), non-linear (plus)");


#Compute error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

#error <- svmModelFit$residuals #training error, not interesting
error<- validationf$Utility - lmPredicted# same as data$Y - predictedY
predictionRMSE <- rmse(error)  
predictionRMSE
# 30.43388 (all events, features connectivity criticality)
# 18.31665 (all events, features connectivty, criticality, reliability)



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

prediction<- predict(modelFit, featuresf);
plot(modelFit)




compareTable <- data.frame(validationf$Criticality,validationf$Connectivity,
                           log(validationf$Utility),
                           prediction);
colnames(compareTable) <- c("criticality","connectivity","actual_utility","predicted_utility");

compareTable

plot(prediction)
 