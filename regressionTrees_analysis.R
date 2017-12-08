#Regression Trees to predict the utility drop based on the
#following features: criticality, connectivity, reliability, 
#fan-in, fan-out, average time to deploy (ATD)

#Imports
library(xgboost)


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


 