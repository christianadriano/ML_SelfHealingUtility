#Predict Utility of Components

library(caret);

# load data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");
dataf<-loadData(fileName="MLDATA2_data.csv");
validationf<- loadData(fileName = "MLDATA2_STATIC.csv")
#summary(dataf);

#Scramble the dataset before extracting the training set.
dataf <- scrambleData(dataf);

#Remove all Failures that do not cause utility increase
dataf<- dataf[dataf$FAILURE.NAME=="CF3",];
validationf<- validationf[dataf$FAILURE.NAME=="CF3",];

#Select only the rows that have the Authentication component
dataf<-dataf[grep("Auth", dataf$AFFECTED.COMPONENT), ];
validationf<-validationf[grep("Auth", dataf$AFFECTED.COMPONENT), ];


# consider only the feature columns
features_df<-data.frame(dataf$CRITICALITY,
                        dataf$RELIABILITY,
                        dataf$CONNECTIVITY,
                        dataf$UTILITY.INCREASE);

validationf<-data.frame(validationf$CRITICALITY,
                        validationf$RELIABILITY,
                        validationf$CONNECTIVITY,
                        validationf$UTILITY.INCREASE);


names<-c("Criticality","Reliability","Connectivity","Utility");
colnames(features_df) <- names;
colnames(validationf) <- names;


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
# myFolds <- createFolds(features_df , k = 10); 

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

trControl <- trainControl(method="cv", number=10);

modelFit<- train(Utility ~ .,features_df, method="lm", trControl=trControl);

modelFit
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


prediction<- predict(modelFit, validationf);
#plot(modelFit)

compareTable <- data.frame(validationf$Criticality,validationf$Reliability,validationf$Connectivity,
                           validationf$Utility,
                           prediction);
colnames(compareTable) <- c("criticality","reliability","connectivity","actual_utility","predicted_utility");

compareTable
                           