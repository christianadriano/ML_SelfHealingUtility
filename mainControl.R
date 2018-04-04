## Control code that can run different machine learning methods

install.packages("xgboost")
install.packages("gbm")
install.packages("devtools")
install_git("git://github.com/jpmml/r2pmml.git")

library(devtools)
library(xgboost)
library(gbm)
library(r2pmml) #https://github.com/jpmml/r2pmml

# Initialization section ------------------------------------------------------

#Load utility functions
source("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//loadData.R");
source("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//models//xboostRegression.R");
source("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//models//gbmRegression.R");


#Data structure to keep results

#Folder with training data
folder <- "C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//data//DataPoints_1K-3K-9K//";
#folder <- "//DataPoints_1K-3K-9K//";

# CONTROL CODE   ------------------------------------------------------------

model.name <- c("Linear","Discontinuous","Saturating","ALL")[4];

method.name <- c("GBM","XGBoost","LigthGBM")[1];

dataset.name.list <- generateDataSetNames(model.name, c("1K","3K","9K"),0);

results.df <- data.frame(matrix(data=NA,nrow=3,ncol=9));
colnames(results.df) <- c("Item","Utility_Type","RMSE","R_Squared", "MAPD","User_Time","Sys_Time","Elapsed_Time","Number_of_Trees");

for(i in c(1:length(dataset.name.list))){
  fileName <- paste0(folder,dataset.name.list[i],".csv");
  data.df <- loadData(fileName);
  #data_all <- read.csv(fileName,header = TRUE,sep=",");
  
  features.df <- prepareFeatures(data.df,"ALL");
  
  #Extract training ad validation sets 
  totalData.size <- dim(features.df)[1];
  training.size <- trunc(totalData.size * 0.7);
  startTestIndex <- totalData.size - training.size;
  training.df <- as.data.frame(features.df[1:training.size,]);
  validation.df <-as.data.frame(features.df[startTestIndex:totalData.size,]);
  
  #Train model
  outcome.list <- trainGBM(training.df,numberOfTrees=15000,kfolds=10);
  
  #Validate model
  results.df <- validateGBM(outcome.list,validation.df,dataset.name.list,i,results.df);
}


#print(results.df); #show on the console

message <- resultsToFile(results.df,model.name,method.name,"_70-30_NOFeatureSelection.csv"); #save to a .csv file
print(message);

pmmlFileName <- paste0(".//pmml///",dataset.name.list[i],"-",method.name,".pmml");
generatePMML(outcome.list[[1]],training.df,pmmlFileName,numberOfTrees);#datasetName[length(datasetName)]);





#-----------------------------------------------------------------------------