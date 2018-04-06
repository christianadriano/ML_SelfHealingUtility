## Control code that can run different machine learning methods

# install.packages("xgboost")
# install.packages("gbm")
# install.packages("devtools")
# install_git("git://github.com/jpmml/r2pmml.git")

#Install LightGBM
# options(devtools.install.args = "--no-multiarch") # if you have 64-bit R only, you can skip this
# install_github("Microsoft/LightGBM", subdir = "R-package")

library(devtools)
library(xgboost)
library(lightgbm, quietly=TRUE)
library(r2pmml) #https://github.com/jpmml/r2pmml

# Initialization section ------------------------------------------------------

#Load utility functions
source("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//loadData.R");
source("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//models//xboostRegression.R");
source("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//models//gbmRegression.R");
source("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//models//lightGbmRegression.R");

#Data structure to keep results

#Folder with training data
folder <- "C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//data//DataPoints_1K-3K-9K//";
#folder <- "//DataPoints_1K-3K-9K//";

# CONTROL CODE   ------------------------------------------------------------

model.name <- c("Linear","Discontinuous","Saturating","ALL")[1];

method.name <- c("GBM","XGBoost","LigthGBM")[3];

dataset.name.list <- generateDataSetNames(model.name, c("1K","3K","9K"),0);

results.df <- data.frame(matrix(data=NA,nrow=3,ncol=9));
colnames(results.df) <- c("Item","Utility_Type","RMSE","R_Squared", "MAPD","User_Time","Sys_Time","Elapsed_Time","Number_of_Trees");

results_line <- 0;

#for(i in c(1:length(dataset.name.list))){
#for(nodes in c(1:10)){
  i <- 1
 # results_line <- results_line+1;
  fileName <- paste0(folder,dataset.name.list[i],".csv");
  data.df <- loadData(fileName);

  features.df <- prepareFeatures(data.df,"ALL");
  
  #Extract training and validation sets 
  totalData.size <- dim(features.df)[1];
  training.size <- trunc(totalData.size * 0.7);
  startTestIndex <- totalData.size - training.size;
  training.df <- as.data.frame(features.df[1:training.size,]);
  validation.df <-as.data.frame(features.df[startTestIndex:totalData.size,]);
  
  #Train model
  numberOfTrees=500;
  kfolds=10;
  outcome.list <- trainLightGBM(training.df,numberOfTrees,kfolds);
  
  #Validate model
  results.df <- validateLightGBM(outcome.list,validation.df,dataset.name.list,i,results.df);
#}


#print(results.df); #show on the console

message <- resultsToFile(results.df,model.name,method.name,"_70-30_NOFeatureSelection.csv"); #save to a .csv file
print(message);

pmmlFileName <- paste0(".//pmml///",dataset.name.list[i],"-",method.name,".pmml");
generatePMML(outcome.list[[1]],training.df,pmmlFileName,numberOfTrees);#datasetName[length(datasetName)]);


#-----------------------------------------------------------------------------