## Control code that can run different machine learning methods

# install.packages("xgboost")
# install.packages("devtools")
# install_git("git://github.com/jpmml/r2pmml.git")

library(devtools)
library(xgboost)
library(r2pmml) #https://github.com/jpmml/r2pmml

# Initialization section ------------------------------------------------------

#Load utility functions
source("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//models//loadData.R");
source("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//models//xboostRegression.R");

#Data structure to keep results

#Folder with training data
folder <- "C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//data//";
datafolder_1k3k9k <- "//DataPoints_1K-3K-9K//";
#datafolder_10K <- "//DataPoints_10K-150K//";

folder <- paste0(folder,datafolder_1k3k9k);

# CONTROL CODE   ---------------------------------------------------------------

model.name.list <- c("Linear","Discontinuous","Saturating","Combined");
model.name <- model.name.list[4];

method.name <- "XGB";

dataset.name.list <- generateDataSetNames(model.name, c("1K","3K","9K"),0);

results.df <- data.frame(matrix(data=NA,nrow=1000,ncol=7));
colnames(results.df) <- c("Item","Utility_Type","RMSE","R_Squared", "MADP","Elapsed_Time","Number_of_Trees");
  #                        "User_Time","Sys_Time","Learning_Rate","Max_Depth","Train_Split","Min_Data_In_Leaf",
  #                        "Bagging_Fraction");

#results_line <- 0;

for(model.name in model.name.list){
  dataset.name.list <- generateDataSetNames(model.name, c("1K","3K","9K"),0); 
  
 for(i in c(1:length(dataset.name.list))){
    #i <- 1;
    results_line <- results_line+1;
    
    fileName <- paste0(folder,dataset.name.list[i],".csv");
    data.df <- loadData(fileName);
    
    features.df <- prepareFeatures(data.df,"Combined");
    
    #Extract training and validation sets 
    totalData.size <- dim(features.df)[1];
    training.size <- trunc(totalData.size * 0.7);
    
    training.df <- as.data.frame(features.df[1:training.size-1,]);
    validation.df <- as.data.frame(features.df[training.size:totalData.size,]);
    
    #Train model
    numberOfTrees <- 1000;
    kfolds <- 10;
    outcome.list <- train_XGBoost(training.df,numberOfTrees,kfolds);
    dataset.name.list[i];
    #Validate model
    results.df <- validate_XGBoost(outcome.list,validation.df,dataset.name.list[i],results_line,results.df);
  }
}

#print(results.df); #show on the console

message <- resultsToFile(results.df,model.name,method.name,"XGB_70-30_500_trees.csv"); #save to a .csv file
print(message);

pmmlFileName <- paste0(".//pmml///",dataset.name.list[i],"-",method.name,".pmml");
generatePMML(outcome.list[[1]],training.df,pmmlFileName,numberOfTrees);#datasetName[length(datasetName)]);


#-----------------------------------------------------------------------------

#for(max.depth in c(12,14,16)){

#for(learning.rate in c(1,10,100)){
#   learning.rate <- 0.1; #learning.rate/1000;

#for(numberOfTrees in c(5000,10000,15000)){
#  numberOfTrees <- 5000;
# for(bagging.fraction in c(7,8,9)){
#bagging.fraction <- bagging.fraction / 10;

#      for(min.data.in.leaf in c(5,10,15,20)){
