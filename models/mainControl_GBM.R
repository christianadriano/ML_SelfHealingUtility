## Control code that can run different machine learning methods

# install.packages("gbm")
# install.packages("devtools")
# install_git("git://github.com/jpmml/r2pmml.git")

library(devtools)
library(gbm)
library(r2pmml) #https://github.com/jpmml/r2pmml

# Initialization section ------------------------------------------------------

#Load utility functions
source("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//models//loadData.R");
source("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//models//gbmRegression.R");

#Data structure to keep results

#Folder with training data
folder <- "C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//data//DataPoints_1K-3K-9K//";
#folder <- "//DataPoints_1K-3K-9K//";

# CONTROL CODE   ------------------------------------------------------------

model.name.list <- c("Linear","Discontinuous","Saturating","Combined");
model.name <- model.name.list[4];

method.name <- "GBM";

dataset.name.list <- generateDataSetNames(model.name, c("9k"),0);

results.df <- data.frame(matrix(data=NA,nrow=1000,ncol=14));
colnames(results.df) <- c("Item","Utility_Type","RMSE","R_Squared", "MADP","User_Time","Sys_Time","Elapsed_Time",
                          "Number_of_Trees","Learning_Rate","Max_Depth","Train_Split","Min_Data_In_Leaf",
                          "Bagging_Fraction");

results_line <- 0;

for(model.name in model.name.list){
  dataset.name.list <- generateDataSetNames(model.name, c("9K"),0);
  
  for(i in c(1:length(dataset.name.list))){
    i <- 1;
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
    numberOfTrees <- 10000;
    kfolds <- 10;
    learning.rate <- 0.01;
    min.nodes.in.leaf <- 5;
    #Train model
    outcome.list <- train_GBM(training.df,numberOfTrees,kfolds,
                              learning.rate,min.nodes.in.leaf);
    
    #Validate model
    results.df <- validate_GBM(outcome.list,validation.df,dataset.name.list[i],results_line,results.df,
                               learning.rate,min.nodes.in.leaf);
    
    message <- resultsToFile(results.df,model.name,method.name,"_70-30_NOFeatureSelection_9K15K.csv"); #save to a .csv file
    print(message);
    
    pmmlFileName <- paste0(".//pmml///",dataset.name.list[i],"-15KTrees",method.name,".pmml");
    generatePMML(outcome.list[[1]],training.df,pmmlFileName,numberOfTrees);
    
  }
  
}



#-----------------------------------------------------------------------------

#for(max.depth in c(12,14,16)){

#for(learning.rate in c(1,10,100)){
#   learning.rate <- 0.1; #learning.rate/1000;

#for(numberOfTrees in c(5000,10000,15000)){
#  numberOfTrees <- 5000;
# for(bagging.fraction in c(7,8,9)){
#bagging.fraction <- bagging.fraction / 10;

#      for(min.data.in.leaf in c(5,10,15,20)){


# outcome.list <- train_LightGBM(train_df=trainingTest.list[[1]],test_df=trainingTest.list[[2]],
#                                 numberOfTrees,kfolds,max.detph,learning.rate,min.data.in.leaf,bagging.fraction);

#Validate model
#results.df <- validate_LightGBM(outcome.list,validation.df,dataset.name.list[i],results_line,results.df,
#                                numberOfTrees,learning.rate,max.depth,train.split,min.data.in.leaf,bagging.fraction);

